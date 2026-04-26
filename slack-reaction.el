;;; slack-reaction.el ---  deal with reactions       -*- lexical-binding: t; -*-

;; Copyright (C) 2015  yuya.minami

;; Author: yuya.minami <yuya.minami@yuyaminami-no-MacBook-Pro.local>
;; Keywords:

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <http://www.gnu.org/licenses/>.

;;; Commentary:

;;

;;; Code:

(require 'eieio)
(require 'slack-user)

(defclass slack-reaction ()
  ((name :initarg :name :type string)
   (count :initarg :count :type integer)
   (users :initarg :users :initform ())
   (user-loading :type boolean :initform nil)))

(cl-defmethod slack-reaction-join ((r slack-reaction) other)
  "Merge OTHER into R, deduplicating users and deriving count.
Idempotent: joining the same user twice is a no-op."
  (when (string= (oref r name) (oref other name))
    (let ((new-users (cl-remove-duplicates
                      (append (oref other users) (oref r users))
                      :test #'string=)))
      (oset r users new-users)
      (oset r count (length new-users)))
    r))

(cl-defmethod slack-equalp ((r slack-reaction) other)
  "Return non-nil when the reaction equals the other argument."
  (slack-reaction-equalp r other))

(cl-defmethod slack-reaction-equalp ((r slack-reaction) other)
  "Return non-nil when reactions R and OTHER share the same emoji name."
  (string= (oref r name) (oref other name)))

(cl-defmethod slack-reaction-fetch-users ((this slack-reaction) team cb)
  "Resolve users who reacted with THIS in TEAM and pass the list to CB.
Triggers a fetch when some user records are missing locally."
  (cl-labels
      ((collect-users (user-ids)
                      (cl-remove-if #'null
                                    (mapcar #'(lambda (id) (slack-user--find id team))
                                            user-ids))))
    (let* ((users (collect-users (oref this users)))
           (user-loaded-p (<= (length (oref this users))
                              (length users))))
      (if user-loaded-p (funcall cb users)
        (progn
          (unless (oref this user-loading)
            (oset this user-loading t)
            (slack-users-info-request (slack-team-missing-user-ids  team (oref this users))
                                      team
                                      :after-success
                                      #'(lambda ()
                                          (oset this user-loading nil)
                                          (funcall cb (collect-users (oref this users)))))))))))

(cl-defmethod slack-reaction-user-reacted-p ((this slack-reaction) user-id)
  "Return non-nil when USER-ID reacted with reaction THIS."
  (member user-id (oref this users)))

(declare-function slack-emoji-resolve "slack-emoji")

(cl-defmethod slack-reaction-help-text ((r slack-reaction) team cb)
  "Show who reacted with R in TEAM, passing the result to CB."
  (slack-reaction-fetch-users r team #'(lambda (users)
                                         (let ((user-names (mapcar #'(lambda (user)
                                                                       (slack-user--name user team))
                                                                   users)))
                                           (funcall cb
                                                    (format "%s reacted with %s"
                                                            (mapconcat #'identity user-names ", ")
                                                            (slack-emoji-resolve (oref r name))))))))

(defun slack-reaction--find (reactions reaction)
  "Return the element of REACTIONS equal to REACTION, or nil."
  (cl-find-if #'(lambda (e) (slack-reaction-equalp e reaction))
              reactions))

(cl-defmethod slack-reaction-delete ((this slack-reaction) reactions)
  "Remove the named reaction from the reaction.
THIS is the slack-reaction instance.
REACTIONS is the reactions argument."
  (cl-delete-if #'(lambda (e) (slack-reaction-equalp e this))
                reactions))

(cl-defmethod slack-reaction-remove-user ((this slack-reaction) user-id)
  "Remove USER-ID from the users of reaction THIS and update its count."
  (with-slots (count users) this
    (setq users (cl-remove-if #'(lambda (old-user) (string= old-user user-id))
                              users))
    (setq count (length users))))

(cl-defmethod slack-merge ((old slack-reaction) new)
  "Merge NEW data into the existing reaction in place.
OLD is the old argument."
  (with-slots (count users) old
    (setq count (oref new count))
    (setq users (cl-remove-duplicates (append users (oref new users))
                                      :test #'string=))))

(provide 'slack-reaction)
;;; slack-reaction.el ends here

