;;; slack-im.el --- slack direct message interface    -*- lexical-binding: t; -*-

;; Copyright (C) 2015  南優也

;; Author: 南優也 <yuyaminami@minamiyuunari-no-MacBook-Pro.local>
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
(require 'slack-util)
(require 'slack-room)
(require 'slack-user)
(require 'slack-request)
(require 'slack-conversations)

(declare-function slack-room-display "slack-message-buffer.el")
(defvar slack-buffer-function)
(defvar slack-display-team-name)
(defvar slack-completing-read-function)

(defconst slack-im-update-mark-url "https://slack.com/api/im.mark")

(defclass slack-im (slack-room)
  ((user :initarg :user :initform "")
   (is-user-deleted :initarg :is_user_deleted :initform nil)
   (is-frozen :initarg :is_frozen :initform nil)
   (properties :initarg :properties :initform nil :documnetation "This contains extra property like :is_dormant, useful to calculate if channel is open.")))

(cl-defmethod slack-merge ((this slack-im) other)
  "Merge new data into the existing im in place.
THIS is the slack-im instance.
OTHER is the other argument."
  (cl-call-next-method)
  (oset this user (oref other user))
  (oset this is-frozen (oref other is-frozen))
  (oset this properties (oref other properties))
  (oset this is-user-deleted (oref other is-user-deleted)))

(cl-defmethod slack-room-open-p ((room slack-im))
  "Return non-nil when the im is currently open.
ROOM is the room argument."
  (and (not (oref room is-frozen))
       (not (oref room is-user-deleted))
       (not (plist-get (oref room properties ) :is_dormant))))

(cl-defmethod slack-im-user-presence ((room slack-im) team)
  "Return the presence indicator string for the user of ROOM on TEAM."
  (or (slack-user-presence-to-string (slack-user-find room team)
                                     team)
      " "))

(cl-defmethod slack-im-user-dnd-status ((room slack-im) team)
  "Return the Do Not Disturb indicator string for the user of ROOM on TEAM."
  (or (slack-user-dnd-status-to-string (slack-user-find room
                                                        team)
                                       team)
      " "))

(cl-defmethod slack-room-name ((room slack-im) team)
  "Return the human-readable name of the im.
ROOM is the room argument.
TEAM is the team argument."
  (with-slots (user) room
    (format "DM: %s" (slack-user-name user team))))

(cl-defmethod slack-room-display-name ((room slack-im) team)
  "To Display emoji in minibuffer configure `emojify-inhibit-in-buffer-functions'.
ROOM is the room argument.
TEAM is the team argument."
  (let* ((status (slack-user-status (oref room user) team))
         (room-name (or (and status
                             (format "%s %s"
                                     (slack-room-name room team)
                                     status))
                        (slack-room-name room team))))
    (if slack-display-team-name
        (format "%s - %s"
                (slack-team-name team)
                room-name)
      room-name)))

(defun slack-im-user-name (im team)
  "Return the display name of the user on the other side of IM on TEAM."
  (with-slots (user) im
    (slack-user-name user team)))

(defun slack-im-names (team)
  "Return an alist mapping display names of open ims of TEAM to their rooms."
  (cl-labels
      ((filter (ims)
         (cl-remove-if #'(lambda (im) (not (slack-room-open-p im)))
                       ims)))
    (slack-room-names (slack-team-ims team)
                      team
                      #'filter)))

(defun slack-im-open ()
  "Open IM message channel with another user.
Use `slack-group-mpim-open' for a group of users."
  (interactive)
  (let* ((team (slack-team-select))
         (user (slack-select-from-list
                   ((slack-user-name-alist
                     team
                     :filter #'(lambda (users)
                                 (cl-remove-if #'slack-user-hidden-p users)))
                    "Select User: "))))
    (slack-conversations-open team
                              :user-ids (list (plist-get user :id))
                              :on-success (lambda (data)
                                            (if-let* ((room-id (plist-get (plist-get data :channel) :id))
                                                      (room (slack-room-find room-id team)))
                                                (slack-room-display room team)
                                              ;; if the room is not in our team cache, we cache the room and then open it
                                              (slack-conversations-info room-id team
                                                                        (lambda ()
                                                                          (let ((room (slack-room-find room-id team)))
                                                                            (slack-room-display room team)))))))))

(cl-defmethod slack-room-label-prefix ((room slack-im) team)
  "Return a string combining DND and presence indicators for ROOM on TEAM."
  (format "%s%s"
          (slack-im-user-dnd-status room team)
          (slack-im-user-presence room team)))

(cl-defmethod slack-room-get-members ((room slack-im))
  "Return a singleton list with the other user's id of the im ROOM."
  (list (oref room user)))

(defun slack-im-find-by-user-id (user-id team)
  "Return the im in TEAM corresponding to USER-ID, or nil if none."
  (cl-find-if #'(lambda (im) (string= user-id (oref im user)))
              (slack-team-ims team)))

(cl-defmethod slack-room--has-unread-p ((this slack-im) counts)
  "Return non-nil when the im has unread messages.
THIS is the slack-im instance.
COUNTS is the counts argument."
  (slack-counts-im-unread-p counts this))

(cl-defmethod slack-room-mention-count ((this slack-im) team)
  "Return the unread mention count for the im.
THIS is the slack-im instance.
TEAM is the team argument."
  (with-slots (counts) team
    (if counts
        (slack-counts-im-mention-count counts this)
      0)))

(cl-defmethod slack-room-set-mention-count ((this slack-im) count team)
  "Set the unread mention COUNT for the im.
THIS is the slack-im instance."
  (slack-if-let* ((counts (oref team counts)))
      (slack-counts-im-set-mention-count counts this count)))

(cl-defmethod slack-room-set-has-unreads ((this slack-im) value team)
  "Set the has-unreads flag for the im.
THIS is the slack-im instance.
VALUE is the value argument."
  (slack-if-let* ((counts (oref team counts)))
      (slack-counts-im-set-has-unreads counts this value)))

(cl-defmethod slack-room--update-latest ((this slack-im) counts ts)
  "Update the latest-message timestamp cached on the im.
THIS is the slack-im instance.
COUNTS is the counts argument."
  (slack-counts-im-update-latest counts this ts))

(cl-defmethod slack-room--latest ((this slack-im) counts)
  "Return the timestamp of the latest message in the im.
THIS is the slack-im instance.
COUNTS is the counts argument."
  (slack-counts-im-latest counts this))

(provide 'slack-im)
;;; slack-im.el ends here
