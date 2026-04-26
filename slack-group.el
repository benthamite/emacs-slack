;;; slack-group.el --- slack private group interface  -*- lexical-binding: t; -*-

;; Copyright (C) 2015  Yuya Minami

;; Author: Yuya Minami
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
(require 'slack-room)
(require 'slack-util)
(require 'slack-request)
(require 'slack-conversations)
(require 'slack-defcustoms)

(declare-function slack-room-display "slack-message-buffer.el")

(defvar slack-buffer-function)
(defvar slack-completing-read-function)

(defclass slack-group (slack-room)
  ((name :initarg :name :type string :initform "")
   (name-normalized :initarg :name_normalized :type string :initform "")
   (num-members :initarg :num_members :initform 0)
   (creator :initarg :creator :type (or null string) :initform nil)
   (is-archived :initarg :is_archived :initform nil :type boolean)
   (is-channel :initarg :is_channel :initform nil :type boolean)
   (is-ext-shared :initarg :is_ext_shared :initform nil :type boolean)
   (is-pending-ext-shared :initarg :is_pending_ext_shared :initform nil :type boolean)
   (is-general :initarg :is_general :initform nil :type boolean)
   (is-group :initarg :is_group :initform nil :type boolean)
   (is-im :initarg :is_im :initform nil :type boolean)
   (is-member :initarg :is_member :initform t :type boolean)
   (is-mpim :initarg :is_mpim :initform nil :type boolean)
   (is-org-shared :initarg :is_org_shared :initform nil :type boolean)
   (is-private :initarg :is_private :initform nil :type boolean)
   (is-read-only :initarg :is_read_only :initform nil :type boolean)
   (is-shared :initarg :is_shared :initform nil :type boolean)
   (parent-conversation :initarg :parent_conversation :initform nil)
   (pending-shared :initarg :pending_shared :initform '() :type list)
   (previous-names :initarg :previous_names :initform '() :type list)
   (purpose :initarg :purpose :initform nil)
   (shared-team-ids :initarg :shared_team_ids :initform '() :type list)
   (members :initarg :members :type list :initform '())
   (members-loaded-p :type boolean :initform nil)))

(cl-defmethod slack-merge ((this slack-group) other)
  "Merge new data into the existing group in place.
THIS is the slack-group instance.
OTHER is the other argument."
  (cl-call-next-method)
  (oset this name (oref other name))
  (oset this name-normalized (oref other name-normalized))
  (oset this num-members (oref other num-members))
  (oset this creator (oref other creator))
  (oset this is-archived (oref other is-archived))
  (oset this is-channel (oref other is-channel))
  (oset this is-ext-shared (oref other is-ext-shared))
  (oset this is-pending-ext-shared (oref other is-pending-ext-shared))
  (oset this is-general (oref other is-general))
  (oset this is-group (oref other is-group))
  (oset this is-im (oref other is-im))
  (oset this is-member (oref other is-member))
  (oset this is-mpim (oref other is-mpim))
  (oset this is-org-shared (oref other is-org-shared))
  (oset this is-private (oref other is-private))
  (oset this is-read-only (oref other is-read-only))
  (oset this is-shared (oref other is-shared))
  (oset this parent-conversation (oref other parent-conversation))
  (oset this pending-shared (oref other pending-shared))
  (oset this previous-names (oref other previous-names))
  (oset this purpose (oref other purpose))
  (oset this shared-team-ids (oref other shared-team-ids))
  (oset this topic (oref other topic)))

(defun slack-group-names (team &optional filter)
  "Return an alist of display names and groups for TEAM, optionally filtered by FILTER."
  (slack-room-names (slack-team-groups team) team filter))

(cl-defmethod slack-room-subscribedp ((room slack-group) team)
  "Return non-nil when the current user is subscribed to the group.
ROOM is the room argument.
TEAM is the team argument."
  (with-slots (subscribed-channels) team
    (let ((name (slack-room-name room team)))
      (or (oref room is-mpim) ;; groups conversations we are in need notification
          (and name
               (memq (intern name) (append subscribed-channels slack-extra-subscribed-channels)))))))

(cl-defmethod slack-room-muted-p ((this slack-group) team)
  "Return non-nil when the group is muted for the current user.
THIS is the slack-group instance.
TEAM is the team argument."
  (seq-contains-p
   (plist-get (oref team user-prefs) :muted_channels)
   (oref this id)))

(defun slack-group-list-update (&optional team after-success)
  "Refresh TEAM's list of group from the Slack API.
AFTER-SUCCESS is the after-success argument."
  (interactive)
  (let ((team (or team (slack-team-select))))
    (cl-labels
        ((success (_channels groups _ims)
                  (slack-team-set-groups team groups)
                  (when (functionp after-success)
                    (funcall after-success team))
                  (slack-log "Slack Group List Updated"
                             team :level 'info)))
      (slack-conversations-list team #'success (list "private_channel" "mpim")))))


(defun slack-create-group ()
  "Create and return a new group instance from PAYLOAD."
  (interactive)
  (let ((team (slack-team-select)))
    (slack-conversations-create team "true")))

(cl-defmethod slack-room-archived-p ((room slack-group))
  "Return non-nil when the group has been archived.
ROOM is the room argument."
  (oref room is-archived))

(defun slack-group-members-s (group team)
  "Return a comma-separated string of GROUP's member display names in TEAM."
  (with-slots (members) group
    (mapconcat #'(lambda (user)
                   (slack-user-name user
                                    team))
               members ", ")))

(defun slack-group-mpim-open ()
  "Open group conversation by selecting users."
  (interactive)
  (let* ((team (slack-team-select))
         (users (slack-user-names team)))
    (cl-labels
        ((prompt (loop-count)
           (if (< 0 loop-count)
               "Select another user (or leave empty): "
             "Select user: "))
         (user-ids ()
           (mapcar #'(lambda (user) (plist-get user :id))
                   (slack-select-multiple #'prompt users))))
      (slack-conversations-open team
                                :user-ids (user-ids)
                                :on-success (lambda (data)
                                              (if-let* ((room-id (plist-get (plist-get data :channel) :id))
                                                        (room (slack-room-find room-id team)))
                                                  (slack-room-display room team)
                                                ;; if the room is not in our team cache, we cache the room and then open it
                                                (slack-conversations-info room-id team
                                                                          (lambda ()
                                                                            (let ((room (slack-room-find room-id team)))
                                                                              (slack-room-display room team))))))))))

(cl-defmethod slack-mpim-p ((room slack-group))
  "Return non-nil when the group is a multi-party IM.
ROOM is the room argument."
  (oref room is-mpim))

(cl-defmethod slack-room--has-unread-p ((this slack-group) counts)
  "Return non-nil when the group has unread messages.
THIS is the slack-group instance.
COUNTS is the counts argument."
  (if (slack-mpim-p this)
      (slack-counts-mpim-unread-p counts this)
    (slack-counts-channel-unread-p counts this)))

(cl-defmethod slack-room-mention-count ((this slack-group) team)
  "Return the unread mention count for the group.
THIS is the slack-group instance.
TEAM is the team argument."
  (with-slots (counts) team
    (if counts
        (if (slack-mpim-p this)
            (slack-counts-mpim-mention-count counts this)
          (slack-counts-channel-mention-count counts this))
      0)))

(cl-defmethod slack-room-set-mention-count ((this slack-group) count team)
  "Set the unread mention COUNT for the group.
THIS is the slack-group instance."
  (slack-if-let* ((counts (oref team counts)))
      (if (slack-mpim-p this)
          (slack-counts-mpim-set-mention-count counts this count)
        (slack-counts-channel-set-mention-count counts this count))))

(cl-defmethod slack-room-set-has-unreads ((this slack-group) value team)
  "Set the has-unreads flag for the group.
THIS is the slack-group instance.
VALUE is the value argument."
  (slack-if-let* ((counts (oref team counts)))
      (if (slack-mpim-p this)
          (slack-counts-mpim-set-has-unreads counts this value)
        (slack-counts-channel-set-has-unreads counts this value))))

(cl-defmethod slack-room--update-latest ((this slack-group) counts ts)
  "Update the latest-message timestamp cached on the group.
THIS is the slack-group instance.
COUNTS is the counts argument."
  (if (slack-mpim-p this)
      (slack-counts-mpim-update-latest counts this ts)
    (slack-counts-channel-update-latest counts this ts)))

(cl-defmethod slack-room--latest ((this slack-group) counts)
  "Return the timestamp of the latest message in the group.
THIS is the slack-group instance.
COUNTS is the counts argument."
  (if (slack-mpim-p this)
      (slack-counts-mpim-latest counts this)
    (slack-counts-channel-latest counts this)))

(cl-defmethod slack-room-members ((this slack-group))
  "Return the list of members of the group.
THIS is the slack-group instance."
  (oref this members))

(cl-defmethod slack-room-set-members ((this slack-group) members)
  "Store the loaded member list on the group.
THIS is the slack-group instance.
MEMBERS is the members argument."
  (oset this members
        (cl-remove-duplicates (append (oref this members) members)
                              :test #'string=)))

(cl-defmethod slack-room-members-loaded-p ((this slack-group))
  "Return non-nil when the group's member list is cached.
THIS is the slack-group instance."
  (oref this members-loaded-p))

(cl-defmethod slack-room-members-loaded ((this slack-group))
  "Return the cached member list of the group, or nil.
THIS is the slack-group instance."
  (oset this members-loaded-p t))

(cl-defmethod slack-room-hidden-p ((this slack-group))
  "Return non-nil when the group is hidden from the user.
THIS is the slack-group instance."
  (or (not (slack-room-member-p this))
      (slack-room-archived-p this)))

(cl-defmethod slack-room-member-p ((room slack-group))
  "Return non-nil when the current user is a member of the group.
ROOM is the room argument."
  (oref room is-member))

(provide 'slack-group)
;;; slack-group.el ends here
