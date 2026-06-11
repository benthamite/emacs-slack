;;; slack-room-event.el ---                          -*- lexical-binding: t; -*-

;; Copyright (C) 2019

;; Author:  <yuya373@archlinux>
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
;; along with this program.  If not, see <https://www.gnu.org/licenses/>.

;;; Commentary:

;;

;;; Code:
(require 'slack-util)
(require 'slack-event)
(require 'slack-conversations)
(require 'slack-group)
(require 'slack-modeline)
(require 'slack-message-buffer)

(defclass slack-room-event (slack-event slack-room-event-processable) ())

(cl-defmethod slack-event-find-room ((this slack-room-event) team)
  "Return THIS room referenced by the room event event from TEAM, or nil."
  (let* ((payload (oref this payload))
         (id (plist-get payload :channel)))
    (slack-room-find id team)))

(defclass slack-room-async-event () () :abstract t)
(cl-defmethod slack-event-save-room ((_this slack-room-async-event) _room _team _cb)
  "Persist any rooms mentioned by the room async event event into TEAM.")
;; This method is defined on `slack-room-async-event' (not the base
;; `slack-room-event') so that events requiring an async save step
;; dispatch here instead of to the synchronous base implementation.
(cl-defmethod slack-event-update ((this slack-room-async-event) team)
  "Apply THIS room async event event to TEAM's in-memory state."
  (slack-if-let* ((room (slack-event-find-room this team)))
      (slack-event-save-room this room team
                             #'(lambda ()
                                 (slack-event-update-ui this room team)))
    (slack-log (format "slack-event-update: missing room for event %s" this) team :level 'error)))

(defclass slack-channel-created-event (slack-room-event) ())

(defun slack-create-channel-created-event (payload)
  "Create and return a new channel created event instance from PAYLOAD."
  (make-instance 'slack-channel-created-event
                 :payload payload))

(cl-defmethod slack-event-find-room ((this slack-channel-created-event) _team)
  "Return THIS room referenced by the channel created event event from TEAM, or nil."
  (with-slots (payload) this
    (slack-room-create (plist-get payload :channel) 'slack-channel)))

(cl-defmethod slack-event-save-room ((_this slack-channel-created-event) room team _cb)
  "Persist any rooms mentioned by the channel created event event into TEAM.
ROOM is the room argument."
  (slack-team-set-channels team (list room)))

(cl-defmethod slack-event-notify ((_this slack-channel-created-event) room team)
  "Trigger user notifications for the channel created event event on TEAM.
ROOM is the room argument."
  (slack-conversations-info (oref room id)
                            team
                            #'(lambda ()
                                (slack-log (format "Created channel %s"
                                                   (slack-room-display-name room team))
                                           team :level 'info))))

(defclass slack-room-archive-event (slack-room-event) ())
(defclass slack-channel-archive-event (slack-room-archive-event) ())
(defclass slack-group-archive-event (slack-room-archive-event) ())

(defun slack-create-room-archive-event (payload)
  "Create and return a new room archive event instance from PAYLOAD."
  (let ((type (plist-get payload :type)))
    (cond ((string= "channel_archive" type)
           (make-instance 'slack-channel-archive-event
                          :payload payload))
          ((string= "group_archive" type)
           (make-instance 'slack-group-archive-event
                          :payload payload)))))

(cl-defmethod slack-event-save-room ((_this slack-channel-archive-event) room team _cb)
  "Persist any rooms mentioned by the channel archive event event into TEAM.
ROOM is the room argument."
  (oset room is-archived t)
  (slack-team-set-channels team (list room)))

(cl-defmethod slack-event-save-room ((_this slack-group-archive-event) room team _cb)
  "Persist any rooms mentioned by the group archive event event into TEAM.
ROOM is the room argument."
  (oset room is-archived t)
  (slack-team-set-groups team (list room)))

(cl-defmethod slack-event-notify ((_this slack-room-archive-event) room team)
  "Trigger user notifications for the ROOM archive event event on TEAM."
  (slack-log (format "Channel: %s is archived"
                     (slack-room-name room team))
             team :level 'info))

(defclass slack-room-unarchive-event (slack-room-event) ())
(defclass slack-channel-unarchive-event (slack-room-unarchive-event) ())
(defclass slack-group-unarchive-event (slack-room-unarchive-event) ())

(defun slack-create-room-unarchive-event (payload)
  "Create and return a new room unarchive event instance from PAYLOAD."
  (let ((type (plist-get payload :type)))
    (cond ((string= "channel_unarchive" type)
           (make-instance 'slack-channel-unarchive-event
                          :payload payload))
          ((string= "group_unarchive" type)
           (make-instance 'slack-group-unarchive-event
                          :payload payload)))))

(cl-defmethod slack-event-save-room ((_this slack-channel-unarchive-event) room team _cb)
  "Persist any rooms mentioned by the channel unarchive event event into TEAM.
ROOM is the room argument."
  (oset room is-archived nil)
  (slack-team-set-channels team (list room)))

(cl-defmethod slack-event-save-room ((_this slack-group-unarchive-event) room team _cb)
  "Persist any rooms mentioned by the group unarchive event event into TEAM.
ROOM is the room argument."
  (oset room is-archived nil)
  (slack-team-set-groups team (list room)))

(cl-defmethod slack-event-notify ((_this slack-room-unarchive-event) room team)
  "Trigger user notifications for the ROOM unarchive event event on TEAM."
  (slack-log (format "Channel: %s is unarchived"
                     (slack-room-name room team))
             team :level 'info))

(defclass slack-channel-deleted-event (slack-room-event) ())

(defun slack-create-channel-deleted-event (payload)
  "Create and return a new channel deleted event instance from PAYLOAD."
  (make-instance 'slack-channel-deleted-event
                 :payload payload))

(cl-defmethod slack-event-save-room ((_this slack-channel-deleted-event) room team _cb)
  "Persist any rooms mentioned by the channel deleted event event into TEAM.
ROOM is the room argument."
  (remhash (oref room id)
           (oref team channels)))

(cl-defmethod slack-event-notify ((_this slack-channel-deleted-event) room team)
  "Trigger user notifications for the channel deleted event event on TEAM.
ROOM is the room argument."
  (slack-log (format "Channel: %s is deleted"
                     (slack-room-name room team))
             team :level 'info))

(defclass slack-room-rename-event (slack-room-event)
  ((previous-name :initform "" :type string)))
(defclass slack-channel-rename-event (slack-room-rename-event) ())
(defclass slack-group-rename-event (slack-room-rename-event) ())

(defun slack-create-room-rename-event (payload)
  "Create and return a new room rename event instance from PAYLOAD."
  (let ((type (plist-get payload :type)))
    (cond ((string= "channel_rename" type)
           (make-instance 'slack-channel-rename-event
                          :payload payload))
          ((string= "group_rename" type)
           (make-instance 'slack-group-rename-event
                          :payload payload)))))

(cl-defmethod slack-event-find-room ((this slack-room-rename-event) team)
  "Return THIS room referenced by the room rename event event from TEAM, or nil."
  (let* ((payload (oref this payload))
         (channel (plist-get payload :channel))
         (id (plist-get channel :id)))
    (slack-room-find id team)))

(cl-defmethod slack-event-update-name ((this slack-room-rename-event) room _team)
  "Apply the rename payload from event THIS to ROOM, saving the previous name."
  (let* ((previous-name (oref room name-normalized))
         (payload (oref this payload))
         (channel (plist-get payload :channel))
         (name (plist-get channel :name))
         ;; The documented channel_rename payload carries only
         ;; {id, name, created}; a nil normalized name would violate
         ;; the slot type after the name was already updated, leaving
         ;; the room half-renamed.
         (name-normalized (or (plist-get channel :name_normalized) name)))
    (oset this previous-name previous-name)
    (oset room name-normalized name-normalized)
    (oset room name name)))

(cl-defmethod slack-event-save-room ((this slack-channel-rename-event) room team _cb)
  "Persist any rooms mentioned by the channel rename event event into TEAM.
THIS is the slack-channel-rename-event instance.
ROOM is the room argument."
  (slack-event-update-name this room team)
  (slack-team-set-channels team (list room)))

(cl-defmethod slack-event-save-room ((this slack-group-rename-event) room team _cb)
  "Persist any rooms mentioned by the group rename event event into TEAM.
THIS is the slack-group-rename-event instance.
ROOM is the room argument."
  (slack-event-update-name this room team)
  (slack-team-set-groups team (list room)))

(cl-defmethod slack-event-notify ((this slack-room-rename-event) room team)
  "Trigger user notifications for THIS ROOM rename event event on TEAM."
  (with-slots (previous-name) this
    (slack-log (format "Channel renamed from %s to %s"
                       previous-name
                       (oref room name-normalized))
               team :level 'info)))

(defclass slack-room-joined-event (slack-room-event slack-room-async-event) ())
(defclass slack-channel-joined-event (slack-room-joined-event) ())
(defclass slack-group-joined-event (slack-room-joined-event) ())

(defun slack-create-channel-joined-event (payload)
  "Create and return a new channel joined event instance from PAYLOAD."
  (make-instance 'slack-channel-joined-event
                 :payload payload))

(defun slack-create-group-joined-event (payload)
  "Create and return a new group joined event instance from PAYLOAD."
  (make-instance 'slack-group-joined-event
                 :payload payload))
(cl-defmethod slack-event-find-room ((this slack-room-joined-event) team)
  "Return THIS room referenced by the room joined event event from TEAM, or nil."
  (let* ((payload (oref this payload))
         (channel (plist-get payload :channel))
         (id (plist-get channel :id))
         (class (or (and (eq t (plist-get channel :is_private))
                         'slack-group)
                    'slack-channel)))
    (or (slack-room-find id team)
        (slack-room-create channel class))))

(cl-defmethod slack-event-save-room ((_this slack-channel-joined-event) room team cb)
  "Persist any rooms mentioned by the channel joined event event into TEAM.
ROOM is the room argument."
  (slack-team-set-channels team (list room))
  (slack-conversations-info (oref room id) team cb))

(cl-defmethod slack-event-save-room ((_this slack-group-joined-event) room team cb)
  "Persist any rooms mentioned by the group joined event event into TEAM.
ROOM is the room argument."
  (slack-team-set-groups team (list room))
  (slack-conversations-info (oref room id) team cb))

(cl-defmethod slack-event-notify ((_this slack-room-joined-event) _room team)
  "Trigger user notifications for the room joined event event on TEAM."
  (slack-counts-update team))

(cl-defmethod slack-event-notify ((_this slack-channel-joined-event) room team)
  "Trigger user notifications for the channel joined event event on TEAM.
ROOM is the room argument."
  (cl-call-next-method)
  (slack-log (format "Joined channel %s"
                     (slack-room-name room team))
             team :level 'info))

(cl-defmethod slack-event-notify ((_this slack-group-joined-event) room team)
  "Trigger user notifications for the group joined event event on TEAM.
ROOM is the room argument."
  (cl-call-next-method)
  (slack-log (format "Joined group %s"
                     (slack-room-name room team))
             team :level 'info))

(defclass slack-room-marked-event (slack-room-event) ())
(defclass slack-channel-marked-event (slack-room-marked-event) ())
(defclass slack-group-marked-event (slack-room-marked-event) ())
(defclass slack-im-marked-event (slack-room-marked-event) ())

(defun slack-create-room-marked-event (payload)
  "Create and return a new room marked event instance from PAYLOAD."
  (let* ((type (plist-get payload :type))
         (klass (cond
                 ((string= "channel_marked" type) 'slack-channel-marked-event)
                 ((or (string= "group_marked" type)
                      (string= "mpim_marked" type))
                  'slack-group-marked-event)
                 ((string= "im_marked" type) 'slack-im-marked-event))))
    (make-instance klass :payload payload)))

(cl-defmethod slack-event-update-room ((this slack-room-marked-event) room team)
  "Apply the room changes from the room marked event event to TEAM.
THIS is the slack-room-marked-event instance."
  (let* ((payload (oref this payload))
         (ts (plist-get payload :ts))
         (unread-count-display (plist-get payload :unread_count_display))
         (mention-count-display (plist-get payload :mention_count_display)))
    (oset room unread-count-display unread-count-display)
    (oset room last-read ts)
    (slack-room-set-mention-count room mention-count-display team)
    (slack-room-set-has-unreads room (< 0 unread-count-display) team)))

(cl-defmethod slack-event-save-room ((this slack-room-marked-event) room team _cb)
  "Persist any rooms mentioned by the room marked event event into TEAM.
THIS is the slack-room-marked-event instance."
  (slack-event-update-room this room team))

(cl-defmethod slack-event-save-room ((_this slack-channel-marked-event) room team _cb)
  "Persist any rooms mentioned by the channel marked event event into TEAM.
ROOM is the room argument."
  (cl-call-next-method)
  (slack-team-set-channels team (list room)))

(cl-defmethod slack-event-save-room ((_this slack-group-marked-event) room team _cb)
  "Persist any rooms mentioned by the group marked event event into TEAM.
ROOM is the room argument."
  (cl-call-next-method)
  (slack-team-set-groups team (list room)))

(cl-defmethod slack-event-save-room ((_this slack-im-marked-event) room team _cb)
  "Persist any rooms mentioned by the im marked event event into TEAM.
ROOM is the room argument."
  (cl-call-next-method)
  (slack-team-set-ims team (list room)))

(cl-defmethod slack-event-update-buffer ((_this slack-room-marked-event) room team)
  "Refresh the buffers affected by the ROOM marked event event for TEAM."
  (slack-update-modeline)
  (slack-if-let*
      ((buffer (slack-buffer-find 'slack-message-buffer team room)))
      (slack-buffer-update-marker-overlay buffer)))

(defclass slack-im-open-event (slack-room-async-event slack-room-event) ())

(defun slack-create-im-open-event (payload)
  "Create and return a new im open event instance from PAYLOAD."
  (make-instance 'slack-im-open-event
                 :payload payload))

(cl-defmethod slack-event-find-room ((this slack-im-open-event) team)
  "Return THIS room referenced by the im open event event from TEAM, or nil."
  (let* ((payload (oref this payload))
         (channel (plist-get payload :channel)))
    (or (slack-room-find channel team)
        (slack-room-create (list :id channel
                                 :user (plist-get payload :user))
                           'slack-im))))

(cl-defmethod slack-event-save-room ((_this slack-im-open-event) room team cb)
  "Persist any rooms mentioned by the im open event event into TEAM.
ROOM is the room argument."
  (oset room properties (plist-put (oref room properties) :is_dormant nil))
  (slack-team-set-ims team (list room))
  (slack-conversations-info (oref room id) team cb))

(cl-defmethod slack-event-notify ((_this slack-im-open-event) room team)
  "Trigger user notifications for the im open event event on TEAM.
ROOM is the room argument."
  (slack-log (format "Open direct message with %s"
                     (slack-user-name (oref room user)
                                      team))
             team :level 'info))

(defclass slack-room-close-event (slack-room-async-event slack-room-event) ())
(defclass slack-group-close-event (slack-room-close-event) ())
(defclass slack-im-close-event (slack-room-close-event) ())

(defun slack-create-room-close-event (payload)
  "Create and return a new room close event instance from PAYLOAD."
  (let* ((type (plist-get payload :type))
         (klass (cond ((string= "im_close" type)
                       'slack-im-close-event)
                      ((string= "group_close" type)
                       'slack-group-close-event))))
    (make-instance klass :payload payload)))

(cl-defmethod slack-event-save-room ((_this slack-group-close-event) room team _cb)
  "Persist any rooms mentioned by the group close event event into TEAM.
ROOM is the room argument."
  (oset room is-member nil)
  (slack-team-set-groups team (list room)))

(cl-defmethod slack-event-notify ((_this slack-group-close-event) room team)
  "Trigger user notifications for the group close event event on TEAM.
ROOM is the room argument."
  (slack-log (format "%s closed"
                     (slack-room-name room team))
             team :level 'info))

(cl-defmethod slack-event-save-room ((_this slack-im-close-event) room team _cb)
  "Persist any rooms mentioned by the im close event event into TEAM.
ROOM is the room argument."
  (oset room properties (plist-put (oref room properties) :is_dormant t))
  (slack-team-set-ims team (list room)))

(cl-defmethod slack-event-notify ((this slack-im-close-event) _room team)
  "Trigger user notifications for the im close event event on TEAM.
THIS is the slack-im-close-event instance."
  (let* ((payload (oref this payload))
         (user (plist-get payload :user)))
    (slack-log (format "Direct message with %s is closed"
                       (slack-user-name user team))
               team :level 'info)))

(defclass slack-member-joined-room-event (slack-room-event) ())
(defclass slack-member-joined-group-event (slack-member-joined-room-event) ())
(defclass slack-member-joined-channel-event (slack-member-joined-room-event) ())

(defun slack-create-member-joined-room-event (payload)
  "Create and return a new member joined room event instance from PAYLOAD."
  (let* ((channel-type (plist-get payload :channel_type))
         (klass (cond ((string= "G" channel-type)
                       'slack-member-joined-group-event)
                      ((string= "C" channel-type)
                       'slack-member-joined-channel-event))))
    (make-instance klass :payload payload)))

(cl-defmethod slack-event-update-room ((this slack-member-joined-room-event) room _team)
  "Apply THIS ROOM changes from the member joined room event event to TEAM."
  (let* ((payload (oref this payload))
         (user (plist-get payload :user)))
    (cl-pushnew user (oref room members)
                :test #'string=)))

(cl-defmethod slack-event-save-room ((this slack-member-joined-channel-event) room team _cb)
  "Persist any rooms mentioned by the member joined channel event event into TEAM.
THIS is the slack-member-joined-channel-event instance.
ROOM is the room argument."
  (slack-event-update-room this room team)
  (slack-team-set-channels team (list room)))

(cl-defmethod slack-event-save-room ((this slack-member-joined-group-event) room team _cb)
  "Persist any rooms mentioned by the member joined group event event into TEAM.
THIS is the slack-member-joined-group-event instance.
ROOM is the room argument."
  (slack-event-update-room this room team)
  (slack-team-set-groups team (list room)))

(defclass slack-member-left-room-event (slack-room-event) ())
(defclass slack-member-left-channel-event (slack-member-left-room-event) ())
(defclass slack-member-left-group-event (slack-member-left-room-event) ())

(defun slack-create-member-left-room-event (payload)
  "Create and return a new member left room event instance from PAYLOAD."
  (let* ((channel-type (plist-get payload :channel_type))
         (klass (cond ((string= "G" channel-type)
                       'slack-member-left-group-event)
                      ((string= "C" channel-type)
                       'slack-member-left-channel-event))))
    (make-instance klass :payload payload)))

(cl-defmethod slack-event-update-room ((this slack-member-left-room-event) room _team)
  "Apply THIS ROOM changes from the member left room event event to TEAM."
  (let* ((payload (oref this payload))
         (user (plist-get payload :user)))
    (oset room members
          (cl-remove-if #'(lambda (e) (string= e user))
                        (oref room members)))))

(cl-defmethod slack-event-save-room ((this slack-member-left-channel-event) room team _cb)
  "Persist any rooms mentioned by the member left channel event event into TEAM.
THIS is the slack-member-left-channel-event instance.
ROOM is the room argument."
  (slack-event-update-room this room team)
  (slack-team-set-channels team (list room)))

(cl-defmethod slack-event-save-room ((this slack-member-left-group-event) room team _cb)
  "Persist any rooms mentioned by the member left group event event into TEAM.
THIS is the slack-member-left-group-event instance.
ROOM is the room argument."
  (slack-event-update-room this room team)
  (slack-team-set-groups team (list room)))


(provide 'slack-room-event)
;;; slack-room-event.el ends here
