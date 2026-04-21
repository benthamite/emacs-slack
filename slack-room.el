;;; slack-room.el --- slack generic room interface    -*- lexical-binding: t; -*-

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
(require 'lui)
(require 'slack-util)
(require 'slack-request)
(require 'slack-user)
(require 'slack-counts)

(defface slack-room-unread-face
  '((t (:weight bold)))
  ;; '((t (:box (:line-width 1 :style released-button))))
  "Face used to mark a room as unread when selecting channels."
  :group 'slack)

(defvar slack-buffer-function)
(defvar slack-completing-read-function)
(defvar slack-display-team-name)
(defvar slack-current-buffer)
(defvar slack-buffer-create-on-notify)

(defclass slack-room ()
  ((id :initarg :id)
   (created :initarg :created :initform nil)
   (unread-count :initarg :unread_count :initform 0 :type integer)
   (unread-count-display :initarg :unread_count_display :initform 0 :type integer)
   (message-ids :initform '() :type list)
   (messages :initform (make-hash-table :test 'equal :size 300)) ;; pre-sized for typical active-channel history
   (last-read :initarg :last_read :type string :initform "0") ;; "0" = Slack API sentinel for "no messages read"
   (topic :initarg :topic :initform nil)))

(cl-defgeneric slack-room-name (room team)
  "Return the human-readable name of ROOM for TEAM.")

(cl-defmethod slack-equalp ((this slack-room) other)
  "Return non-nil when the room equals the other argument."
  (string= (oref this id)
           (oref other id)))

(cl-defmethod slack-merge ((this slack-room) other)
  "except MESSAGES"
  (oset this id (oref other id))
  (oset this created (oref other created))
  (oset this unread-count (oref other unread-count))
  (oset this unread-count-display (oref other unread-count-display))
  (unless (string= "0" (oref other last-read))
    (oset this last-read (oref other last-read))))

(defun slack-room-create (payload class)
  "Create a room instance of CLASS from PAYLOAD."
  (let* ((attributes (slack-collect-slots class payload)))
    (apply #'make-instance class attributes)))

(cl-defmethod slack-room-subscribedp ((_room slack-room) _team)
  "Return non-nil when the current user is subscribed to the room."
  nil)

(cl-defmethod slack-room-muted-p ((_this slack-room) _team)
  "Return non-nil when the room is muted for the current user."
  nil)

(cl-defmethod slack-room-hidden-p ((room slack-room))
  "Return non-nil when the room is hidden from the user."
  (slack-room-hiddenp room))

(defun slack-room-hiddenp (room)
  "Return non-nil when ROOM should be hidden from the channel list."
  (or (not (slack-room-member-p room))
      (slack-room-archived-p room)
      (not (slack-room-open-p room))))

(defun slack-room-names (rooms team &optional filter collecter)
  "Return a list of labeled ROOMS for TEAM sorted by latest activity.
Optional FILTER restricts the rooms; optional COLLECTER builds each entry."
  (cl-labels
      ((latest-ts (room)
                  (slack-room-latest room team))
       (sort-rooms (rooms)
                   (nreverse (cl-sort (append rooms nil)
                                      #'string< :key #'latest-ts))))
    (cl-loop for room in (sort-rooms (if filter
                                         (funcall filter rooms)
                                       rooms))
             as label = (slack-room-label room team)
             collect (if (functionp collecter)
                         (funcall collecter label room)
                       (cons label room)))))

(defun slack-room-select (rooms team)
  "Prompt for and return one of ROOMS from TEAM, excluding hidden rooms."
  (let* ((alist (slack-room-names
                 rooms team #'(lambda (rs) (cl-remove-if #'slack-room-hidden-p rs)))))
    (slack-select-from-list (alist "Select Channel: "))))

(defun slack-room-find-message (room ts)
  "Return the message in ROOM with timestamp TS, or nil if not found."
  (with-slots (messages) room
    (gethash ts messages)))

(cl-defmethod slack-room-display-name ((room slack-room) team)
  "Return the display name for ROOM on TEAM, optionally prefixed by team name."
  (let ((room-name (slack-room-name room team)))
    (if slack-display-team-name
        (format "%s - %s"
                (slack-team-name team)
                room-name)
      room-name)))

(cl-defmethod slack-room-label-prefix ((_room slack-room) _team)
  "Return the label prefix used in channel pickers for ROOM in TEAM."
  "  ")

(cl-defmethod slack-room-mention-count-display ((room slack-room) team)
  "Return a parenthesized mention-count string for ROOM in TEAM, or empty."
  (let ((count (slack-room-mention-count room team)))
    (if (< 0 count) (format "(%s)" count) "")))

(cl-defmethod slack-room-mention-count ((this slack-room) team)
  "Return the unread mention count for the room."
  (with-slots (counts) team
    (if counts
        (slack-counts-channel-mention-count counts this)
      0)))

(cl-defmethod slack-room-set-mention-count ((this slack-room) count team)
  "Set the unread mention count for the room."
  (slack-if-let* ((counts (oref team counts)))
      (slack-counts-channel-set-mention-count counts
                                              this
                                              count)))

(cl-defmethod slack-room-set-has-unreads ((this slack-room) value team)
  "Set the has-unreads flag for the room."
  (slack-if-let* ((counts (oref team counts)))
      (slack-counts-channel-set-has-unreads counts this value)))

(cl-defmethod slack-room-label ((room slack-room) team)
  "Return a display label for ROOM in TEAM, propertized if unread."
  (let ((str (format "%s %s%s"
                     (slack-room-label-prefix room team)
                     (slack-room-display-name room team)
                     (slack-room-mention-count-display room team))))
    (if (slack-room-has-unread-p room team)
        (propertize str 'face 'slack-room-unread-face)
      str)))

(cl-defmethod slack-room-name ((_room (eql nil)) _team)
  "Return nil when ROOM is nil."
  nil)

(cl-defmethod slack-room-name ((room slack-room) _team)
  "Return the human-readable name of the room."
  (oref room name))

(defun slack-room-sort-messages (messages)
  "Return MESSAGES sorted chronologically by timestamp."
  (cl-sort messages #'string< :key #'slack-ts))

(cl-defmethod slack-room-sorted-messages ((room slack-room) &optional message-ids)
  "Return the messages in ROOM sorted by timestamp.
If MESSAGE-IDS is non-nil, use it instead of the room's full id list."
  (with-slots (messages) room
    (let ((ids (or message-ids (oref room message-ids)))
          (ret))
      (cl-loop for id in (reverse ids)
               do (slack-if-let* ((message (gethash id messages)))
                      (push message ret)))
      ret)))

(cl-defmethod slack-room-latest ((this slack-room) team)
  "Return the latest-message timestamp for room THIS in TEAM, or \"0\"."
  (with-slots (counts) team
    (or (when counts
          (slack-room--latest this counts))
        "0")))

(cl-defmethod slack-room--latest ((this slack-room) counts)
  "Return the timestamp of the latest message in the room."
  (slack-counts-channel-latest counts this))

(cl-defmethod slack-room--update-latest ((this slack-room) counts ts)
  "Update the latest-message timestamp cached on the room."
  (slack-counts-channel-update-latest counts this ts))

(cl-defmethod slack-room-delete-message ((this slack-room) ts)
  "Delete the message with timestamp TS from room THIS."
  (remhash ts (oref this messages))
  (oset this
        message-ids
        (cl-remove-if #'(lambda (e) (string= ts e))
                      (oref this message-ids))))

(cl-defmethod slack-room-push-message ((this slack-room) message team)
  "Add MESSAGE to room THIS in TEAM and update the latest-ts cache."
  (let ((ts (slack-ts message)))
    (puthash ts message (oref this messages))
    (cl-pushnew ts (oref this message-ids)
                :test #'string=)
    ;; Slack timestamps (e.g. "1680000000.000100") sort correctly
    ;; as strings, keeping message-ids in chronological order.
    (oset this message-ids
          (cl-sort (oref this message-ids) #'string<))

    (slack-if-let* ((counts (oref team counts)))
        (slack-room--update-latest this counts ts))))

(cl-defmethod slack-room-clear-messages ((room slack-room))
  "Remove all messages cached on ROOM."
  (oset room messages (make-hash-table :test 'equal :size 300))
  (oset room message-ids '()))


(cl-defmethod slack-room-trim-messages ((room slack-room) &optional (n 100))
  "Keep only the last N messages in ROOM.
Defaults to 100. Used to reduce memory after closing buffers."
  (with-slots (messages message-ids) room
    (let* ((len (length message-ids))
           (keep-ids (if (> len n)
                         (last message-ids n)
                       message-ids))
           (new-ht (make-hash-table :test 'equal :size (max n 10))))
      (dolist (ts keep-ids)
        (slack-if-let* ((m (gethash ts messages)))
            (puthash ts m new-ht)))
      (oset room messages new-ht)
      (oset room message-ids (cl-sort keep-ids #'string<)))))

(cl-defmethod slack-room-set-messages ((room slack-room) messages team)
  "Store MESSAGES on ROOM for TEAM and refresh the latest-ts cache."
  (cl-loop for m in messages
           do (let ((ts (slack-ts m)))
                (puthash ts m (oref room messages))
                (cl-pushnew ts (oref room message-ids)
                            :test #'string=)))
  (oset room
        message-ids
        (cl-sort (oref room message-ids) #'string<))

  (slack-if-let* ((counts (oref team counts))
                  (latest (car (last (oref room message-ids)))))
      (slack-room--update-latest room counts latest)))

(cl-defmethod slack-room-update-mark ((room slack-room) team ts)
  "Update the read mark for ROOM in TEAM to timestamp TS."
  (slack-conversations-mark room team ts))

(cl-defmethod slack-room-member-p ((_room slack-room))
  "Return non-nil when the current user is a member of the room." t)

(cl-defmethod slack-room-archived-p ((_room slack-room))
  "Return non-nil when the room has been archived." nil)

(cl-defmethod slack-room-open-p ((_room slack-room))
  "Return non-nil when the room is currently open." t)

(cl-defmethod slack-room-equal-p ((room slack-room) other)
  "Return non-nil when ROOM and OTHER have the same identifier."
  (string= (oref room id) (oref other id)))

(cl-defmethod slack-room-inc-unread-count ((room slack-room))
  "Increment the displayed unread-message count for ROOM by one."
  (cl-incf (oref room unread-count-display)))

(cl-defmethod slack-user-find ((room slack-room) team)
  "Return the user referenced by the room in TEAM."
  (slack-user--find (oref room user) team))

(cl-defmethod slack-room-member-p ((_this slack-room))
  "Return non-nil when the current user is a member of the room."
  t)

(cl-defmethod slack-room-find ((_id null) _team)
  "Return nil when ID is nil."
  nil)

(cl-defmethod slack-room-find ((id string) team)
  "Return the string matching the given identifier in TEAM."
  (if (and id team)
      (cl-labels ((find-room (room)
                             (string= id (oref room id))))
        (cond
         ((string-prefix-p "Q" id) (cl-find-if #'find-room (oref team search-results)))
         (t
          (or (gethash id (oref team channels))
              (gethash id (oref team groups))
              (gethash id (oref team ims))))))))

(cl-defmethod slack-room-has-unread-p ((this slack-room) team)
  "Return non-nil when room THIS has unread messages in TEAM."
  (with-slots (counts) team
    (when counts
      (slack-room--has-unread-p this counts))))

(cl-defmethod slack-room--has-unread-p ((this slack-room) counts)
  "Return non-nil when the room has unread messages."
  (slack-counts-channel-unread-p counts this))

(cl-defmethod slack-mpim-p ((_this slack-room))
  "Return non-nil when the room is a multi-party IM."
  nil)

(cl-defmethod slack-room-members ((_this slack-room))
  "Return the list of members of the room."
  nil)

(cl-defmethod slack-room-set-members ((_this slack-room) _members)
  "Store the loaded member list on the room.")

(cl-defmethod slack-room-members-loaded-p ((_this slack-room))
  "Return non-nil when the room's member list is cached."
  nil)

(cl-defmethod slack-room-members-loaded ((_this slack-room))
  "Return the cached member list of the room, or nil.")

(cl-defmethod slack-team-set-room ((this slack-team) room)
  "Store ROOM on team THIS, dispatching by room class."
  (cl-case (eieio-object-class-name room)
    (slack-channel (slack-team-set-channels this (list room)))
    (slack-group (slack-team-set-groups this (list room)))
    (slack-im (slack-team-set-ims this (list room)))))

(defun slack-team--merge-into-table (table items)
  "Merge ITEMS into TABLE, updating existing entries or inserting new ones."
  (cl-loop for item in items
           do (slack-if-let* ((old (gethash (oref item id) table)))
                  (slack-merge old item)
                (puthash (oref item id) item table))))

(cl-defmethod slack-team-set-channels ((this slack-team) channels)
  "Merge CHANNELS into the channels table of team THIS."
  (slack-team--merge-into-table (oref this channels) channels))

(cl-defmethod slack-team-set-groups ((this slack-team) groups)
  "Merge GROUPS into the groups table of team THIS."
  (slack-team--merge-into-table (oref this groups) groups))

(cl-defmethod slack-team-set-ims ((this slack-team) ims)
  "Merge IMS into the ims table of team THIS."
  (slack-team--merge-into-table (oref this ims) ims))

(provide 'slack-room)
;;; slack-room.el ends here
