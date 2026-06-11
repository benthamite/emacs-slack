;;; slack-reaction-event.el ---                      -*- lexical-binding: t; -*-

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
(require 'eieio)
(require 'slack-util)
(require 'slack-event)
(require 'slack-message-buffer)

(declare-function slack-conversations-history "slack-conversations")
(declare-function slack-message-create "slack-create-message")
(declare-function slack-activity-feed-refresh-cache-from-event
                  "slack-activity-feed-buffer")

(defclass slack-reaction-event (slack-event slack-message-event-processable) ())
(defclass slack-message-reaction-event (slack-reaction-event) ())
(defclass slack-message-reaction-added-event (slack-message-reaction-event) ())
(defclass slack-message-reaction-removed-event (slack-message-reaction-event) ())

(defun slack-create-reaction-event (payload)
  "Create and return a new reaction event instance from PAYLOAD."
  (let* ((type (plist-get payload :type))
         (item (plist-get payload :item))
         (item-type (plist-get item :type)))
    (cond ((string= "message" item-type)
           (cond ((string= "reaction_added" type)
                  (slack-create-message-reaction-added-event payload))
                 ((string= "reaction_removed" type)
                  (slack-create-message-reaction-removed-event payload)))))))

(defun slack-create-message-reaction-added-event (payload)
  "Create and return a new message reaction added event instance from PAYLOAD."
  (slack-message-reaction-added-event :type (plist-get payload :type)
                                      :payload payload))

(defun slack-create-message-reaction-removed-event (payload)
  "Create and return a new message reaction removed event instance from PAYLOAD."
  (slack-message-reaction-removed-event :type (plist-get payload :type)
                                        :payload payload))

(cl-defmethod slack-event-find-message ((this slack-message-reaction-event) team)
  "Return THIS message referenced by the message reaction event event from TEAM, or nil."
  (let* ((payload (oref this payload))
         (item (plist-get payload :item))
         (channel (plist-get item :channel))
         (room (slack-room-find channel team))
         (ts (plist-get item :ts)))
    (when room
      (or (slack-room-find-message room ts)
          ;; Message not in cache — fetch it so the reaction is not lost
          (slack-reaction-event--fetch-and-cache-message room ts team)))))

(cl-defmethod slack-event-save-message ((this slack-message-reaction-removed-event) message _team)
  "Persist THIS MESSAGE carried by the message reaction removed event event into TEAM."
  (let* ((payload (oref this payload))
         (user-id (plist-get payload :user))
         (reaction (slack-reaction :name (plist-get payload :reaction)
                                   :count 1
                                   :users (list user-id))))
    (slack-if-let* ((old-reaction (slack-reaction-find message reaction)))
        (if (< 1 (oref old-reaction count))
            (slack-reaction-remove-user old-reaction user-id)
          (slack-reaction-delete message reaction)))))

(cl-defmethod slack-event-save-message ((this slack-message-reaction-added-event) message _team)
  "Persist THIS MESSAGE carried by the message reaction added event event into TEAM."
  (let* ((payload (oref this payload))
         (reaction (slack-reaction :name (plist-get payload :reaction)
                                   :count 1
                                   :users (list (plist-get payload :user)))))
    (slack-if-let* ((old-reaction (slack-reaction-find message reaction)))
        (slack-reaction-join old-reaction reaction)
      (slack-reaction-push message reaction))))

(cl-defmethod slack-event-update-buffer ((_this slack-message-reaction-event) message team)
  "Refresh the buffers affected by the MESSAGE reaction event event for TEAM."
  (slack-message-replace-buffer message team))

(cl-defmethod slack-event-update-ui :after ((_this slack-message-reaction-event)
                                            _message team)
  "Refresh Activity Feed caches affected by message reaction events."
  (when (fboundp 'slack-activity-feed-refresh-cache-from-event)
    (slack-activity-feed-refresh-cache-from-event team)))

(defun slack-reaction-event--fetch-and-cache-message (room ts team)
  "Fetch message at TS from ROOM via API and cache it.
Returns the fetched message, or nil on failure or when the fetched
message's ts differs from TS: conversations.history never returns
thread replies, so a reaction on an uncached reply would otherwise be
attached to the nearest older channel message.  Used when a reaction
WebSocket event targets a message not yet in cache."
  (condition-case err
      (-some--> (slack-conversations-history room team
                                              :latest ts
                                              :inclusive "true"
                                              :limit "1"
                                              :sync t)
        (oref it response)
        (request-response-data it)
        (plist-get it :messages)
        (nth 0 it)
        (and (string= ts (plist-get it :ts)) it)
        (slack-message-create it team room)
        (progn
          (slack-room-push-message room it team)
          it))
    (error
     (message "slack-reaction-event: failed to fetch message %s: %S" ts err)
     nil)))

(provide 'slack-reaction-event)
;;; slack-reaction-event.el ends here
