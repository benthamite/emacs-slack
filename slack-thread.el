;;; slack-thread.el ---                              -*- lexical-binding: t; -*-

;; Copyright (C) 2017  南優也

;; Author: 南優也 <yuyaminami@minamiyuuya-no-MacBook.local>
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
(require 'slack-room)
(require 'slack-channel)
(require 'slack-im)
(require 'slack-message)
(require 'slack-request)
(require 'slack-conversations)

(defvar slack-message-thread-status-keymap)
(defconst slack-subscriptions-thread-add-url "https://slack.com/api/subscriptions.thread.add")
(defconst slack-subscriptions-thread-remove-url "https://slack.com/api/subscriptions.thread.remove")
(defconst slack-subscriptions-thread-get-url
  "https://slack.com/api/subscriptions.thread.get")
(defconst slack-thread-mark-url
  "https://slack.com/api/subscriptions.thread.mark")

(defcustom slack-thread-also-send-to-room 'ask
  "Whether a thread message should also be sent to its room.
If nil: don't send to the room.
If `ask': ask the user every time.
Any other non-nil value: send to the room."
  :type '(choice (const :tag "Never send message to the room." nil)
                 (const :tag "Ask the user every time." ask)
                 (const :tag "Always send message to the room." t))
  :group 'slack)

(cl-defmethod slack-thread-replies ((this slack-message) room team &key after-success on-error (cursor nil) (oldest nil))
  "Fetch thread replies for THIS message in ROOM on TEAM.
CURSOR and OLDEST are paging parameters; AFTER-SUCCESS is a callback
invoked with (NEXT-CURSOR HAS-MORE) after messages are stored.
ON-ERROR is invoked on request failure."
  (let* ((ts (slack-thread-ts this))
         (oldest (or oldest ts)))
    (cl-labels ((success (messages next-cursor has-more)
                  (slack-room-set-messages room messages team)
                  (slack-message-set-replies room ts messages)
                  (when (functionp after-success)
                    (funcall after-success next-cursor has-more))))
      (slack-conversations-replies room ts team
                                   :after-success #'success
                                   :on-error on-error
                                   :cursor cursor
                                   :oldest oldest))))

(cl-defmethod slack-thread-to-string ((m slack-message) team)
  "Return a propertized summary string of the thread rooted at M for TEAM."
  (if (slack-message-thread-parentp m)
      (let* ((count (oref m reply-count))
             (images (slack-thread--reply-user-images m team))
             (reply-label (format "%d %s"
                                  count
                                  (if (<= 2 count) "replies" "reply")))
             (age (slack-thread--format-last-reply-age
                   (and (slot-boundp m 'latest-reply)
                        (oref m latest-reply))))
             (label (propertize
                     (if age
                         (format "%s, Last reply %s" reply-label age)
                       reply-label)
                     'face '(:underline t))))
        (propertize (concat images label)
                    'keymap slack-message-thread-status-keymap))
    ""))

(defun slack-thread--reply-user-images (message team)
  "Return concatenated profile-image string for MESSAGE reply-users on TEAM.
Returns an empty string when profile-image rendering is disabled or no
images are available."
  (if (and slack-render-image-p slack-render-profile-images-p)
      (cl-loop for user-id in (delete-dups (append (oref message reply-users) nil))
               for user = (slack-user--find user-id team)
               for image = (and user (slack-user-image user team 24))
               when image
               concat (concat (propertize "image"
                                          'display image
                                          'face 'slack-profile-image-face)
                              " "))
    ""))

(defun slack-thread--format-last-reply-age (latest-reply)
  "Return a human-readable age string for LATEST-REPLY timestamp.
LATEST-REPLY is a Slack timestamp string or nil.  Returns nil when
LATEST-REPLY is missing or blank."
  (when (and latest-reply (not (slack-string-blankp latest-reply)))
    (let* ((seconds (float-time
                     (time-subtract (current-time)
                                    (slack-ts-to-time latest-reply))))
           (minutes (floor (/ seconds 60)))
           (hours (floor (/ minutes 60)))
           (days (floor (/ hours 24))))
      (cond
       ((< seconds 60) "just now")
       ((< minutes 60) (format "%d minute%s ago" minutes (if (= minutes 1) "" "s")))
       ((< hours 24) (format "%d hour%s ago" hours (if (= hours 1) "" "s")))
       (t (format "%d day%s ago" days (if (= days 1) "" "s")))))))

(cl-defmethod slack-thread-create ((m slack-message) &optional payload)
  "Create and return a `slack-thread' rooted at message M.
Optional PAYLOAD supplies reply/unread metadata from Slack."
  (if payload
      (let ((reply-count (plist-get payload :reply_count))
            (unread-count (plist-get payload :unread_count))
            (last-read (plist-get payload :last_read)))
        (make-instance 'slack-thread
                       :thread_ts (slack-ts m)
                       :root m
                       :reply_count (or reply-count 0)
                       :unread_count (or unread-count 1)
                       :last_read last-read))
    (make-instance 'slack-thread
                   :thread_ts (slack-ts m)
                   :root m)))

(defun slack-subscriptions-thread-add (room ts team &optional after-success)
  "Subscribe TEAM to the thread at TS in ROOM, calling AFTER-SUCCESS on success."
  (cl-labels
      ((success (&key data &allow-other-keys)
                (slack-request-handle-error
                 (data "slack-subscriptions-thread-add")
                 (when (functionp after-success)
                   (funcall after-success)))))
    (slack-request
     (slack-request-create
      slack-subscriptions-thread-add-url
      team
      :type "POST"
      :params (list (cons "thread_ts" ts)
                    (cons "last_read" ts)
                    (cons "channel" (oref room id)))
      :success #'success))))

(defun slack-subscriptions-thread-remove (room ts team &optional after-success)
  "Unsubscribe TEAM from the thread at TS in ROOM, calling AFTER-SUCCESS on success."
  (cl-labels
      ((success (&key data &allow-other-keys)
                (slack-request-handle-error
                 (data "slack-subscriptions-thread-remove")
                 (when (functionp after-success)
                   (funcall after-success)))))
    (slack-request
     (slack-request-create
      slack-subscriptions-thread-remove-url
      team
      :type "POST"
      :params (list (cons "thread_ts" ts)
                    (cons "last_read" ts)
                    (cons "channel" (oref room id)))
      :success #'success))))

(defun slack-subscriptions-thread-get (room ts team &optional after-success handle-error)
  "Fetch thread subscriptions for TS in ROOM on TEAM.
Call AFTER-SUCCESS with the subscriptions list.  HANDLE-ERROR is an
optional fallback error handler."
  (cl-labels
      ((success (&key data &allow-other-keys)
                (slack-request-handle-error
                 (data "slack-subscriptions-thread-get" handle-error)
                 (when (functionp after-success)
                   (let ((subscriptions (plist-get data :subscriptions)))
                     (funcall after-success subscriptions))))))
    (slack-request
     (slack-request-create
      slack-subscriptions-thread-get-url
      team
      :type "POST"
      :params (list (cons "thread_ts" ts)
                    (cons "channel" (oref room id)))
      :success #'success))))

(cl-defmethod slack-thread-mark ((this slack-message) room ts team)
  "Mark the thread rooted at THIS in ROOM as read up to TS on TEAM."
  (let* ((channel (oref room id))
         (thread-ts (or (oref this thread-ts)
                        ;; initial thread reply
                        (slack-ts this)))
         (params (list (cons "channel" channel)
                       (cons "thread_ts" thread-ts)
                       (cons "ts" ts))))
    (slack-subscriptions-thread-get
     room
     thread-ts
     team
     #'(lambda (subscriptions)
         (if (cl-find-if #'(lambda (subscription)
                             (or (string= subscription ts)
                                 (string< subscription ts)))
                         subscriptions)
             (cl-labels
                 ((success (&key data &allow-other-keys)
                           (slack-request-handle-error
                            (data "slack-thread-mark"))))
               (slack-request
                (slack-request-create
                 slack-thread-mark-url
                 team
                 :type "POST"
                 :params params
                 :success #'success))))))))

(provide 'slack-thread)
;;; slack-thread.el ends here
