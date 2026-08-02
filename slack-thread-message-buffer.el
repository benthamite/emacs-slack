;;; slack-thread-message-buffer.el ---               -*- lexical-binding: t; -*-

;; Copyright (C) 2017

;; Author:  <yuya373@yuya373>
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
(require 'slack-message-sender)
(require 'slack-message-reaction)
(require 'slack-message-edit-buffer)
(require 'slack-message-share-buffer)
(require 'slack-star)

(define-derived-mode slack-thread-message-buffer-mode
  slack-buffer-mode
  "Slack Thread Message"
  "Major mode for a Slack thread reply buffer.

Message-region bindings (active when point is on a reply, not on the
input prompt):
\\{slack-message-keymap}
Buffer-wide bindings:
\\{slack-thread-message-buffer-mode-map}"
  (lui-set-prompt lui-prompt-string)
  (cursor-sensor-mode)
  (add-hook 'post-command-hook #'slack-buffer--maybe-load-more-at-end nil t)
  (setq lui-input-function 'slack-thread-message--send))

(defclass slack-thread-message-buffer (slack-room-buffer)
  ((thread-ts :initarg :thread-ts :type string)
   (has-more :initarg :has-more :type boolean)
   (last-read :initform nil :type (or null string))))

(defun slack-create-thread-message-buffer (room team thread-ts &optional has-more)
  "Create thread message buffer according to ROOM, TEAM, THREAD-TS.
HAS-MORE indicates whether more replies remain on the server."
  (slack-if-let* ((buf (slack-buffer-find 'slack-thread-message-buffer team room thread-ts)))
      buf
    (let ((buf (slack-thread-message-buffer :room-id (oref room id)
                                            :team-id (oref team id)
                                            :has-more has-more
                                            :thread-ts thread-ts)))
      (slack-buffer-cache-team buf team)
      buf)))

(defun slack-thread-page-key (room thread-ts)
  "Return the stable page key for ROOM and THREAD-TS."
  (list 'thread (oref room id) thread-ts))

(defun slack-thread-page-state (room team thread-ts)
  "Return TEAM's durable page state for ROOM and THREAD-TS."
  (slack-team-page-state team (slack-thread-page-key room thread-ts)))

(cl-defmethod slack-buffer-name ((this slack-thread-message-buffer))
  "Return the display buffer name for THIS buffer."
  (slack-if-let* ((team (slack-buffer-team this))
                  (room (slack-buffer-room this))
                  (room-name (slack-room-name room team)))
      (format "*slack-thread: %s - %s"
              room-name
              (oref this thread-ts))))

(cl-defmethod slack-buffer-key ((_class (subclass slack-thread-message-buffer)) room ts)
  "Return the class-level buffer key for the thread message buffer.
ROOM disambiguates: Slack guarantees ts uniqueness per channel only,
so keying on TS alone could return another room's thread buffer."
  (concat (oref room id) ":" ts))

(cl-defmethod slack-buffer-key ((this  slack-thread-message-buffer))
  "Return the lookup key identifying the buffer for THIS buffer."
  (slack-buffer-key 'slack-thread-message-buffer
                    (slack-buffer-room this)
                    (oref this thread-ts)))

(cl-defmethod slack-team-buffer-key ((_class (subclass slack-thread-message-buffer)))
  "Return the team-scoped class-level buffer key for the thread message buffer."
  'slack-thread-message-buffer)

(cl-defmethod slack-buffer-update-last-read ((this slack-thread-message-buffer) message)
  "Record the timestamp of MESSAGE as the last-read position of buffer THIS."
  (when message
    (oset this last-read (slack-ts message))))

(cl-defmethod slack-buffer-init-buffer ((this slack-thread-message-buffer))
  "Initialize and return the display buffer for THIS buffer."
  (let ((buf (cl-call-next-method)))
    (when buf
      (with-current-buffer buf
        (slack-thread-message-buffer-mode)
        (slack-buffer-set-current-buffer this)
        (goto-char lui-input-marker)
        (with-slots (thread-ts) this
          (slack-if-let* ((team (slack-buffer-team this))
                          (room (slack-buffer-room this))
                          (message (slack-room-find-message room thread-ts)))
              (progn
                (slack-buffer-insert this message t)
                (let ((lui-time-stamp-position nil))
                  (lui-insert (slack-buffer-separator) t))
                (slack-if-let* ((messages (slack-message-replies message room)))
                    (progn
                      (dolist (reply messages)
                        (slack-buffer-insert this reply t))
                      (unless (oref this has-more)
                        (let ((latest-message (car (last messages))))
                          (slack-buffer-update-last-read this latest-message)
                          (slack-buffer-update-mark this))))))))))
    buf))

(defun slack-thread-message-buffer-render-page-state (object state)
  "Render OBJECT's cached thread and shared page status from STATE."
  (let* ((room (slack-buffer-room object))
         (thread-ts (oref object thread-ts))
         (parent (slack-room-find-message room thread-ts))
         (replies (and parent (slack-message-replies parent room)))
         (latest-reply (car (last replies)))
         (page-replies
          (and (slack-page-state-loaded-p state)
               (cl-remove-if
                (lambda (message)
                  (string= thread-ts (slack-ts message)))
                (slack-page-state-value state))))
         (latest-page-reply
          (car (last (sort (copy-sequence page-replies)
                           (lambda (a b)
                             (string< (slack-ts a) (slack-ts b)))))))
         (input-offset (when (>= (point) (marker-position lui-input-marker))
                         (- (point) (marker-position lui-input-marker))))
         (message-ts (get-text-property (point) 'ts))
         (output-offset (- (point) (point-min))))
    (when (slack-page-state-loaded-p state)
      (oset object has-more (and (slack-page-state-has-more state) t)))
    (slack-buffer-widen
      (let ((inhibit-read-only t))
        (delete-region (point-min) (marker-position lui-output-marker))
        (set-marker lui-output-marker (point-min))
        (goto-char lui-output-marker)
        (slack-buffer-with-deferred-hooks
          (when parent
            (slack-buffer-insert object parent t)
            (let ((lui-time-stamp-position nil))
              (lui-insert (slack-buffer-separator) t))
            (dolist (reply replies)
              (slack-buffer-insert object reply t))))
        (goto-char lui-output-marker)
        (slack-buffer-insert-page-status object state)
        (set-marker lui-output-marker (point))))
    (when (slack-page-state-loaded-p state)
      (let* ((anchor
              (if (oref object has-more) latest-page-reply latest-reply))
             (mark-read-p
              (and latest-reply
                   (not (oref object has-more))
                   (not (equal (oref object last-read)
                               (slack-ts latest-reply))))))
        (slack-buffer-update-last-read object anchor)
        (when mark-read-p
          (slack-buffer-update-mark object))))
    (cond
     (input-offset
      (goto-char (min (point-max)
                      (+ (marker-position lui-input-marker) input-offset))))
     ((and message-ts (slack-buffer-goto message-ts)))
     (t
      (goto-char (min (+ (point-min) output-offset)
                      (marker-position lui-output-marker)))))))

(cl-defmethod slack-buffer-has-next-page-p ((this slack-thread-message-buffer))
  "Return non-nil when THIS buffer has more history to load."
  (oref this has-more))

(cl-defmethod slack-buffer-delete-load-more-string ((_this slack-thread-message-buffer))
  "Remove the \"load more\" marker from the buffer for the thread message
buffer.")

(cl-defmethod slack-buffer-prepare-marker-for-history ((_this slack-thread-message-buffer))
  "Position point so history can be inserted in the thread message buffer.")

(cl-defmethod slack-buffer-insert--history ((this slack-thread-message-buffer))
  "Insert loaded history items into the buffer for THIS buffer."
  (slack-buffer-insert-history this))

(cl-defmethod slack-buffer-request-history ((this slack-thread-message-buffer) after-success &optional on-error)
  "Request older history for THIS buffer from the Slack API.
AFTER-SUCCESS is the after-success argument.  ON-ERROR is invoked on
request failure."
  (with-slots (thread-ts last-read) this
    (slack-if-let* ((team (slack-buffer-team this))
                    (room (slack-buffer-room this))
                    (message (slack-room-find-message room thread-ts)))
        (cl-labels
            ((success (_next-cursor has-more)
                      (oset this has-more has-more)
                      (funcall after-success)))
          (slack-thread-replies message room team
                                :after-success #'success
                                :on-error on-error
                                :oldest last-read)))))

(cl-defmethod slack-buffer-update-mark ((this slack-thread-message-buffer))
  "Update the read-mark position for THIS buffer."
  (with-slots (last-read thread-ts) this
    (slack-if-let* ((team (slack-buffer-team this))
                    (room (slack-buffer-room this))
                    (message (slack-room-find-message room thread-ts)))
        (slack-thread-mark message
                           room
                           last-read
                           team))))

(cl-defmethod slack-buffer-insert-history ((this slack-thread-message-buffer))
  "Insert historical messages into the buffer for THIS buffer."
  (with-slots (thread-ts last-read) this
    (slack-if-let* ((team (slack-buffer-team this))
                    (room (slack-buffer-room this))
                    (message (slack-room-find-message room thread-ts))
                    (messages (slack-message-replies message room))
                    (latest-message (car (last messages))))
        (progn
          (cl-loop for m in messages
                   do (when (string< last-read (slack-ts m))
                        (slack-buffer-insert this m t)))
          (slack-buffer-update-last-read this latest-message)
          (slack-buffer-update-mark this)))))


(cl-defmethod slack-buffer-send-message ((this slack-thread-message-buffer) message)
  "Send a MESSAGE from THIS buffer."
  (with-slots (thread-ts) this
    (slack-thread-send-message (slack-buffer-room this)
                               (slack-buffer-team this)
                               message
                               thread-ts)))

(cl-defun slack-thread-send-message (room team message thread-ts &key files on-success on-error)
  "Send MESSAGE as a reply to the thread at THREAD-TS in ROOM on TEAM.
FILES are sent as attachments.  ON-SUCCESS and ON-ERROR are passed to
`slack-message-send-internal'."
  (let ((broadcast (if (eq slack-thread-also-send-to-room 'ask)
                       (y-or-n-p (format "Also send to %s ? "
                                         (slack-room-name room team)))
                     slack-thread-also-send-to-room)))
    (let* ((payload (list (if files
                              (cons "broadcast" broadcast)
                            (cons "reply_broadcast" broadcast))
                          (cons "thread_ts" thread-ts))))
      (slack-message-send-internal message room team
                                   :payload payload
                                   :files files
                                   :on-success on-success
                                   :on-error on-error))))

(defun slack-thread-message--send (message)
  "Send MESSAGE from the current thread message buffer."
  (slack-if-let* ((buf slack-current-buffer))
      (slack-buffer-send-message buf message)))

(cl-defmethod slack-buffer-add-reaction-to-message ((this slack-thread-message-buffer) reaction ts)
  "Add a REACTION to the message selected in THIS buffer."
  (slack-message-reaction-add reaction
                              ts
                              (slack-buffer-room this)
                              (slack-buffer-team this)))

(cl-defmethod slack-buffer-remove-reaction-from-message ((this slack-thread-message-buffer) ts)
  "Remove a reaction from THIS message at point in the thread message buffer.
TS is the ts argument."
  (let* ((team (slack-buffer-team this))
         (room (slack-buffer-room this))
         (message (slack-room-find-message room ts))
         (reaction (slack-message-reaction-select
                    (slack-message-reactions message))))
    (slack-message-reaction-remove reaction ts room team)))

(cl-defmethod slack-buffer-add-star ((this slack-thread-message-buffer) ts &optional due-in-ms)
  "Star the item at point in THIS buffer.
TS is the ts argument."
  (slack-if-let* ((team (slack-buffer-team this))
                  (room (slack-buffer-room this))
                  (message (slack-room-find-message room ts)))
      (progn
        (slack-star-api-request slack-message-stars-add-url
                                (append (list (cons "channel" (oref room id)))
                                        (slack-message-star-api-params message due-in-ms))
                                team)
        (slack-message-star-added message)
        (slack-team-mark-saved team (oref room id) (slack-ts message)))))

(cl-defmethod slack-buffer-remove-star ((this slack-thread-message-buffer) ts)
  "Remove the star from THIS buffer.
TS is the ts argument."
  (slack-if-let* ((team (slack-buffer-team this))
                  (room (slack-buffer-room this))
                  (message (slack-room-find-message room ts)))
      (progn
        (slack-star-api-request slack-message-stars-remove-url
                                (append (list (cons "channel" (oref room id)))
                                        (slack-message-star-api-params message))
                                team)
        (slack-message-star-removed message)
        (slack-team-mark-unsaved team (slack-ts message)))))

(cl-defmethod slack-buffer-update ((this slack-thread-message-buffer) message &key replace)
  "Update THIS buffer after new data arrives.
MESSAGE is the message argument.  While older replies remain
unfetched (has-more), the read mark is not advanced: a live reply's
ts lies past the unfetched gap, so marking there would tell the
server the gap was read and anchor the next load-more beyond it,
permanently skipping those replies."
  (if replace (slack-buffer-replace this message)
    (unless (slack-buffer-message-exists-p this (slack-ts message))
      (let ((buffer (slack-buffer-buffer this)))
        (with-current-buffer buffer
          (slack-buffer-insert this message))
        (unless (oref this has-more)
          (slack-buffer-update-last-read this message)
          (slack-buffer-update-mark this))))))

(cl-defmethod slack-buffer-display-edit-message-buffer ((this slack-thread-message-buffer) ts)
  "Open an edit buffer for THIS message at point in the thread message buffer.
TS is the ts argument."
  (let* ((team (slack-buffer-team this))
         (room (slack-buffer-room this))
         (buf (slack-create-edit-message-buffer room team ts)))
    (slack-buffer-display buf)))

(cl-defmethod slack-buffer-share-message ((this slack-thread-message-buffer) ts)
  "Share the message at point from THIS buffer to another conversation.
TS is the ts argument."
  (let* ((team (slack-buffer-team this))
         (room (slack-buffer-room this))
         (buf (slack-create-message-share-buffer room team ts)))
    (slack-buffer-display buf)))

(cl-defmethod slack-file-upload-params ((this slack-thread-message-buffer))
  "Return THIS buffer."
  (list (cons "thread_ts" (oref this thread-ts))
        (cons "channels" (oref (slack-buffer-room this) id))))

(defun slack-thread-message-buffer-jump-to-channel-buffer ()
  "Display the channel of current thread."
  (interactive)
  (unless (eq major-mode #'slack-thread-message-buffer-mode)
    (user-error "Not in a thread"))
  (slack-if-let-room-and-team (room team)
      (slack-room-display room team)
    (user-error "Can't determine the room")))

(defun slack-thread-toggle-subscription ()
  "Toggle follow/unfollow for the current thread.
Queries the subscription status and then adds or removes the
subscription accordingly."
  (interactive)
  (unless (derived-mode-p 'slack-thread-message-buffer-mode)
    (user-error "Not in a thread buffer"))
  (slack-if-let* ((buf slack-current-buffer)
                  (team (slack-buffer-team buf))
                  (room (slack-buffer-room buf))
                  (ts (oref buf thread-ts)))
      (slack-thread-toggle-subscription-1 room ts team)))

(defun slack-thread-toggle-subscription-1 (room ts team)
  "Toggle thread subscription for ROOM, TS, and TEAM."
  (cl-labels
      ((on-subscribed ()
                      (slack-log "Followed thread" team :level 'info)
                      (message "Followed thread"))
       (on-unsubscribed ()
                        (slack-log "Unfollowed thread" team :level 'info)
                        (message "Unfollowed thread"))
       (on-success (subscriptions)
                   (if (cl-find ts subscriptions :test #'string=)
                       (slack-subscriptions-thread-remove
                        room ts team #'on-unsubscribed)
                     (slack-subscriptions-thread-add
                      room ts team #'on-subscribed)))
       (on-error (_err)
                 (slack-subscriptions-thread-add
                  room ts team #'on-subscribed)))
    (slack-subscriptions-thread-get room ts team #'on-success #'on-error)))

(define-key slack-thread-message-buffer-mode-map
            (kbd "C-c C-s") #'slack-thread-toggle-subscription)
(define-key slack-thread-message-buffer-mode-map
            (kbd "C-c C-o") #'slack-thread-message-buffer-jump-to-channel-buffer)

(provide 'slack-thread-message-buffer)
;;; slack-thread-message-buffer.el ends here
