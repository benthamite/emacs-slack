;;; slack-room-buffer.el ---                         -*- lexical-binding: t; -*-

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

(require 'alert)
(require 'eieio)
(require 'slack-util)
(require 'slack-buffer)
(require 'slack-request)
(require 'slack-file)
(require 'slack-team)
(require 'slack-buffer)
(require 'slack-message-reaction)
(require 'slack-thread)
(require 'slack-message-notification)
(require 'slack-action)
(require 'slack-message-share-buffer)
(require 'slack-reminder)
(require 'slack-bot-message)
(require 'slack-star)

(defvar slack-completing-read-function)
(defvar slack-alert-icon)
(defvar slack-message-minibuffer-local-map nil)

(defvar slack-attachment-action-keymap
  (let ((keymap (make-sparse-keymap)))
    (define-key keymap (kbd "RET") #'slack-attachment-action-run)
    (define-key keymap [mouse-1] #'slack-attachment-action-run)
    keymap))

(defvar slack-action-keymap
  (let ((keymap (make-sparse-keymap)))
    (define-key keymap (kbd "RET") #'slack-action-run)
    (define-key keymap [mouse-1] #'slack-action-run)
    keymap))

(defconst slack-message-delete-url "https://slack.com/api/chat.delete")
(defconst slack-get-permalink-url "https://slack.com/api/chat.getPermalink")

(defclass slack-room-buffer (slack-buffer)
  ((room-id :initarg :room-id :type string)
   (cursor-event-prev-ts :initform nil :type (or null string)))
  :abstract t)

(cl-defmethod slack-buffer-room ((this slack-room-buffer))
  "Return the room associated with the room buffer."
  (slack-room-find (oref this room-id)
                   (slack-buffer-team this)))

(cl-defmethod slack-buffer-toggle-reaction ((this slack-room-buffer) reaction)
  "Toggle the reaction on the message at point in the room buffer."
  (let* ((reaction-users (oref reaction users))
         (reaction-name (oref reaction name))
         (team (slack-buffer-team this))
         (room (slack-buffer-room this))
         (self-id (oref team self-id)))
    (if (cl-find-if #'(lambda (id) (string= id self-id)) reaction-users)
        (slack-message-reaction-remove reaction-name
                                       (slack-get-ts)
                                       room
                                       team)
      (slack-buffer-add-reaction-to-message this
                                            reaction-name
                                            (slack-get-ts)))))

(cl-defmethod slack-buffer-reaction-help-text ((this slack-room-buffer) reaction)
  "Return the help text displayed next to reactions in the room buffer."
  (slack-reaction-help-text reaction (slack-buffer-team this)))

(cl-defmethod slack-buffer-delete-message ((this slack-room-buffer) ts)
  "Prompt to delete the message at TS from the room buffer THIS."
  (slack-if-let* ((team (slack-buffer-team this))
                  (room (slack-buffer-room this))
                  (message (slack-room-find-message room ts)))
      (cl-labels
          ((on-delete
            (&key data &allow-other-keys)
            (slack-request-handle-error
             (data "slack-message-delete"))))
        (if (yes-or-no-p "Are you sure you want to delete this message?")
            (slack-request
             (slack-request-create
              slack-message-delete-url
              team
              :type "POST"
              :params (list (cons "ts" (slack-ts message))
                            (cons "channel" (oref room id)))
              :success #'on-delete))
          (message "Canceled")))))

(cl-defmethod slack-buffer-message-delete ((this slack-room-buffer) ts)
  "Delete the message at point from the room buffer."
  (let ((buffer (slack-buffer-buffer this)))
    (with-current-buffer buffer
      (lui-delete #'(lambda () (equal (get-text-property (point) 'ts)
                                      ts))))))

(cl-defmethod slack-buffer-copy-link ((this slack-room-buffer) ts &optional success-callback)
  "Use slack permakink api to retrieve an http link to the message at TS.
SUCCESS-CALLBACK allows you to run a function on that permalink."
  (slack-if-let* ((team (slack-buffer-team this))
                  (room (slack-buffer-room this))
                  (message (slack-room-find-message room ts))
                  (template "https://%s.slack.com/archives/%s/p%s%s"))
      (cl-labels
          ((on-success (&key data &allow-other-keys)
             (slack-request-handle-error
              (data "slack-get-permalink")
              (let ((permalink (plist-get data :permalink)))
                (kill-new permalink)
                (message "Link Copied to Clipboard")
                (when (functionp success-callback) (funcall success-callback permalink))))))
        (slack-request
         (slack-request-create
          slack-get-permalink-url
          team
          :type "POST"
          :params (list (cons "channel" (oref room id))
                        (cons "message_ts" ts))
          :success #'on-success)))))

(cl-defmethod slack-buffer--replace ((this slack-room-buffer) ts)
  "Replace the rendered message identified by the argument in the room buffer."
  (slack-if-let* ((room (slack-buffer-room this))
                  (message (slack-room-find-message room ts)))
      (slack-buffer-replace this message)))

(cl-defmethod slack-buffer-toggle-email-expand ((this slack-room-buffer) file-id)
  "Toggle the expanded/collapsed state of the email at point in the room buffer."
  (slack-if-let* ((room (slack-buffer-room this))
                  (ts (get-text-property (point) 'ts))
                  (message (slack-room-find-message room ts))
                  (file (cl-find-if
                         #'(lambda (e) (string= (oref e id)
                                                file-id))
                         (oref message files))))
      (progn
        (oset file is-expanded (not (oref file is-expanded)))
        (slack-buffer-update this message :replace t))))

(defun slack-pins-request (url room team ts)
  "Call the pins API at URL for message TS of ROOM in TEAM."
  (cl-labels ((on-pins-add
               (&key data &allow-other-keys)
               (slack-request-handle-error
                (data "slack-message-pins-request"))))
    (slack-request
     (slack-request-create
      url
      team
      :params (list (cons "channel" (oref room id))
                    (cons "timestamp" ts))
      :success #'on-pins-add
      ))))

(cl-defmethod slack-buffer-pins-remove ((this slack-room-buffer) ts)
  "Unpin the message at point from the room buffer."
  (slack-pins-request slack-message-pins-remove-url
                      (slack-buffer-room this)
                      (slack-buffer-team this)
                      ts))

(cl-defmethod slack-buffer-pins-add ((this slack-room-buffer) ts)
  "Pin the message at point in the room buffer."
  (slack-pins-request slack-message-pins-add-url
                      (slack-buffer-room this)
                      (slack-buffer-team this)
                      ts))

(cl-defmethod slack-buffer-remove-star ((this slack-room-buffer) ts)
  "Remove the star from the item at point in the room buffer."
  (slack-if-let* ((team (slack-buffer-team this))
                  (room (slack-buffer-room this))
                  (message (slack-room-find-message room ts)))
      (progn
        (slack-star-api-request slack-message-stars-remove-url
                                (list (cons "ts" (slack-ts message))
                                      (cons "item_id" (oref room id))
                                      (cons "item_type" "message"))
                                team)
        (slack-message-star-removed message)
        (slack-team-mark-unsaved team (slack-ts message)))))

(cl-defmethod slack-buffer-add-star ((this slack-room-buffer) ts)
  "Star the item at point in the room buffer."
  (slack-if-let* ((team (slack-buffer-team this))
                  (room (slack-buffer-room this))
                  (message (slack-room-find-message room ts)))
      (progn
        (slack-star-api-request slack-message-stars-add-url
                                (list (cons "item_id" (oref room id))
                                      (cons "ts" (slack-ts message))
                                      (cons "item_type" "message"))
                                team)
        (slack-message-star-added message)
        (slack-team-mark-saved team (oref room id) (slack-ts message)))))

(cl-defmethod slack-buffer-add-reaction-to-message ((this slack-room-buffer) reaction ts)
  "Add a reaction to the message selected in the room buffer."
  (slack-message-reaction-add reaction
                              ts
                              (slack-buffer-room this)
                              (slack-buffer-team this)))

(cl-defmethod slack-buffer-remove-reaction-from-message ((this slack-room-buffer) ts)
  "Remove a reaction from the message at point in the room buffer."
  (let* ((team (slack-buffer-team this))
         (room (slack-buffer-room this))
         (message (slack-room-find-message room ts))
         (reactions (slack-message-reactions message))
         (reaction (slack-message-reaction-select reactions)))
    (slack-message-reaction-remove reaction ts room team)))

(cl-defmethod slack-buffer-share-message ((this slack-room-buffer) ts)
  "Share the message at point from the room buffer to another conversation."
  (let* ((team (slack-buffer-team this))
         (room (slack-buffer-room this))
         (buf (slack-create-message-share-buffer room team ts)))
    (slack-buffer-display buf)))

(cl-defmethod slack-buffer-display-edit-message-buffer ((this slack-room-buffer) ts)
  "Open an edit buffer for the message at point in the room buffer."
  (let* ((team (slack-buffer-team this))
         (room (slack-buffer-room this))
         (buf (slack-create-edit-message-buffer room team ts)))
    (slack-buffer-display buf)))

(cl-defmethod slack-buffer-update-mark ((_this slack-room-buffer) &key (_force nil))
  "Update the read-mark position for the room buffer.")

(cl-defmethod slack-buffer-builtin-actions ((this slack-room-buffer) ts handler)
  "Build the built-in message action list for TS in THIS and pass it to HANDLER."
  (let ((display-follow nil))
    (let ((team (slack-buffer-team this))
          (room (slack-buffer-room this)))
      (cl-labels
          ((get-message () (slack-room-find-message room ts))
           (handle-follow-message () (slack-subscriptions-thread-add room ts team))
           (handle-unfollow-message () (slack-subscriptions-thread-remove room ts team))
           (handle-copy-link () (slack-buffer-copy-link this ts))
           (handle-mark-unread () (slack-buffer-update-mark this :force t))
           (handle-pin () (slack-buffer-pins-add this ts))
           (handle-un-pin () (slack-buffer-pins-remove this ts))
           (handle-delete-message () (slack-buffer-delete-message this ts))
           (handle-star-message () (slack-buffer-add-star this ts))
           (handle-unstar-message () (slack-buffer-remove-star this ts))
           (handle-add-reaction () (let ((reaction
                                          (slack-message-reaction-input team)))
                                     (slack-buffer-add-reaction-to-message
                                      this reaction ts)))
           (handle-remove-reaction () (slack-buffer-remove-reaction-from-message
                                       this ts))
           (handle-share () (slack-buffer-share-message this ts))
           (handle-edit () (slack-buffer-display-edit-message-buffer this
                                                                     ts))
           (handle-remind () (slack-if-let* ((message (get-message)))
                                 (slack-reminder-add-from-message room
                                                                  message
                                                                  team)))
           (display-pin-p ()
                          (slack-if-let* ((message (get-message)))
                              (not (slack-message-pinned-to-room-p message room))))
           (display-un-pin-p ()
                             (slack-if-let* ((message (get-message)))
                                 (slack-message-pinned-to-room-p message room)))
           (display-follow-p () display-follow)
           (display-unfollow-p () (not display-follow))
           (display-star-p () (slack-if-let* ((message (get-message)))
                                  (not (slack-message-starred-p message))))
           (display-unstar-p () (slack-if-let* ((message (get-message)))
                                    (slack-message-starred-p message)))
           (message-buffer-p () (eq (eieio-object-class this)
                                    'slack-message-buffer)))
        (let ((builtins `(:app_name
                          "Slack"
                          :actions
                          ((:name "Follow message"
                            :handler ,#'handle-follow-message
                            :display-p ,#'display-follow-p)
                           (:name "Unfollow message"
                            :handler ,#'handle-unfollow-message
                            :display-p ,#'display-unfollow-p)
                           (:name "Save for later"
                            :handler ,#'handle-star-message
                            :display-p ,#'display-star-p)
                           (:name "Remove from saved"
                            :handler ,#'handle-unstar-message
                            :display-p ,#'display-unstar-p)
                           (:name "Add reaction to message"
                            :handler ,#'handle-add-reaction)
                           (:name "Remove reaction from message"
                            :handler ,#'handle-remove-reaction)
                           (:name "Edit message"
                            :handler ,#'handle-edit)
                           (:name "Share message"
                            :handler ,#'handle-share)
                           (:name "Copy link"
                            :handler ,#'handle-copy-link)
                           (:name "Mark unread"
                            :display-p ,#'message-buffer-p
                            :handler ,#'handle-mark-unread)
                           (:name "Remind me about this"
                            :handler ,#'handle-remind)
                           (:name ,(format "Pin to %s%s"
                                           (if (slack-im-p room) "@" "#")
                                           (slack-room-name room team))
                            :display-p ,#'display-pin-p
                            :handler ,#'handle-pin)
                           (:name ,(format "Un-pin from %s%s"
                                           (if (slack-im-p room) "@" "#")
                                           (slack-room-name room team))
                            :display-p ,#'display-un-pin-p
                            :handler ,#'handle-un-pin)
                           (:name "Delete message"
                            :handler ,#'handle-delete-message)))))
          (cl-labels
              ((on-success (subscriptions)
                           (if (cl-find ts subscriptions :test #'string=)
                               (setq display-follow nil)
                             (setq display-follow t))
                           (funcall handler builtins))
               (on-error (_err) (funcall handler builtins)))
            (slack-subscriptions-thread-get room ts team #'on-success #'on-error)))))))

(cl-defmethod slack-buffer-execute-message-action ((this slack-room-buffer) ts)
  "Execute a message action on the message at point in the room buffer."
  (let ((team (slack-buffer-team this))
        (room (slack-buffer-room this)))
    (cl-labels
        ((run-action (selected)
                     (slack-if-let*
                         ((action (cdr selected))
                          (app (car selected))
                          (type (plist-get action :type))
                          (action-id (plist-get action :action_id))
                          (app-id (plist-get app :app_id)))
                         (slack-actions-run ts room type action-id app-id team)))
         (handler (builtin actions)
                  (slack-if-let*
                      ((selected (slack-actions-select (cons builtin actions)))
                       (action (cdr selected)))
                      (if (functionp (plist-get action :handler))
                          (funcall (plist-get action :handler))
                        (run-action selected))))
         (on-success
          (actions)
          (slack-buffer-builtin-actions
           this ts
           #'(lambda (builtin) (run-at-time nil nil #'handler builtin actions))))
         (on-error
          (_err)
          (slack-buffer-builtin-actions
           this ts
           #'(lambda (builtin) (run-at-time nil nil #'handler builtin nil)))))
      (slack-actions-list team #'on-success #'on-error))))

(defun slack-message-delete ()
  "Delete the message at point in the current Slack buffer."
  (interactive)
  (slack-if-let* ((buf slack-current-buffer))
      (slack-buffer-delete-message buf (slack-get-ts))))

(defun slack-message-copy-link (&optional success-callback)
  "Copy permalink at point.
Optionally pass SUCCESS-CALLBACK to perform an action on the permalink obtained."
  (interactive)
  (slack-buffer-copy-link slack-current-buffer (slack-get-ts) success-callback))

(defun slack-message-copy-id ()
  "Copy the message timestamp (ID) at point to the kill ring."
  (interactive)
  (if-let* ((ts (slack-get-ts)))
      (progn
        (kill-new ts)
        (message "Copied message id: %s" ts))
    (user-error "No message at point")))

(defun slack-open-url (url)
  "Open a slack URL (permalink) in emacs-slack."
  (interactive
   (list (cond ((url-p (car kill-ring)) (car kill-ring))
               ((thing-at-point 'url) (thing-at-point 'url))
               (t (read-string "Enter slack url:")))))
  (if-let* ((info (slack-permalink-to-info url))
            (team-domain (plist-get info :team-domain))
            (team (slack-team-find-by-domain team-domain))
            (room-id (plist-get info :room-id))
            (room (or
                   (--> team
                        slack-team-ims
                        (--find (equal room-id (oref it id)) it))
                   (--> team
                        slack-team-channels
                        (--find (equal room-id (oref it id)) it))
                   ))
            (ts (plist-get info :ts))
            (thread-ts (plist-get info :thread-ts)))
      (slack-open-message
       team
       room
       ts
       thread-ts)
    (error (format "Not an url: %s" url))
    ))

(defalias 'slack-open-link 'slack-open-url  "Open a Slack permalink in emacs-slack.")

(defun slack-insert-link (title url)
  "Insert link TITLE and URL in markdown fomat."
  (interactive
   (list
    (or (when (region-active-p)
          (substring-no-properties (funcall region-extract-function)))
        (read-string "Title:"))
    (cond (
           ;; TODO there must be a nicer way to check url, url-p was wrong because it just checks if it is an url-object
           (with-temp-buffer (clipboard-yank) (goto-char (point-min)) (thing-at-point-url-at-point)) (with-temp-buffer (clipboard-yank))
           (with-temp-buffer (insert (car kill-ring)) (goto-char (point-min)) (thing-at-point-url-at-point)) (car kill-ring))
          (t (read-string "URL:")))))
  (when (region-active-p) (delete-region (region-beginning) (region-end)))
  (insert (format "[%s](%s)" title url)))

(defun slack-jump-to-browser ()
  "Attempt to jump from message at point to web slack app."
  (interactive)
  (slack-message-copy-link
   (lambda (link) (browse-url (string-replace "archives" "messages" link)))))

(defun slack-jump-to-app ()
  "Attempt to jump from message at point to slack app."
  (interactive)
  (slack-message-copy-link #'browse-url))

(defun slack-message-test-notification ()
  "Debug notification.
Execute this function when cursor is on some message."
  (interactive)
  (let* ((ts (slack-get-ts))
         (team (slack-buffer-team slack-current-buffer))
         (room (slack-buffer-room slack-current-buffer))
         (message (slack-room-find-message room ts)))
    (slack-message-notify message room team)))

(defun slack--get-channel-id ()
  "Copy the current room's channel ID to the kill ring and echo it."
  (interactive)
  (with-current-buffer (current-buffer)
    (slack-if-let* ((buffer slack-current-buffer)
                    (boundp (slot-boundp buffer 'room))
                    (room (slack-buffer-room buffer)))
        (progn
          (kill-new (oref room id))
          (message "%s" (oref room id))))))

(defun slack-attachment-action-run ()
  "Run the attachment action at point in the current Slack buffer."
  (interactive)
  (slack-if-let* ((buffer slack-current-buffer)
                  (room (slack-buffer-room buffer))
                  (team (slack-buffer-team buffer))
                  (type (get-text-property (point) 'type))
                  (attachment-id (get-text-property (point) 'attachment-id))
                  (ts (slack-get-ts))
                  (message (slack-room-find-message room ts))
                  (action (get-text-property (point) 'action)))
      (when (slack-attachment-action-confirm action)
        (slack-if-let* ((callback-id (get-text-property (point) 'callback-id))
                        (common-payload (list
                                         (cons "attachment_id" (number-to-string
                                                                attachment-id))
                                         (cons "callback_id" callback-id)
                                         (cons "is_ephemeral" (oref message
                                                                    is-ephemeral))
                                         (cons "message_ts" ts)
                                         (cons "channel_id" (oref room id))))
                        (service-id (if (slack-bot-message-p message)
                                        (slack-message-bot-id message)
                                      "B01")))
            (let ((url "https://slack.com/api/chat.attachmentAction")
                  (params (list (cons "payload"
                                      (json-encode-alist
                                       (slack-attachment-action-run-payload
                                        action
                                        team
                                        common-payload
                                        service-id)))
                                (cons "service_id" service-id)
                                (cons "client_token"
                                      (slack-team-client-token team)))))
              (cl-labels
                  ((log-error (err)
                              (slack-log (format "Error: %s, URL: %s, PARAMS: %s"
                                                 err
                                                 url
                                                 params)
                                         team
                                         :level 'error))
                   (on-success (&key data &allow-other-keys)
                               (slack-request-handle-error
                                (data "slack-attachment-action-run" #'log-error))))
                (slack-request
                 (slack-request-create
                  url
                  team
                  :type "POST"
                  :params params
                  :success #'on-success))))
          (slack-if-let* ((url (oref action url)))
              (browse-url url))))))

(defun slack-message-run-action ()
  "Prompt for and run a message action on the non-ephemeral message at point."
  (interactive)
  (slack-if-let* ((buffer slack-current-buffer)
                  (room (slack-buffer-room buffer))
                  (ts (slack-get-ts))
                  (message (slack-room-find-message room ts))
                  (not-ephemeral-messagep (not (oref message is-ephemeral))))
      (slack-buffer-execute-message-action buffer ts)))

(defun slack-action-run ()
  "Invoke the bot action encoded at point via chat.action."
  (interactive)
  (slack-if-let* ((bot (get-text-property (point) 'bot))
                  (payload (get-text-property (point) 'payload))
                  (buffer slack-current-buffer)
                  (team (slack-buffer-team buffer)))
      (let ((url "https://slack.com/api/chat.action")
            (params (list (cons "bot" bot)
                          (cons "payload" payload))))
        (cl-labels
            ((log-error (err) (format "Error: %s, URL: %s, PARAMS: %s"
                                      err
                                      url
                                      params))
             (on-success (&key data &allow-other-keys)
                         (slack-request-handle-error
                          (data "slack-action-run" #'log-error))))
          (slack-request
           (slack-request-create
            url
            team
            :type "POST"
            :params params
            :success #'on-success))))))

(defun slack-message-setup-minibuffer-keymap ()
  "Initialize `slack-message-minibuffer-local-map' once, binding RET to newline."
  (unless slack-message-minibuffer-local-map
    (setq slack-message-minibuffer-local-map
          (let ((map (make-sparse-keymap)))
            (define-key map (kbd "RET") 'newline)
            (set-keymap-parent map minibuffer-local-map)
            map))))

(defun slack-message-read-from-minibuffer ()
  "Read a multi-line message string from the minibuffer, using RET for newline."
  (let ((prompt "Message: "))
    (slack-message-setup-minibuffer-keymap)
    (read-from-minibuffer
     prompt
     nil
     slack-message-minibuffer-local-map)))

(defun slack-message-follow ()
  "Subscribe to the thread rooted at the message at point."
  (interactive)
  (slack-if-let* ((buffer slack-current-buffer))
      (slack-buffer-follow-message buffer)))

(cl-defmethod slack-buffer-follow-message ((this slack-room-buffer))
  "Subscribe to the thread at point in room buffer THIS."
  (slack-if-let* ((ts (slack-get-ts)))
      (let ((team (slack-buffer-team this))
            (room (slack-buffer-room this)))
        (cl-labels
            ((after-success ()
                            (slack-log "Successfully followed."
                                       team :level 'info)))
          (slack-subscriptions-thread-add room ts team
                                          #'after-success)))))

(defun slack-message-unfollow ()
  "Unsubscribe from the thread rooted at the message at point."
  (interactive)
  (slack-if-let* ((buffer slack-current-buffer))
      (slack-buffer-unfollow-message buffer)))

(cl-defmethod slack-buffer-unfollow-message ((this slack-room-buffer))
  "Unsubscribe from the thread at point in room buffer THIS."
  (slack-if-let* ((ts (slack-get-ts)))
      (let ((team (slack-buffer-team this))
            (room (slack-buffer-room this)))
        (cl-labels
            ((after-success ()
                            (slack-log "Successfully unfollowed."
                                       team :level 'info)))
          (slack-subscriptions-thread-remove room ts team
                                             #'after-success)))))

(cl-defmethod slack-buffer-block-action-container ((this slack-room-buffer) message)
  "Return the block-action container alist for MESSAGE in room buffer THIS."
  (let ((room (slack-buffer-room this)))
    (list (cons "type" "message")
          (cons "message_ts" (slack-ts message))
          (cons "channel_id" (oref room id))
          (cons "is_ephemeral" (or (oref message is-ephemeral)
                                   :json-false)))))

(cl-defmethod slack-message-block-action-service-id ((this slack-message))
  "Return the service id to use when executing block actions for THIS message."
  (if (slack-bot-message-p this)
      (slack-message-bot-id this)
    "B01"))

(defun slack-block-find-action-from-payload (action-payload message)
  "Return the block element referenced by ACTION-PAYLOAD inside MESSAGE."
  (slack-if-let* ((block-id (cdr-safe (assoc-string "block_id" action-payload)))
                  (bl (slack-message-find-block message block-id))
                  (action-id (cdr-safe (assoc-string "action_id" action-payload))))
      (slack-block-find-action bl action-id)))

(defmacro slack-with-block-action (buffer &rest body)
  "Execute BODY with block action context from BUFFER.
Binds ROOM, TEAM, MESSAGE, ACTION, and BLOCK-ELEMENT from the
text property at point."
  (declare (indent 1) (debug t))
  `(slack-if-let* ((cur-point (point))
                   (ts (slack-get-ts))
                   (room (slack-buffer-room ,buffer))
                   (team (slack-buffer-team ,buffer))
                   (message (slack-room-find-message room ts))
                   (action (get-text-property cur-point 'slack-action-payload))
                   (block-element (slack-block-find-action-from-payload action message)))
     ,@body))

(defun slack-block-action--execute-with-selection (buffer message action team &optional selected-pair)
  "Execute a block action for MESSAGE in BUFFER.
ACTION is the action payload, TEAM is the workspace.
SELECTED-PAIR, when non-nil, is a cons cell appended to ACTION."
  (slack-block-action-execute
   (slack-message-block-action-service-id message)
   (list (if selected-pair
             (append action (list selected-pair))
           action))
   (slack-buffer-block-action-container buffer message)
   team))

(defun slack-block-action--option-payload (selected-option)
  "Build a selected_option payload cons from SELECTED-OPTION."
  (with-slots (text value) selected-option
    (cons "selected_option"
          (list (cons "text" (slack-block-action-payload text))
                (cons "value" value)))))

(cl-defmethod slack-buffer-execute-button-block-action ((this slack-room-buffer))
  "Execute a button block element action from the room buffer."
  (slack-with-block-action this
    (when (slack-block-handle-confirm block-element)
      (slack-if-let* ((url (oref block-element url)))
          (browse-url url)
        (slack-block-action--execute-with-selection this message action team)))))

(cl-defmethod slack-buffer-execute-conversation-select-block-action ((this slack-room-buffer))
  "Execute a conversation-select block element action from the room buffer."
  (slack-with-block-action this
    (slack-if-let* ((selected (slack-room-select (append (slack-team-channels team)
                                                         (slack-team-groups team)
                                                         (slack-team-ims team))
                                                 team)))
        (when (slack-block-handle-confirm block-element)
          (slack-block-action--execute-with-selection
           this message action team
           (cons "selected_conversation" (oref selected id)))))))

(cl-defmethod slack-buffer-execute-channel-select-block-action ((this slack-room-buffer))
  "Execute a channel-select block element action from the room buffer."
  (slack-with-block-action this
    (slack-if-let* ((selected (slack-room-select (append (slack-team-channels team) nil) team)))
        (when (slack-block-handle-confirm block-element)
          (slack-block-action--execute-with-selection
           this message action team
           (cons "selected_channel" (oref selected id)))))))

(cl-defmethod slack-buffer-execute-user-select-block-action ((this slack-room-buffer))
  "Execute a user-select block element action from the room buffer."
  (slack-with-block-action this
    (slack-if-let* ((selected (slack-select-from-list
                                  ((slack-user-name-alist
                                    team :filter #'(lambda (users)
                                                     (cl-remove-if
                                                      #'slack-user-hidden-p
                                                      users)))
                                   "Select User: "))))
        (when (slack-block-handle-confirm block-element)
          (slack-block-action--execute-with-selection
           this message action team
           (cons "selected_user" (plist-get selected :id)))))))

(cl-defmethod slack-message-find-block ((this slack-message) block-id)
  "Return the block with BLOCK-ID inside message THIS, or nil if absent."
  (with-slots (blocks) this
    (cl-find-if #'(lambda (e) (and (slot-boundp e 'block-id)
                                   (string= block-id (oref e block-id))))
                blocks)))

(cl-defmethod slack-buffer-execute-static-select-block-action ((this slack-room-buffer))
  "Execute a static-select block element action from the room buffer."
  (slack-with-block-action this
    (slack-if-let* ((selected (slack-block-select-option block-element)))
        (when (slack-block-handle-confirm block-element)
          (slack-block-action--execute-with-selection
           this message action team
           (slack-block-action--option-payload selected))))))

(cl-defmethod slack-buffer-execute-external-select-block-action ((this slack-room-buffer))
  "Execute an external-select block element action from the room buffer."
  (slack-with-block-action this
    (cl-labels
        ((success (options option-groups)
                  (slack-if-let* ((selected (if options
                                                (slack-block-select-from-options block-element options)
                                              (slack-block-select-from-option-groups block-element option-groups))))
                      (when (slack-block-handle-confirm block-element)
                        (slack-block-action--execute-with-selection
                         this message action team
                         (slack-block-action--option-payload selected))))))
      (slack-block-fetch-suggestions
       block-element
       (slack-message-block-action-service-id message)
       (slack-buffer-block-action-container this message)
       team
       #'success))))

(cl-defmethod slack-buffer-execute-overflow-menu-block-action ((this slack-room-buffer))
  "Execute an overflow menu block element action from the room buffer."
  (slack-with-block-action this
    (slack-if-let* ((options (oref block-element options))
                    (selected (slack-block-select-from-options block-element options)))
        (when (slack-block-handle-confirm block-element)
          (slack-block-action--execute-with-selection
           this message action team
           (slack-block-action--option-payload selected))))))

(cl-defmethod slack-buffer-execute-datepicker-block-action ((this slack-room-buffer))
  "Execute a datepicker block element action from the room buffer."
  (slack-with-block-action this
    (slack-if-let* ((selected-date (read-from-minibuffer "Date (YYYY-MM-DD): "
                                                         (oref block-element initial-date))))
        (when (slack-block-handle-confirm block-element)
          (slack-block-action--execute-with-selection
           this message action team
           (cons "selected_date" selected-date))))))

(provide 'slack-room-buffer)
;;; slack-room-buffer.el ends here
