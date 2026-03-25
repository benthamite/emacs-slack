;;; slack-activity-feed-buffer.el ---                -*- lexical-binding: t; -*-

;; Copyright (C) 2024

;; Author:  <andrea-dev@hotmail.com>
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

;; This buffer allow you to see the latest activity in slack. You can invoke it with `slack-activity-feed-show'.

;;; Code:

(require 'eieio)
(require 'slack-util)
(require 'slack-buffer)
(require 'slack-search)
(require 'slack-room)
(require 'slack-message-buffer)
(require 'slack-team)
(require 'dash)
(require 's)

(declare-function "slack-message-get-or-fetch" "slack-message.el")

(defvar slack-activity-feed-url "https://slack.com/api/activity.feed")
(defvar slack-activity-feed-mode-show-only-unread nil "If non-nil, show only unread activity.")

(defun slack-activity-feed-toggle-mode ()
  (interactive)
  (setq slack-activity-feed-mode-show-only-unread (not slack-activity-feed-mode-show-only-unread))
  (message (if slack-activity-feed-mode-show-only-unread
               "slack-activity-feed will show only unread messages next time"
             "slack-activity-feed will show read and unread messages next time")))

(defun slack-activity-feed--jbool (jf)
  "Return nil if JF is JSON false, t otherwise."
  (not (eq jf :json-false)))

(defun slack-activity-feed--parse-item (item-data)
  "Parse a single ITEM-DATA plist from the activity.feed API response."
  (let* ((i (plist-get item-data :item))
         (type (plist-get i :type))
         (m (plist-get i :message))
         (r (plist-get i :reaction))
         (bundle-payload (plist-get (plist-get i :bundle_info) :payload))
         (bundle-msg (plist-get bundle-payload :message))
         ;; thread_v2: thread entry with channel_id, thread_ts, latest_ts
         (thread-entry (plist-get bundle-payload :thread_entry))
         ;; dm: DM entry with latest_message containing ts and channel
         (dm-entry (plist-get (plist-get bundle-payload :dm_entry) :latest_message))
         ;; generic_system_alert: channel invite with click_target_id
         (alert-payload (plist-get i :generic_system_alert_payload))
         ;; Resolve ts and channel from whichever source is available
         (ts (or (plist-get m :ts)
                 (plist-get bundle-msg :ts)
                 (plist-get thread-entry :latest_ts)
                 (plist-get dm-entry :ts)))
         (channel (or (plist-get m :channel)
                      (plist-get bundle-msg :channel)
                      (plist-get thread-entry :channel_id)
                      (plist-get dm-entry :channel)
                      (plist-get alert-payload :click_target_id)))
         (thread-ts (or (plist-get m :thread_ts)
                        (plist-get thread-entry :thread_ts))))
    (make-instance
     'slack-activity
     :is-unread (slack-activity-feed--jbool (plist-get item-data :is_unread))
     :feed-ts (format "%s" (plist-get item-data :feed_ts))
     :item (make-instance
            'activity-item
            :type type
            :message (make-instance
                      'activity-message
                      :ts (format "%s" (or ts "0"))
                      :channel (format "%s" (or channel "unknown"))
                      :is-broadcast (slack-activity-feed--jbool
                                     (plist-get m :is_broadcast))
                      :thread-ts (when thread-ts (format "%s" thread-ts))
                      :author-id (when-let ((id (plist-get m :author_user_id)))
                                   (format "%s" id)))
            :reaction (when r (make-instance
                               'activity-reaction
                               :user (format "%s" (plist-get r :user))
                               :name (format "%s" (plist-get r :name))))))))

(defun slack-activity-feed-request (team &optional after-success cursor)
  "Request activity feed for CHANNEL-ID of TEAM.
Run an action on the data returned with AFTER-SUCCESS."
  (cl-labels
      ((on-success (&key data &allow-other-keys)
         (slack-request-handle-error
          (data "slack-activity-feed-request")
          (if after-success
              (funcall after-success data)))))
    (slack-request
     (slack-request-create
      slack-activity-feed-url
      team
      :type "POST"
      :success #'on-success
      :data (let ((token (or (oref team :enterprise-token)
                             (oref team :token)))
                  (mode (if slack-activity-feed-mode-show-only-unread "priority_unreads_v1" "chrono_reads_and_unreads")))
              (concat "------WebKitFormBoundaryh7x3DqJqAIvkEcie\r\nContent-Disposition: form-data; name=\"token\"\r\n\r\n" token "\r\n------WebKitFormBoundaryh7x3DqJqAIvkEcie\r\nContent-Disposition: form-data; name=\"limit\"\r\n\r\n20\r\n------WebKitFormBoundaryh7x3DqJqAIvkEcie\r\nContent-Disposition: form-data; name=\"types\"\r\n\r\nthread_v2,dm,generic_system_alert,message_reaction,internal_channel_invite,list_record_edited,bot_dm_bundle,at_user,at_user_group,at_channel,at_everyone,keyword,list_record_assigned,list_user_mentioned,external_channel_invite,shared_workspace_invite,external_dm_invite\r\n------WebKitFormBoundaryh7x3DqJqAIvkEcie\r\nContent-Disposition: form-data; name=\"mode\"\r\n\r\n" mode "\r\n" (if cursor (concat "------WebKitFormBoundaryh7x3DqJqAIvkEcie\r\nContent-Disposition: form-data; name=\"cursor\"\r\n\r\n" cursor "\r\n------WebKitFormBoundaryh7x3DqJqAIvkEcie--\r\n") "") "------WebKitFormBoundaryh7x3DqJqAIvkEcie\r\nContent-Disposition: form-data; name=\"_x_reason\"\r\n\r\nfetchActivityFeed\r\n------WebKitFormBoundaryh7x3DqJqAIvkEcie\r\nContent-Disposition: form-data; name=\"_x_mode\"\r\n\r\nonline\r\n------WebKitFormBoundaryh7x3DqJqAIvkEcie\r\nContent-Disposition: form-data; name=\"_x_sonic\"\r\n\r\ntrue\r\n------WebKitFormBoundaryh7x3DqJqAIvkEcie\r\nContent-Disposition: form-data; name=\"_x_app_name\"\r\n\r\nclient\r\n------WebKitFormBoundaryh7x3DqJqAIvkEcie--\r\n"))
      :headers (list
                (cons "content-type"
                      "multipart/form-data; boundary=----WebKitFormBoundaryh7x3DqJqAIvkEcie"))))))

(defclass slack-activity-feed ()
  ((activities :initarg :activities :initform nil :type (or null list))
   (pagination :initarg :pagination :type (or null string))
   (last :initarg :last :type (or null integer))))
(define-derived-mode slack-activity-feed-buffer-mode slack-buffer-mode "Slack Activity Feed"
  (remove-hook 'lui-post-output-hook 'slack-display-image t))

(defclass slack-activity-feed-buffer (slack-buffer)
  ((activity-feed :initarg :activity-feed :type slack-activity-feed)))

(cl-defmethod slack-buffer-name ((_class (subclass slack-activity-feed-buffer)) team)
  (format "*slack: %s Activity Feed %s*"
          (oref team name)
          (format-time-string "%Y-%m-%d %H:%M:%S")
          ))

(cl-defmethod slack-buffer-name ((this slack-activity-feed-buffer))
  (format "*slack: %s Activity Feed %s*"
          (slack-team-name (slack-buffer-team this))
          (format-time-string "%Y-%m-%d %H:%M:%S")
          ))

(cl-defmethod slack-buffer-key ((_class (subclass slack-activity-feed-buffer)))
  "activity feed")

(cl-defmethod slack-buffer-key ((this slack-activity-feed-buffer))
  (slack-buffer-key 'slack-activity-feed-buffer))

(cl-defmethod slack-team-buffer-key ((_class (subclass slack-activity-feed-buffer)))
  'slack-activity-feed-buffer)

(defun slack-create-activity-feed-buffer (activity-feed team)
  (let ((buffer (slack-buffer-find 'slack-activity-feed-buffer team)))
    (when buffer (kill-buffer (oref buffer buf)))
    (make-instance 'slack-activity-feed-buffer
                   :team-id (oref team id)
                   :activity-feed activity-feed)))

(defclass activity-message ()
  ((ts :initarg :ts :type string)
   (channel :initarg :channel :type string)
   (is-broadcast :initarg :is-broadcast :type boolean)
   (thread-ts :initarg :thread-ts :type (or null string))
   (author-id :initarg :author-id :type (or null string))))

(cl-defmethod slack-activity-message-to-string ((this activity-message) team &optional activity-type)
  "Format THIS activity-message of TEAM as a string for presentation.
ACTIVITY-TYPE is the activity type string (e.g. \"thread_reply\")."
  (with-slots (channel ts is-broadcast thread-ts author-id) this
    (condition-case err ;; this is to find out more easily messages that we fail to handle
        (let* ((room (slack-room-find channel team))
               (room-name (or (ignore-errors (slack-room-name room team))
                              "name not available - try to update channel list"))
               (location (format "%s%s"
                                 (if (slack-channel-p room) "#" "@")
                                 room-name))
               (type-prefix (pcase activity-type
                              ((or "thread_reply" "thread_v2") "Thread in ")
                              ((or "at_user" "at_user_group" "at_channel" "at_everyone")
                               "Mentioned in ")
                              ("internal_channel_invite" "Invited to ")
                              (_ "")))
               (context-header (propertize (concat type-prefix location)
                                           'face 'slack-search-result-message-header-face))
               (fetched-msg
                (condition-case msg-err
                    (when (or ts thread-ts)
                      (slack-message-get-or-fetch
                       ts (oref room id) team thread-ts))
                  (error
                   (message "slack-activity-message-to-string: Loading messages failed with: %S"
                            (error-message-string msg-err))
                   nil))))
          (propertize (concat context-header "\n"
                              (if fetched-msg
                                  (slack-message-to-string fetched-msg team)
                                "TODO"))
                      'ts ts
                      'team-id (oref team id)
                      'room-id (oref room id)
                      'thread-ts thread-ts))
      (error
       (format "TODO there was an error, please report this message at https://github.com/emacs-slack/emacs-slack/issues:\n%s"
               (list this err))))))

(defclass activity-reaction ()
  ((user :initarg :user :type string)
   (name :initarg :name :type string)))

(cl-defmethod slack-activity-reaction-to-string ((this activity-reaction) team)
  (with-slots (user name) this
    (format "  %s reacted with :%s:"
            (or (slack-user-name user team) user)
            name)))

(defclass activity-item ()
  ((type :initarg :type :type string)
   (message :initarg :message :type activity-message)
   (reaction :initarg :reaction :type (or null activity-reaction))))

(cl-defmethod slack-activity-item-to-string ((this activity-item) team)
  "Convert THIS activity for TEAM into a string."
  (with-slots (type message reaction) this
    (if (equal type "bot_dm_bundle") ;; this bot message seem to have no valuable information
        ""
      (concat
       (slack-activity-message-to-string message team type)
       (when reaction (concat "\n" (slack-activity-reaction-to-string reaction team)))))))

(defclass slack-activity ()
  ((is-unread :initarg :is-unread :type boolean)
   (feed-ts :initarg :feed-ts :type string)
   (item :initarg :item :type activity-item)))

(cl-defmethod slack-activity-to-string ((this slack-activity) team)
  (with-slots (is-unread item) this
    (format "%s %s" (if is-unread "*" " ") (slack-activity-item-to-string item team))))

(cl-defmethod slack-buffer-insert ((this slack-activity-feed-buffer) activity)
  (let* ((team (slack-buffer-team this))
         (time (slack-ts-to-time (oref activity feed-ts)))
         (is-unread (oref activity is-unread))
         (item (oref activity item))
         (type (oref item type))
         (msg (oref item message))
         (reaction (oref item reaction)))
    (unless (equal type "bot_dm_bundle")
      (with-slots (channel ts thread-ts) msg
        (condition-case err
            (let* ((room (slack-room-find channel team))
                   (room-id (when room (oref room id)))
                   (room-name (if room
                                  (or (ignore-errors (slack-room-name room team))
                                      "name not available")
                                channel))
                   (location (format "%s%s"
                                     (if (and room (slack-channel-p room)) "#" "@")
                                     room-name))
                   (type-prefix (pcase type
                                  ((or "thread_reply" "thread_v2") "Thread in ")
                                  ((or "at_user" "at_user_group" "at_channel" "at_everyone")
                                   "Mentioned in ")
                                  ("internal_channel_invite" "Invited to ")
                                  (_ "")))
                   (context-header
                    (format "%s %s"
                            (if is-unread "*" " ")
                            (propertize (concat type-prefix location)
                                        'face 'slack-search-result-message-header-face
                                        'room-id (or room-id channel)
                                        'keymap slack-channel-button-keymap)))
                   (fetched-msg
                    (condition-case msg-err
                        (when (and room-id (or ts thread-ts))
                          (slack-message-get-or-fetch
                           ts room-id team thread-ts))
                      (error
                       (message "slack-activity-feed: Loading message failed: %S"
                                (error-message-string msg-err))
                       nil)))
                   (message-str
                    (concat (if fetched-msg
                                (slack-message-to-string fetched-msg team)
                              "TODO")
                            (when reaction
                              (concat "\n" (slack-activity-reaction-to-string
                                            reaction team))))))
              ;; Context header: no timestamp, no fill
              (let ((lui-time-stamp-position nil))
                (lui-insert context-header t))
              ;; Message: with timestamp, consistent with channel buffers
              (let ((lui-time-stamp-time time)
                    (lui-time-stamp-format "[%Y-%m-%d %H:%M] "))
                (lui-insert-with-text-properties
                 message-str
                 'ts ts
                 'team-id (oref team id)
                 'room-id (or room-id channel)
                 'thread-ts thread-ts))
              ;; Blank separator
              (let ((lui-time-stamp-position nil))
                (lui-insert "" t)))
          (error
           (let ((lui-time-stamp-position nil))
             (lui-insert (format "Error loading activity: %s"
                                 (error-message-string err))
                         t))))))))

(cl-defmethod slack-buffer-has-next-page-p ((this slack-activity-feed-buffer))
  "Tell if there is another page of results for THIS SLACK-ACTIVITY-FEED-BUFFER."
  (with-slots (activity-feed) this
    (oref activity-feed pagination)))

(cl-defmethod slack-buffer-insert-history ((this slack-activity-feed-buffer))
  (with-slots (activity-feed) this
    (let* ((cur-point (point))
           (activities (-drop (oref activity-feed last) (oref activity-feed activities))))
      (cl-loop for m in activities
               do (slack-buffer-insert this m))
      (goto-char cur-point))
    ))

(cl-defmethod slack-buffer-request-history ((this slack-activity-feed-buffer) after-success)
  (with-slots (activity-feed) this
    (slack-activity-feed-request
     (slack-buffer-team this)
     (lambda (data)
       (let ((new-activity-feed
              (make-instance
               'slack-activity-feed
               :activities
               (append
                (oref activity-feed activities)
                (mapcar #'slack-activity-feed--parse-item
                        (plist-get data :items)))
               :pagination (plist-get (plist-get data :response_metadata)
                                      :next_cursor)
               :last (- (length (oref activity-feed activities)) 1))))
         (oset this activity-feed new-activity-feed)
         (funcall after-success)))
     (oref activity-feed pagination))))

(cl-defmethod slack-buffer-init-buffer ((this slack-activity-feed-buffer))
  (let ((buffer (cl-call-next-method)))
    (with-current-buffer buffer
      (slack-activity-feed-buffer-mode)
      (slack-buffer-set-current-buffer this)
      (with-slots (activity-feed) this
        (let* ((activities (oref activity-feed activities)))
          (cl-loop for m in activities
                   do (slack-buffer-insert this m)))
        (let ((lui-time-stamp-position nil))
          (if (slack-buffer-has-next-page-p this)
              (slack-buffer-insert-load-more this)))))
    buffer))

(cl-defmethod slack-buffer-loading-message-end-point ((_this slack-activity-feed-buffer))
  (previous-single-property-change (point-max)
                                   'loading-message))

(cl-defmethod slack-buffer-delete-load-more-string ((this slack-activity-feed-buffer))
  (let* ((inhibit-read-only t)
         (loading-message-end
          (slack-buffer-loading-message-end-point this))
         (loading-message-start
          (previous-single-property-change loading-message-end
                                           'loading-message)))
    (delete-region loading-message-start
                   loading-message-end)))

(cl-defmethod slack-buffer-prepare-marker-for-history ((_this slack-activity-feed-buffer)))

(cl-defmethod slack-buffer-insert--history ((this slack-activity-feed-buffer))
  (slack-buffer-insert-history this)
  (if (slack-buffer-has-next-page-p this)
      (slack-buffer-insert-load-more this)
    (let ((lui-time-stamp-position nil))
      (lui-insert "(no more messages)\n" t))))

(defun slack-activity-feed-show ()
  "Show Slack activity feed."
  (interactive)
  (let ((team (slack-team-select)))
    (slack-activity-feed-request
     team
     (lambda (data)
       (let* ((activity-feed
               (make-instance
                'slack-activity-feed
                :activities (mapcar #'slack-activity-feed--parse-item
                                    (plist-get data :items))
                :pagination (plist-get (plist-get data :response_metadata)
                                       :next_cursor)))
              (buffer (slack-create-activity-feed-buffer activity-feed team)))
         (slack-buffer-display buffer))))))

(defun slack-activity-feed-open-message ()
  "Open message at point of activity-feed."
  (interactive)
  (if-let* ((ts (get-text-property (point) 'ts))
            (team-id (get-text-property (point) 'team-id))
            (room-id (get-text-property (point) 'room-id))
            (team (slack-team-find team-id)))
      (let ((thread-ts (get-text-property (point) 'thread-ts)))
        (slack-open-message
         team
         (slack-room-find room-id team)
         ts
         thread-ts))
    (error "Not possible to jump to message")))
(defun slack-activity-feed-goto-next ()
  "Move point to the next activity entry."
  (interactive)
  (let ((cur-ts (get-text-property (point) 'ts)))
    ;; First skip past current ts region (or nil region)
    (if-let ((pos (next-single-property-change (point) 'ts)))
        (if (get-text-property pos 'ts)
            (goto-char pos)
          ;; Landed on a nil gap (separator/header), skip to next ts
          (if-let ((pos2 (next-single-property-change pos 'ts)))
              (goto-char pos2)
            (message "You are on the last message.")))
      (message "You are on the last message."))))

(defun slack-activity-feed-goto-prev ()
  "Move point to the previous activity entry."
  (interactive)
  (let ((cur-ts (get-text-property (point) 'ts)))
    (if-let ((pos (previous-single-property-change (point) 'ts)))
        (if (get-text-property pos 'ts)
            ;; Find the start of this ts region
            (goto-char (or (previous-single-property-change pos 'ts) (point-min)))
          ;; Landed on a nil gap, skip back to previous ts
          (if-let ((pos2 (previous-single-property-change pos 'ts)))
              (goto-char (or (previous-single-property-change pos2 'ts) (point-min)))
            (message "You are on the first message.")))
      (message "You are on the first message."))))

(define-key slack-activity-feed-buffer-mode-map (kbd "RET") 'slack-activity-feed-open-message)
(define-key slack-activity-feed-buffer-mode-map (kbd "n") 'slack-activity-feed-goto-next)
(define-key slack-activity-feed-buffer-mode-map (kbd "p") 'slack-activity-feed-goto-prev)

(provide 'slack-activity-feed-buffer)
;;; slack-activity-feed-buffer.el ends here
