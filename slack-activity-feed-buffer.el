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
(require 'slack-room-buffer)
(require 'slack-search)
(require 'slack-room)
(require 'slack-message-buffer)
(require 'slack-team)
(require 'dash)
(require 'seq)
(require 's)

(declare-function slack-conversations-history "slack-conversations")
(declare-function slack-conversations-replies "slack-conversations")
(declare-function slack-conversations-info "slack-conversations")
(declare-function slack-conversations-mark "slack-conversations")
(declare-function slack-message-create "slack-create-message")
(declare-function slack-select-token "slack-request")
(declare-function slack-message-replace-buffer "slack-message-buffer")
(declare-function slack-emoji-resolve "slack-emoji")

(defun slack-team-ensure-registered (team)
  "Ensure TEAM is the canonical object in the global lookup tables.
Fixes the case where `slack-team-find' would return a stale team
object that lacks rooms or has an unbound id slot."
  (condition-case nil
      (let ((id (oref team id))
            (token (oref team token)))
        (when (and id token)
          (puthash token team slack-teams-by-token)
          (puthash id token slack-tokens-by-id)))
    (error nil)))

(defvar slack-activity-feed-url "https://slack.com/api/activity.feed")
(defvar slack-activity-feed-mark-read-url "https://slack.com/api/activity.markRead")
(defvar slack-activity-feed-mode-show-only-unread nil "If non-nil, show only unread activity.")
(defcustom slack-activity-feed-watch-channels nil
  "List of channels whose recent messages are included in Activity.
Each entry may be a channel ID such as \"C123\" or a channel name
without its leading #."
  :type '(repeat string)
  :group 'slack)
(defcustom slack-activity-feed-watch-channel-limit 50
  "Maximum number of recent messages fetched per watched channel."
  :type 'integer
  :group 'slack)

(defcustom slack-activity-feed-render-batch-size 5
  "Number of Activity Feed rows to render per incremental batch."
  :type 'integer
  :group 'slack)

(defvar slack-activity-feed--cache (make-hash-table :test 'equal)
  "Per-team Activity Feed snapshots.")

(defconst slack-activity-feed-multipart-boundary "----WebKitFormBoundaryh7x3DqJqAIvkEcie")

(defun slack-activity-feed--watch-channel-limit ()
  "Return the watched-channel history limit as a Slack API string."
  (if (numberp slack-activity-feed-watch-channel-limit)
      (number-to-string slack-activity-feed-watch-channel-limit)
    slack-activity-feed-watch-channel-limit))

(defun slack-activity-feed-toggle-unread ()
  "Toggle between showing all activity and only unread, then refresh."
  (interactive)
  (setq slack-activity-feed-mode-show-only-unread
        (not slack-activity-feed-mode-show-only-unread))
  (message (if slack-activity-feed-mode-show-only-unread
               "Showing unread only..."
             "Showing all activity..."))
  (slack-activity-feed-show))

(defun slack-activity-feed--jbool (jf)
  "Return nil if JF is JSON false, t otherwise."
  (not (eq jf :json-false)))

(defun slack-activity-feed--request-data (token mode &optional cursor)
  "Build multipart form data for the activity feed request.
TOKEN is the token argument.
MODE is the mode argument."
  (let ((fields (delq nil
                      (list (cons "token" token)
                            (cons "limit" "20")
                            (cons "types" "thread_v2,dm,generic_system_alert,message_reaction,internal_channel_invite,list_record_edited,bot_dm_bundle,at_user,at_user_group,at_channel,at_everyone,keyword,list_record_assigned,list_user_mentioned,external_channel_invite,shared_workspace_invite,external_dm_invite")
                            (cons "mode" mode)
                            (and cursor (cons "cursor" cursor))
                            (cons "_x_reason" "fetchActivityFeed")
                            (cons "_x_mode" "online")
                            (cons "_x_sonic" "true")
                            (cons "_x_app_name" "client")))))
    (concat
     (mapconcat #'(lambda (field)
                    (format "--%s\r\nContent-Disposition: form-data; name=\"%s\"\r\n\r\n%s\r\n"
                            slack-activity-feed-multipart-boundary
                            (car field)
                            (cdr field)))
                fields
                "")
     (format "--%s--\r\n" slack-activity-feed-multipart-boundary))))

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
     :feed-key (when-let ((k (plist-get item-data :key))) (format "%s" k))
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
                      :author-id (when-let ((id (or (plist-get m :author_user_id)
                                                    (plist-get bundle-msg :author_user_id))))
                                   (format "%s" id)))
            :reaction (when r (make-instance
                               'activity-reaction
                               :user (format "%s" (plist-get r :user))
                               :name (format "%s" (plist-get r :name))))))))

(defun slack-activity-feed--cached-message (ts room)
  "Return cached message TS from ROOM, or nil."
  (when (and room ts)
    (condition-case nil
        (slack-room-find-message room ts)
      (error nil))))

(defun slack-activity-feed--find-message (ts messages)
  "Return message TS from MESSAGES, or nil."
  (cl-find-if
   (lambda (message)
     (equal (slack-ts message) ts))
   messages))

(defun slack-activity-feed--prefetch-messages (activities team callback
                                                          &optional messages-callback
                                                          unavailable-callback)
  "Prefetch uncached messages for ACTIVITIES in parallel, then call CALLBACK.
Fires async API requests for all messages not already in cache,
and invokes CALLBACK (with no arguments) once every request has
completed (or immediately if all messages are cached).
When MESSAGES-CALLBACK is non-nil, call it with ROOM and the fetched
messages after each successful fetch.
When UNAVAILABLE-CALLBACK is non-nil, call it with the activity whose
message could not be fetched.
TEAM is the team argument."
  (let* ((pending (list 0))   ; boxed counter for mutation in closures
         (items
          (cl-loop
           for activity in activities
           for item = (oref activity item)
           collect (let ((msg (oref item message)))
                     (list activity
                           (oref msg source-message)
                           (oref msg ts)
                           (oref msg channel)
                           (oref msg thread-ts))))))
    ;; Count how many need fetching
    (dolist (entry items)
      (let* ((source-message (nth 1 entry))
             (ts (nth 2 entry))
             (channel (nth 3 entry))
             (room (slack-room-find channel team)))
        (when (and (not source-message)
                   room
                   (not (equal ts "0"))
                   (not (equal channel "unknown"))
                   (not (slack-activity-feed--cached-message ts room)))
          (cl-incf (car pending)))))
    (if (= 0 (car pending))
        ;; All cached — render immediately
        (funcall callback)
      ;; Fire parallel async requests
      (dolist (entry items)
        (let* ((activity (nth 0 entry))
               (source-message (nth 1 entry))
               (ts (nth 2 entry))
               (channel (nth 3 entry))
               (thread-ts (nth 4 entry))
               (room (slack-room-find channel team)))
          (when (and (not source-message)
                     room
                     (not (equal ts "0"))
                     (not (equal channel "unknown"))
                     (not (slack-activity-feed--cached-message ts room)))
            (let ((is-reply (and thread-ts
                                 (not (string-equal ts thread-ts)))))
              (condition-case err
                  (if is-reply
                      (slack-conversations-replies
                       room thread-ts team
                       :oldest ts
                       :inclusive "true"
                       :limit "1"
                       :after-success
                       (lambda (messages &rest _)
                         (if (slack-activity-feed--find-message ts messages)
                             (let ((matching-messages
                                    (list (slack-activity-feed--find-message
                                           ts messages))))
                               (slack-room-set-messages
                                room matching-messages team)
                               (when (functionp messages-callback)
                                 (funcall messages-callback
                                          room matching-messages)))
                           (when (functionp unavailable-callback)
                             (funcall unavailable-callback activity)))
                         (when (= 0 (cl-decf (car pending)))
                           (funcall callback)))
                       :on-error
                       (lambda (&rest _)
                         (when (functionp unavailable-callback)
                           (funcall unavailable-callback activity))
                         (when (= 0 (cl-decf (car pending)))
                           (funcall callback))))
                    (slack-conversations-history
                     room team
                     :latest ts
                     :inclusive "true"
                     :limit "1"
                     :after-success
                     (lambda (messages &rest _)
                       (if (slack-activity-feed--find-message ts messages)
                           (let ((matching-messages
                                  (list (slack-activity-feed--find-message
                                         ts messages))))
                             (slack-room-set-messages
                              room matching-messages team)
                             (when (functionp messages-callback)
                               (funcall messages-callback
                                        room matching-messages)))
                         (when (functionp unavailable-callback)
                           (funcall unavailable-callback activity)))
                       (when (= 0 (cl-decf (car pending)))
                         (funcall callback)))
                     :on-error
                     (lambda (&rest _)
                       (when (functionp unavailable-callback)
                         (funcall unavailable-callback activity))
                       (when (= 0 (cl-decf (car pending)))
                         (funcall callback)))))
                (error
                 (message "slack-activity-feed: prefetch error for %s: %S" ts err)
                 (when (functionp unavailable-callback)
                   (funcall unavailable-callback activity))
                 (when (= 0 (cl-decf (car pending)))
                   (funcall callback)))))))))))

(defun slack-activity-feed--prefetch-rooms (activities team callback)
  "Prefetch uncached rooms for ACTIVITIES in TEAM, then call CALLBACK.
Fire async `slack-conversations-info' requests for channel IDs not
in the local cache.  CALLBACK is invoked with no arguments once
every request completes, or immediately when all rooms are cached."
  (let* ((pending (list 0))
         (seen (make-hash-table :test #'equal))
         (missing-ids
          (cl-loop
           for activity in activities
           for item = (oref activity item)
           for channel = (oref (oref item message) channel)
           unless (or (null channel)
                      (gethash channel seen)
                      (equal channel "unknown")
                      (slack-room-find channel team))
           collect (progn (puthash channel t seen) channel))))
    (if (null missing-ids)
        (funcall callback)
      (setcar pending (length missing-ids))
      (dolist (channel-id missing-ids)
        (condition-case err
            (slack-conversations-info
             channel-id team
             (lambda ()
               (when (= 0 (cl-decf (car pending)))
                 (funcall callback)))
             (lambda (&rest _)
               (when (= 0 (cl-decf (car pending)))
                 (funcall callback))))
          (error
           (message "slack-activity-feed: room prefetch error for %s: %S"
                    channel-id err)
           (when (= 0 (cl-decf (car pending)))
             (funcall callback))))))))

(defun slack-activity-feed--watched-room (channel team)
  "Return the watched room named or identified by CHANNEL in TEAM."
  (or (slack-room-find channel team)
      (cl-loop for room being the hash-values of (oref team channels)
               when (string= channel (slack-room-name room team))
               return room)))

(defun slack-activity-feed--watched-rooms (team)
  "Return the configured Activity watch rooms for TEAM."
  (delq nil
        (delete-dups
         (mapcar (lambda (channel)
                   (slack-activity-feed--watched-room channel team))
                 slack-activity-feed-watch-channels))))

(defun slack-activity-feed--watched-room-p (room team)
  "Return non-nil when ROOM is watched for Activity in TEAM."
  (memq room (slack-activity-feed--watched-rooms team)))

(defun slack-activity-feed--message-unread-p (message room)
  "Return non-nil when MESSAGE is newer than ROOM's last-read marker."
  (let ((last-read (oref room last-read))
        (ts (slack-ts message)))
    (or (string= "0" last-read)
        (string< last-read ts))))

(defun slack-activity-feed-watch-channel-message (message room team)
  "Update Activity unread state for a watched-channel MESSAGE.
ROOM is the room containing MESSAGE, and TEAM is the Slack team."
  (when (and (slack-activity-feed--watched-room-p room team)
             (slack-activity-feed--message-unread-p message room))
    (setq slack-has-unreads t
          slack-unread-count (1+ (or slack-unread-count 0)))
    (slack-activity-feed--cache-update-activity
     team (slack-activity-feed--message-activity message room))
    (force-mode-line-update)))

(defun slack-activity-feed--message-activity (message room)
  "Return an Activity entry for MESSAGE from ROOM."
  (let ((ts (slack-ts message)))
    (make-instance
     'slack-activity
     :is-unread (slack-activity-feed--message-unread-p message room)
     :feed-ts ts
     :item (make-instance
            'activity-item
            :type "channel_message"
            :message (make-instance
                      'activity-message
                      :ts ts
                      :channel (oref room id)
                      :is-broadcast nil
                      :thread-ts (and (slot-boundp message 'thread-ts)
                                      (oref message thread-ts))
                      :author-id (slack-message-sender-id message)
                      :source-message message)
            :reaction nil))))

(defun slack-activity-feed--fetch-watched-activities (team callback)
  "Fetch watched-channel Activity entries for TEAM, then call CALLBACK.
CALLBACK receives a list of `slack-activity' objects."
  (let* ((rooms (slack-activity-feed--watched-rooms team))
         (pending (list (length rooms)))
         (activities nil))
    (if (null rooms)
        (funcall callback nil)
      (cl-labels
          ((finish ()
                   (when (= 0 (cl-decf (car pending)))
                     (funcall callback activities)))
           (fetch-history
            (room)
            (slack-conversations-history
             room team
             :limit (slack-activity-feed--watch-channel-limit)
             :after-success
             (lambda (messages &rest _)
               (slack-room-set-messages room messages team)
               (setq activities
                     (nconc activities
                            (mapcar (lambda (message)
                                      (slack-activity-feed--message-activity
                                       message room))
                                    messages)))
               (finish))
             :on-error
             (lambda (&rest _)
               (finish)))))
        (dolist (room rooms)
          (slack-conversations-info
           (oref room id)
           team
           (lambda ()
             (fetch-history room))
           (lambda (&rest _)
             (fetch-history room))))))))

(defun slack-activity-feed--activity-key (activity)
  "Return the deduplication key for ACTIVITY."
  (let ((message (oref (oref activity item) message)))
    (cons (oref message channel)
          (oref message ts))))

(defun slack-activity-feed--merge-activities (activities extra-activities)
  "Merge ACTIVITIES and EXTRA-ACTIVITIES, newest first."
  (let ((seen (make-hash-table :test 'equal))
        (merged (copy-sequence activities)))
    (dolist (activity activities)
      (puthash (slack-activity-feed--activity-key activity) t seen))
    (dolist (activity extra-activities)
      (let ((key (slack-activity-feed--activity-key activity)))
        (unless (gethash key seen)
          (puthash key t seen)
          (push activity merged))))
    (sort merged
          (lambda (a b)
            (> (string-to-number (oref a feed-ts))
               (string-to-number (oref b feed-ts)))))))

(defun slack-activity-feed--with-watched-activities (activities team callback)
  "Call CALLBACK with ACTIVITIES plus watched-channel entries for TEAM."
  (slack-activity-feed--fetch-watched-activities
   team
   (lambda (extra-activities)
     (funcall callback
              (slack-activity-feed--merge-activities
               activities extra-activities)))))

(defun slack-activity-feed--cache-key (team)
  "Return the Activity Feed cache key for TEAM and the current feed mode."
  (list (oref team id) slack-activity-feed-mode-show-only-unread))

(defun slack-activity-feed--cache-get (team)
  "Return TEAM's cached Activity Feed snapshot."
  (gethash (slack-activity-feed--cache-key team)
           slack-activity-feed--cache))

(defun slack-activity-feed--cache-put (team activities pagination)
  "Cache ACTIVITIES and PAGINATION for TEAM."
  (slack-activity-feed--cache-put-key
   (slack-activity-feed--cache-key team)
   activities
   pagination))

(defun slack-activity-feed--cache-put-key (key activities pagination)
  "Cache ACTIVITIES and PAGINATION under Activity Feed cache KEY."
  (let ((snapshot (list :activities activities
                        :pagination pagination
                        :updated-at (current-time))))
    (puthash key snapshot slack-activity-feed--cache)
    snapshot))

(defun slack-activity-feed--cache-keys-for-team (team)
  "Return Activity Feed cache keys currently stored for TEAM."
  (let ((team-id (oref team id))
        keys)
    (maphash
     (lambda (key _snapshot)
       (when (equal (car key) team-id)
         (push key keys)))
     slack-activity-feed--cache)
    keys))

(defun slack-activity-feed--activity-keys (activities)
  "Return stable identity keys for ACTIVITIES."
  (mapcar #'slack-activity-feed--activity-key activities))

(defun slack-activity-feed--snapshot-changed-p (old new)
  "Return non-nil when Activity Feed snapshot OLD differs from NEW."
  (not (equal (and old (slack-activity-feed--activity-keys
                       (plist-get old :activities)))
              (and new (slack-activity-feed--activity-keys
                       (plist-get new :activities))))))

(defun slack-activity-feed--visible-p (team)
  "Return non-nil when TEAM's Activity Feed buffer is visible."
  (slack-if-let* ((buffer (slack-buffer-find 'slack-activity-feed-buffer team))
                  (buf (and (slot-boundp buffer 'buf) (oref buffer buf))))
      (get-buffer-window buf t)))

(defun slack-activity-feed--selected-team ()
  "Return the Activity Feed buffer team or select the current Slack team."
  (if (and (boundp 'slack-current-buffer)
           slack-current-buffer
           (object-of-class-p slack-current-buffer
                              'slack-activity-feed-buffer))
      (slack-buffer-team slack-current-buffer)
    (slack-team-select)))

(defun slack-activity-feed--refresh-cache (team &optional after-refresh quiet)
  "Refresh TEAM's Activity Feed cache without changing visible buffers.
Call AFTER-REFRESH with the old and new snapshots when done.  If
QUIET is nil, notify when a visible Activity Feed has newer cached
content."
  (let ((old-snapshot (slack-activity-feed--cache-get team)))
    (slack-activity-feed-request
     team
     (lambda (data)
       (let ((activities (mapcar #'slack-activity-feed--parse-item
                                 (plist-get data :items)))
             (pagination (plist-get (plist-get data :response_metadata)
                                    :next_cursor)))
         (slack-activity-feed--fetch-watched-activities
          team
          (lambda (extra-activities)
            (let ((new-snapshot
                   (slack-activity-feed--cache-put
                    team
                    (slack-activity-feed--merge-activities
                     activities extra-activities)
                    pagination)))
              (when (and (not quiet)
                         (slack-activity-feed--visible-p team)
                         (slack-activity-feed--snapshot-changed-p
                          old-snapshot new-snapshot))
                (message
                 "Activity feed has newer cached results; press g to refresh."))
              (when after-refresh
                (funcall after-refresh old-snapshot new-snapshot))))))))))

(defun slack-activity-feed--cache-update-activity (team activity)
  "Merge ACTIVITY into any existing Activity Feed cache snapshots for TEAM."
  (let ((changed nil))
    (dolist (key (slack-activity-feed--cache-keys-for-team team))
      (let* ((snapshot (gethash key slack-activity-feed--cache))
             (old-activities (plist-get snapshot :activities)))
        (unless (and (cadr key) (not (oref activity is-unread)))
          (let ((new-activities
                 (slack-activity-feed--merge-activities
                  (list activity) old-activities)))
            (unless (equal (slack-activity-feed--activity-keys old-activities)
                           (slack-activity-feed--activity-keys new-activities))
              (slack-activity-feed--cache-put-key
               key new-activities (plist-get snapshot :pagination))
              (setq changed t))))))
    (when (and changed (slack-activity-feed--visible-p team))
      (message "Activity feed has newer cached results; press g to refresh."))
    changed))

(defun slack-activity-feed--cache-merge-activities (team activities)
  "Merge ACTIVITIES into existing Activity Feed cache snapshots for TEAM."
  (let ((changed nil))
    (dolist (key (slack-activity-feed--cache-keys-for-team team))
      (let* ((snapshot (gethash key slack-activity-feed--cache))
             (old-activities (plist-get snapshot :activities))
             (new-activities
              (slack-activity-feed--merge-activities
               activities old-activities)))
        (unless (equal (slack-activity-feed--activity-keys old-activities)
                       (slack-activity-feed--activity-keys new-activities))
          (slack-activity-feed--cache-put-key
           key new-activities (plist-get snapshot :pagination))
          (setq changed t))))
    (when (and changed (slack-activity-feed--visible-p team))
      (message "Activity feed has newer cached results; press g to refresh."))
    changed))

(defun slack-activity-feed-refresh-cache-from-event (team)
  "Refresh existing Activity Feed cache snapshots for TEAM after an event.
This updates cache data only; it does not redraw visible Activity Feed buffers."
  (dolist (key (slack-activity-feed--cache-keys-for-team team))
    (let ((slack-activity-feed-mode-show-only-unread (cadr key)))
      (slack-activity-feed--refresh-cache team))))

(defun slack-activity-feed-request (team &optional after-success cursor)
  "Request activity feed for CHANNEL-ID of TEAM.
Run an action on the data returned with AFTER-SUCCESS.
CURSOR is the cursor argument."
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
      :data (let ((token (slack-select-token slack-activity-feed-url team))
                  (mode (if slack-activity-feed-mode-show-only-unread "priority_unreads_v1" "chrono_reads_and_unreads")))
              (slack-activity-feed--request-data token mode cursor))
      :headers (list
                (cons "content-type"
                      (format "multipart/form-data; boundary=%s"
                              slack-activity-feed-multipart-boundary)))))))

(defclass slack-activity-feed ()
  ((activities :initarg :activities :initform nil :type (or null list))
   (pagination :initarg :pagination :type (or null string))
   (last :initarg :last :type (or null integer))))

(defun slack-activity-feed--prepare-buffer ()
  "Apply Activity Feed display invariants in the current buffer."
  (setq-local lui-max-buffer-size nil))

(define-derived-mode slack-activity-feed-buffer-mode slack-buffer-mode "Slack Activity Feed"
  "Major mode for the Slack activity feed.

Message-region bindings (active when point is on an activity entry):
\\{slack-message-keymap}
Buffer-wide bindings:
\\{slack-activity-feed-buffer-mode-map}"
  (add-hook 'lui-pre-output-hook 'slack-mrkdwn-add-face nil t)
  (add-hook 'lui-pre-output-hook 'slack-display-inline-action t t)
  (add-hook 'post-command-hook #'slack-buffer--maybe-load-more-at-end nil t)
  (setq-local revert-buffer-function
              (lambda (_ignore-auto _noconfirm)
                (slack-activity-feed-refresh)))
  (cursor-sensor-mode)
  (slack-activity-feed--prepare-buffer))

(defclass slack-activity-feed-buffer (slack-room-buffer)
  ((activity-feed :initarg :activity-feed :type slack-activity-feed)
   (cached-team :initarg :cached-team :initform nil)
   (render-timer :initarg :render-timer :initform nil)))

(cl-defmethod slack-buffer-team ((this slack-activity-feed-buffer))
  "Return the team for THIS buffer, preferring the cached reference."
  (or (oref this cached-team) (cl-call-next-method)))

(cl-defmethod slack-buffer-room ((this slack-activity-feed-buffer))
  "Return the room for THIS message at point.
Unlike single-room buffers, the activity feed shows messages from
many rooms.  The room is resolved from text properties at or near
point.  As a last resort, the TS at point is matched against the
cached activity list to recover the channel."
  (let* ((team (slack-buffer-team this))
         (room-id (or (slack-activity-feed--room-id-at (point))
                      (slack-activity-feed--room-id-from-ts
                       this (get-text-property (point) 'ts)))))
    (when room-id
      (slack-room-find room-id team))))

(defun slack-activity-feed--room-id-at (pos)
  "Return room-id from text properties at or near POS.
Checks POS itself, the current line, then searches backward and
forward by property boundaries."
  (or (get-text-property pos 'room-id)
      (save-excursion
        (goto-char pos)
        (cl-loop for i from (pos-bol) to (pos-eol)
                 for rid = (get-text-property i 'room-id)
                 if rid return rid))
      (let ((prev (previous-single-property-change pos 'room-id)))
        (and prev (> prev (point-min))
             (get-text-property (1- prev) 'room-id)))
      (let ((next (next-single-property-change pos 'room-id)))
        (and next (get-text-property next 'room-id)))))

(defun slack-activity-feed--room-id-from-ts (buffer ts)
  "Look up the channel for TS from BUFFER's cached activity list.
Falls back to the activity feed data structure when text
properties are unreliable."
  (when ts
    (cl-loop for activity in (oref (oref buffer activity-feed) activities)
             for msg = (oref (oref activity item) message)
             when (equal (oref msg ts) ts)
             return (oref msg channel))))

(defun slack-activity-feed--activity-from-ts (buffer ts)
  "Return the activity in BUFFER whose message timestamp is TS."
  (cl-loop for activity in (oref (oref buffer activity-feed) activities)
           for msg = (oref (oref activity item) message)
           when (equal (oref msg ts) ts)
           return activity))

(cl-defmethod slack-buffer-name ((_class (subclass slack-activity-feed-buffer)) team)
  "Return the display buffer name for the activity feed buffer.
TEAM is the team argument."
  (format "*slack: %s Activity Feed*"
          (oref team name)))

(cl-defmethod slack-buffer-name ((this slack-activity-feed-buffer))
  "Return the display buffer name for THIS buffer."
  (format "*slack: %s Activity Feed*"
          (slack-team-name (slack-buffer-team this))))

(cl-defmethod slack-buffer-key ((_class (subclass slack-activity-feed-buffer)))
  "Return the class-level buffer key for the activity feed buffer."
  "activity feed")

(cl-defmethod slack-buffer-key ((_this slack-activity-feed-buffer))
  "Return the lookup key identifying the buffer for the activity feed buffer."
  (slack-buffer-key 'slack-activity-feed-buffer))

(cl-defmethod slack-team-buffer-key ((_class (subclass slack-activity-feed-buffer)))
  "Return the team-scoped class-level buffer key for the activity feed buffer."
  'slack-activity-feed-buffer)

(defun slack-activity-feed--cancel-render-timer (buffer)
  "Cancel BUFFER's pending incremental render timer."
  (when-let ((timer (and (slot-boundp buffer 'render-timer)
                         (oref buffer render-timer))))
    (cancel-timer timer)
    (oset buffer render-timer nil)))

(defun slack-activity-feed--insert-activity-batch (buffer activities)
  "Insert ACTIVITIES into BUFFER's live Emacs buffer."
  (when activities
    (with-current-buffer (slack-buffer-buffer buffer)
      (slack-buffer-with-deferred-hooks
        (dolist (activity activities)
          (slack-buffer-insert buffer activity))))))

(defun slack-activity-feed--render-activities (buffer activities)
  "Render ACTIVITIES into BUFFER incrementally."
  (slack-activity-feed--cancel-render-timer buffer)
  (let* ((batch-size (max 1 slack-activity-feed-render-batch-size))
         (initial (seq-take activities batch-size))
         (remaining (nthcdr (length initial) activities)))
    (slack-activity-feed--insert-activity-batch buffer initial)
    (when remaining
      (cl-labels
          ((render-next
            ()
            (let* ((batch (seq-take remaining batch-size))
                   (batch-length (length batch)))
              (setq remaining (nthcdr batch-length remaining))
              (slack-activity-feed--insert-activity-batch buffer batch)
              (if remaining
                  (oset buffer render-timer
                        (run-at-time 0.01 nil #'render-next))
                (oset buffer render-timer nil)))))
        (oset buffer render-timer
              (run-at-time 0.01 nil #'render-next))))))

(defun slack-create-activity-feed-buffer (activity-feed team)
  "Create and return a new activity feed buffer instance from PAYLOAD.
ACTIVITY-FEED is the activity-feed argument.
TEAM is the team argument."
  (slack-team-ensure-registered team)
  (let ((existing (slack-buffer-find 'slack-activity-feed-buffer team)))
    (if (and existing
             (buffer-live-p (oref existing buf)))
        (progn
          (slack-activity-feed--cancel-render-timer existing)
          (oset existing activity-feed activity-feed)
          (oset existing cached-team team)
          (with-current-buffer (oref existing buf)
            (slack-activity-feed--prepare-buffer)
            (let ((inhibit-read-only t))
              (erase-buffer))
            (when (markerp lui-output-marker)
              (set-marker lui-output-marker (point-max)))
            (when (markerp lui-input-marker)
              (set-marker lui-input-marker (point-max)))
            (lui-set-prompt " "))
          (slack-activity-feed--render-activities
           existing
           (oref activity-feed activities))
          existing)
      (make-instance 'slack-activity-feed-buffer
                     :team-id (oref team id)
                     :room-id "__activity-feed__"
                     :cached-team team
                     :activity-feed activity-feed))))

(defclass activity-message ()
  ((ts :initarg :ts :type string)
   (channel :initarg :channel :type string)
   (is-broadcast :initarg :is-broadcast :type boolean)
   (thread-ts :initarg :thread-ts :type (or null string))
   (author-id :initarg :author-id :type (or null string))
   (source-message :initarg :source-message :initform nil
                   :type (or null slack-message))
   (source-message-unavailable :initarg :source-message-unavailable
                               :initform nil
                               :type boolean)))

(cl-defmethod slack-activity-message-to-string ((this activity-message) team &optional activity-type)
  "Format THIS activity-message of TEAM as a string for presentation.
ACTIVITY-TYPE is the activity type string (e.g. \"thread_reply\")."
  (with-slots (channel ts is-broadcast thread-ts author-id source-message
                       source-message-unavailable) this
    (condition-case err ;; this is to find out more easily messages that we fail to handle
        (let* ((room (slack-room-find channel team))
               (room-name (or (condition-case err
                                  (slack-room-name room team)
                                (error
                                 (message "slack-activity: room name lookup failed: %S" err)
                                 nil))
                              "name not available - try to update channel list"))
               (location (format "%s%s"
                                 (if (slack-channel-p room) "#" "@")
                                 room-name))
               (type-prefix (pcase activity-type
                              ((or "thread_reply" "thread_v2") "Thread in ")
                              ((or "at_user" "at_user_group" "at_channel" "at_everyone")
                               "Mentioned in ")
                              ("internal_channel_invite" "Invited to ")
                              ((or "external_channel_invite" "shared_workspace_invite")
                               "Invited to ")
                              ("external_dm_invite" "DM invite from ")
                              ("message_reaction" "Reacted in ")
                              ("keyword" "Keyword match in ")
                              ("generic_system_alert" "Alert in ")
                              (_ "")))
               (context-header (propertize (concat type-prefix location)
                                           'face 'slack-search-result-message-header-face))
               (fetched-msg
                (or source-message
                    (slack-activity-feed--cached-message ts room))))
          (propertize (concat context-header "\n"
                              (if fetched-msg
                                  (slack-message-to-string fetched-msg team)
                                (if source-message-unavailable
                                    "Message unavailable."
                                  "Loading message...")))
                      'ts ts
                      'team-id (oref team id)
                      'room-id (oref room id)
                      'thread-ts thread-ts))
      (error
       (format "Error loading activity. Please report this message at https://github.com/emacs-slack/emacs-slack/issues:\n%s"
               (list this err))))))

(defclass activity-reaction ()
  ((user :initarg :user :type string)
   (name :initarg :name :type string)))

(cl-defmethod slack-activity-reaction-to-string ((this activity-reaction) team)
  "Format THIS activity reaction for TEAM as a display string.
Eagerly resolves the emoji shortcode to Unicode when available."
  (with-slots (user name) this
    (format "  %s reacted with %s"
            (or (slack-user-name user team) user)
            (slack-emoji-resolve name))))

(defclass activity-item ()
  ((type :initarg :type :type string)
   (message :initarg :message :type activity-message)
   (reaction :initarg :reaction :type (or null activity-reaction))))

(cl-defmethod slack-activity-item-to-string ((this activity-item) team)
  "Convert THIS activity for TEAM into a string."
  (with-slots (type message reaction) this
    (concat
     (slack-activity-message-to-string message team type)
     (when reaction (concat "\n" (slack-activity-reaction-to-string reaction team))))))

(defclass slack-activity ()
  ((is-unread :initarg :is-unread :type boolean)
   (feed-ts :initarg :feed-ts :type string)
   (feed-key :initarg :feed-key :initform nil :type (or null string))
   (item :initarg :item :type activity-item)))

(defun slack-activity-feed--activity-message-string (activity team)
  "Return ACTIVITY's message body string for TEAM."
  (let* ((item (oref activity item))
         (msg (oref item message))
         (reaction (oref item reaction)))
    (with-slots (channel ts source-message source-message-unavailable) msg
      (let* ((room (slack-room-find channel team))
             (fetched-msg
              (or source-message
                  (slack-activity-feed--cached-message ts room))))
        (concat (if fetched-msg
                    (slack-message-to-string fetched-msg team)
                  (if source-message-unavailable
                      "Message unavailable."
                    "Loading message..."))
                (when reaction
                  (concat "\n" (slack-activity-reaction-to-string
                                reaction team))))))))

(cl-defmethod slack-activity-to-string ((this slack-activity) team)
  "Render activity THIS on TEAM as a single-line string with an unread marker."
  (with-slots (is-unread item) this
    (format "%s %s" (if is-unread "*" " ") (slack-activity-item-to-string item team))))

(cl-defmethod slack-buffer--replace ((this slack-activity-feed-buffer) ts)
  "Replace the message with TS in THIS buffer.
Resolves the room from text properties at the TS position,
falling back to nearby positions if needed."
  (slack-if-let* ((team (slack-buffer-team this))
                  (buf (slack-buffer-buffer this)))
    (with-current-buffer buf
      (let ((pos (text-property-any (point-min) (point-max) 'ts ts)))
        (when pos
          (slack-if-let* ((room-id (slack-activity-feed--room-id-at pos))
                          (room (slack-room-find room-id team))
                          (message (slack-room-find-message room ts)))
              (slack-buffer-replace this message)))))))

(cl-defmethod slack-buffer-replace ((this slack-activity-feed-buffer) message)
  "Replace the rendered activity row body for MESSAGE in THIS buffer."
  (let* ((ts (slack-ts message))
         (activity
          (cl-loop for activity in (oref (oref this activity-feed) activities)
                   for msg = (oref (oref activity item) message)
                   when (equal (oref msg ts) ts)
                   return activity)))
    (when activity
      (let ((activity-message (oref (oref activity item) message)))
        (oset activity-message source-message message)
        (oset activity-message source-message-unavailable nil)
        (slack-activity-feed--replace-activity this activity)))))

(defun slack-activity-feed--replace-activity (feed-buffer activity)
  "Replace ACTIVITY's rendered row in FEED-BUFFER."
  (let* ((team (slack-buffer-team feed-buffer))
         (item (oref activity item))
         (activity-message (oref item message))
         (ts (oref activity-message ts))
         (room (slack-room-find (oref activity-message channel) team))
         (room-id (when room (oref room id)))
         (rendered
          (slack-buffer--render-native-emoji-string
           (slack-activity-feed--activity-message-string activity team))))
    (with-current-buffer (slack-buffer-buffer feed-buffer)
      (let ((inhibit-read-only t))
        (lui-replace
         (slack-buffer--apply-message-keymap rendered)
         (lambda ()
           (equal (get-text-property (point) 'ts) ts)))
        (let ((beg (text-property-any (point-min) (point-max) 'ts ts)))
          (when beg
            (let ((end (or (next-single-property-change beg 'ts)
                           (point-max))))
              (add-text-properties
               beg end
               (list 'ts ts
                     'team-id (oref feed-buffer team-id)
                     'room-id (or room-id (oref activity-message channel))
                     'thread-ts (oref activity-message thread-ts)
                     'activity-type (oref item type)
                     'activity-feed-ts (oref activity feed-ts)
                     'activity-feed-key (oref activity feed-key))))))))))

(cl-defmethod slack-message-replace-buffer :after ((this slack-message) team)
  "Also update the activity feed buffer when a message changes.
Resolves the room from the message's channel slot rather than
relying on buffer text properties."
  (slack-if-let* ((af-buffer (slack-buffer-find 'slack-activity-feed-buffer team))
                  (buf (and (slot-boundp af-buffer 'buf) (oref af-buffer buf)))
                  (live (buffer-live-p buf))
                  (channel (oref this channel))
                  (room (slack-room-find channel team))
                  (message (slack-room-find-message room (slack-ts this))))
      (with-current-buffer buf
        (slack-buffer-replace af-buffer message))))

(defun slack-activity-feed--replace-prefetched-messages (team messages)
  "Update TEAM's visible activity feed rows for fetched MESSAGES."
  (slack-if-let* ((af-buffer (slack-buffer-find 'slack-activity-feed-buffer team))
                  (buf (and (slot-boundp af-buffer 'buf) (oref af-buffer buf)))
                  (live (buffer-live-p buf)))
      (with-current-buffer buf
        (dolist (message messages)
          (slack-buffer-replace af-buffer message)))))

(defun slack-activity-feed--replace-unavailable-message (team activity)
  "Mark ACTIVITY's source message unavailable in TEAM's visible feed."
  (let ((activity-message (oref (oref activity item) message)))
    (oset activity-message source-message-unavailable t))
  (slack-if-let* ((af-buffer (slack-buffer-find 'slack-activity-feed-buffer team))
                  (buf (and (slot-boundp af-buffer 'buf) (oref af-buffer buf)))
                  (live (buffer-live-p buf)))
      (with-current-buffer buf
        (slack-activity-feed--replace-activity af-buffer activity))))

(defun slack-activity-feed--find-activity (feed-buffer feed-ts)
  "Return FEED-BUFFER's activity with FEED-TS, or nil."
  (when (and feed-buffer
             feed-ts
             (slot-boundp feed-buffer 'activity-feed))
    (cl-loop for activity in (oref (oref feed-buffer activity-feed) activities)
             when (equal (oref activity feed-ts) feed-ts)
             return activity)))

(defun slack-activity-feed--decrement-unread-summary ()
  "Decrement the local Activity unread summary after one item is read."
  (when (and (boundp 'slack-unread-count)
             (numberp slack-unread-count)
             (< 0 slack-unread-count))
    (setq slack-unread-count (1- slack-unread-count)))
  (when (and (boundp 'slack-has-unreads)
             (numberp slack-unread-count)
             (= 0 slack-unread-count))
    (setq slack-has-unreads nil))
  (force-mode-line-update))

(defun slack-activity-feed--cache-mark-read (team feed-ts)
  "Mark FEED-TS read in TEAM's Activity Feed cache.
Return non-nil when at least one cached snapshot changed.  Normal
snapshots keep the activity and clear its unread state; unread-only
snapshots remove the activity so reopening that view does not show a
read item."
  (let ((changed nil))
    (when (and team feed-ts)
      (dolist (key (slack-activity-feed--cache-keys-for-team team))
        (let* ((snapshot (gethash key slack-activity-feed--cache))
               (activities (plist-get snapshot :activities)))
          (if (cadr key)
              (let ((new-activities
                     (cl-remove-if
                      (lambda (activity)
                        (equal (oref activity feed-ts) feed-ts))
                      activities)))
                (unless (= (length activities) (length new-activities))
                  (slack-activity-feed--cache-put-key
                   key new-activities (plist-get snapshot :pagination))
                  (setq changed t)))
            (dolist (activity activities)
              (when (and (equal (oref activity feed-ts) feed-ts)
                         (oref activity is-unread))
                (oset activity is-unread nil)
                (setq changed t)))))))
    changed))

(cl-defmethod slack-buffer-insert ((this slack-activity-feed-buffer) activity &rest _args)
  "Insert a rendered representation of THIS buffer into the current buffer.
ACTIVITY is the activity argument."
  (let* ((team (slack-buffer-team this))
         (time (slack-ts-to-time (oref activity feed-ts)))
         (is-unread (oref activity is-unread))
         (item (oref activity item))
         (type (oref item type))
         (msg (oref item message)))
    (with-slots (channel ts thread-ts source-message) msg
        (condition-case err
            (let* ((room (slack-room-find channel team))
                   (room-id (when room (oref room id)))
                   (room-name (if room
                                  (or (condition-case err
                                          (slack-room-name room team)
                                        (error
                                         (message "slack-activity: room name lookup failed: %S" err)
                                         nil))
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
                                  ((or "external_channel_invite" "shared_workspace_invite")
                                   "Invited to ")
                                  ("external_dm_invite" "DM invite from ")
                                  ("message_reaction" "Reacted in ")
                                  ("keyword" "Keyword match in ")
                                  ("generic_system_alert" "Alert in ")
                                  (_ "")))
                   (context-header
                    (concat (propertize (if is-unread "\u25cf " "  ")
                                        'face (when is-unread
                                                'slack-activity-unread-face))
                            (propertize (concat type-prefix location)
                                        'face 'slack-search-result-message-header-face
                                        'room-id (or room-id channel)
                                        'keymap slack-channel-button-keymap)))
                   (message-str
                    (slack-activity-feed--activity-message-string
                     activity team)))
              ;; Context header: no timestamp, no fill
              (let ((lui-time-stamp-position nil))
                (lui-insert context-header t))
              ;; Message: with timestamp, consistent with channel buffers
              (let ((lui-time-stamp-time time)
                    (lui-time-stamp-format "[%Y-%m-%d %H:%M] "))
                (lui-insert-with-text-properties
                 (slack-buffer--apply-message-keymap
                  (slack-buffer--render-native-emoji-string message-str))
                 'ts ts
                 'team-id (oref this team-id)
                 'room-id (or room-id channel)
                 'thread-ts thread-ts
                 'activity-type type
                 'activity-feed-ts (oref activity feed-ts)
                 'activity-feed-key (oref activity feed-key)))
              ;; Blank separator
              (let ((lui-time-stamp-position nil))
                (lui-insert "" t)))
          (error
           (let ((lui-time-stamp-position nil))
             (lui-insert (format "Error loading activity: %s"
                                 (error-message-string err))
                         t)))))))

(cl-defmethod slack-buffer-has-next-page-p ((this slack-activity-feed-buffer))
  "Tell if there is another page of results for THIS SLACK-ACTIVITY-FEED-BUFFER."
  (with-slots (activity-feed) this
    (oref activity-feed pagination)))

(cl-defmethod slack-buffer-insert-history ((this slack-activity-feed-buffer))
  "Insert historical messages into the buffer for THIS buffer."
  (with-slots (activity-feed) this
    (let* ((cur-point (point))
           (activities (-drop (oref activity-feed last) (oref activity-feed activities))))
      (cl-loop for m in activities
               do (slack-buffer-insert this m))
      (goto-char cur-point))
    ))

(cl-defmethod slack-buffer-request-history ((this slack-activity-feed-buffer) after-success)
  "Request older history for THIS buffer from the Slack API.
AFTER-SUCCESS is the after-success argument."
  (with-slots (activity-feed) this
    (let ((team (slack-buffer-team this)))
      (slack-activity-feed-request
       team
       (lambda (data)
         (let* ((new-activities (mapcar #'slack-activity-feed--parse-item
                                        (plist-get data :items)))
                (new-activity-feed
                 (make-instance
                  'slack-activity-feed
                  :activities (append (oref activity-feed activities)
                                      new-activities)
                  :pagination (plist-get (plist-get data :response_metadata)
                                         :next_cursor)
                  :last (- (length (oref activity-feed activities)) 1))))
           (slack-activity-feed--prefetch-rooms
            new-activities team
            (lambda ()
              (slack-activity-feed--prefetch-messages
               new-activities team
               #'ignore)))
           (oset this activity-feed new-activity-feed)
           (funcall after-success)))
       (oref activity-feed pagination)))))

(cl-defmethod slack-buffer-init-buffer ((this slack-activity-feed-buffer))
  "Initialize and return the display buffer for THIS buffer."
  (let ((buffer (cl-call-next-method)))
    (with-current-buffer buffer
      (slack-activity-feed-buffer-mode)
      (slack-buffer-set-current-buffer this)
      (with-slots (activity-feed) this
        (slack-activity-feed--render-activities
         this
         (oref activity-feed activities))))
    buffer))

(cl-defmethod slack-buffer-delete-load-more-string ((_this slack-activity-feed-buffer))
  "Remove the \"load more\" marker from the buffer for the activity feed
buffer.")

(cl-defmethod slack-buffer-prepare-marker-for-history ((_this slack-activity-feed-buffer))
  "Position point so history can be inserted in the activity feed buffer.")

(cl-defmethod slack-buffer-load-more ((this slack-activity-feed-buffer))
  "Load and append the next page of activity feed results.
THIS is the slack-activity-feed-buffer instance."
  (when (and (slack-buffer-has-next-page-p this)
             (not slack-buffer--loading-more-p))
    (setq slack-buffer--loading-more-p t)
    (cl-labels
        ((after-success
          ()
          (with-current-buffer (slack-buffer-buffer this)
            (slack-buffer-insert--history this)
            (setq slack-buffer--loading-more-p nil))))
      (slack-buffer-request-history this #'after-success))))

(cl-defmethod slack-buffer-insert--history ((this slack-activity-feed-buffer))
  "Insert loaded history items into the buffer for THIS buffer."
  (slack-buffer-insert-history this)
  (unless (slack-buffer-has-next-page-p this)
    (let ((lui-time-stamp-position nil))
      (lui-insert "(no more messages)\n" t))))

(defun slack-activity-feed--display-activities (activities team pagination)
  "Prefetch and display ACTIVITIES for TEAM with PAGINATION."
  (let ((activity-feed
         (make-instance 'slack-activity-feed
                        :activities activities
                        :pagination pagination)))
    (let ((buffer (slack-create-activity-feed-buffer activity-feed team)))
      (slack-buffer-display buffer)
      (message "Activity feed ready; fetching room metadata..."))
    (slack-activity-feed--prefetch-rooms
     activities team
     (lambda ()
       (message "Activity feed room metadata fetched; fetching message content...")
       (slack-activity-feed--prefetch-messages
        activities team
       (lambda ()
          (message "Activity feed message content fetched."))
        (lambda (_room messages)
          (slack-activity-feed--replace-prefetched-messages
           team messages))
        (lambda (activity)
          (slack-activity-feed--replace-unavailable-message
           team activity)))))))

(defun slack-activity-feed--show-data (data team)
  "Render Activity feed DATA for TEAM."
  (let ((activities (mapcar #'slack-activity-feed--parse-item
                            (plist-get data :items)))
        (pagination (plist-get (plist-get data :response_metadata)
                               :next_cursor)))
    (slack-activity-feed--display-activities activities team pagination)
    (slack-activity-feed--fetch-watched-activities
     team
     (lambda (extra-activities)
       (let ((snapshot
              (slack-activity-feed--cache-put
               team
               (slack-activity-feed--merge-activities
                activities extra-activities)
               pagination)))
         (when (and (slack-activity-feed--visible-p team)
                    (slack-activity-feed--snapshot-changed-p nil snapshot))
           (message
            "Activity feed has newer cached results; press g to refresh.")))))))

(defun slack-activity-feed--display-snapshot (snapshot team)
  "Display cached Activity Feed SNAPSHOT for TEAM."
  (slack-activity-feed--display-activities
   (plist-get snapshot :activities)
   team
   (plist-get snapshot :pagination)))

(defun slack-activity-feed-show ()
  "Show Slack activity feed."
  (interactive)
  (let ((team (slack-activity-feed--selected-team)))
    (if-let ((snapshot (slack-activity-feed--cache-get team)))
        (progn
          (slack-activity-feed--display-snapshot snapshot team)
          (message "Showing cached activity feed; refreshing in background...")
          (slack-activity-feed--refresh-cache team))
      (message "Fetching activity feed...")
      (slack-activity-feed-request
       team
       (lambda (data)
         (slack-activity-feed--show-data data team))))))

(defun slack-activity-feed-refresh ()
  "Refresh and redisplay the Slack activity feed explicitly."
  (interactive)
  (let ((team (slack-activity-feed--selected-team)))
    (message "Refreshing activity feed...")
    (slack-activity-feed--refresh-cache
     team
     (lambda (_old-snapshot new-snapshot)
       (slack-activity-feed--display-snapshot new-snapshot team)
       (message "Activity feed refreshed."))
     t)))

(cl-defmethod slack-feed--open ((_buf slack-activity-feed-buffer) ts)
  "Open the activity feed entry at TS."
  (if-let* ((room-id (get-text-property (point) 'room-id))
            (buf slack-current-buffer)
            (team (slack-buffer-team buf))
            (room (slack-room-find room-id team)))
      (let ((thread-ts (get-text-property (point) 'thread-ts)))
        (slack-team-ensure-registered team)
        (slack-activity-feed--mark-read team)
        (cond
         (thread-ts
          (slack-open-message team room thread-ts thread-ts ts))
         ((slack-activity-feed--thread-parent-p room ts)
          (slack-open-message team room ts ts ts))
         (t
          (slack-open-message team room ts nil ts))))
    (error "Not possible to jump to message")))

(cl-defmethod slack-buffer-display-thread ((buf slack-activity-feed-buffer) ts)
  "Open the activity feed thread at TS in BUF."
  (if-let* ((activity (slack-activity-feed--activity-from-ts buf ts))
            (msg (oref (oref activity item) message))
            (team (slack-buffer-team buf))
            (room (slack-room-find (oref msg channel) team)))
      (let ((thread-ts (oref msg thread-ts)))
        (slack-team-ensure-registered team)
        (slack-activity-feed--mark-read team)
        (if thread-ts
            (slack-open-message team room thread-ts thread-ts ts)
          (slack-open-message team room ts ts ts)))
    (error "Not possible to jump to thread")))

(defun slack-activity-feed--thread-parent-p (room ts)
  "Return non-nil when the message at TS in ROOM has replies.
The Slack API omits `thread_ts' from at_user and similar activity
payloads even when the mentioned message is a thread parent.  Fall
back to the cached message's `replies' slot to detect that case."
  (when-let* ((msg (ignore-errors (slack-room-find-message room ts))))
    (and (slot-boundp msg 'replies)
         (oref msg replies))))

(defun slack-activity-feed--mark-read (team)
  "Mark the feed item at point read in TEAM.
Params are read from the activity's text properties planted by
`slack-buffer-insert'.  Activity API rows use `activity.markRead';
watched-channel rows without Activity keys use
`conversations.mark'.  Only after Slack confirms success does the
local `slack-activity' object flip `is-unread' to nil and the
visual unread bullet on the header line get erased."
  (let* ((feed-key (get-text-property (point) 'activity-feed-key))
         (feed-ts (get-text-property (point) 'activity-feed-ts))
         (type (get-text-property (point) 'activity-type))
         (channel (slack-activity-feed--room-id-at (point)))
         (ts (or (get-text-property (point) 'ts)
                 (slack-get-ts)))
         (thread-ts (get-text-property (point) 'thread-ts))
         (feed-buffer slack-current-buffer)
         (source-buffer (current-buffer))
         (pos (point)))
    (cond
     (feed-key
      (let ((params (append (list (cons "feed_ts" feed-ts)
                                  (cons "type" type)
                                  (cons "channel" channel)
                                  (cons "key" feed-key))
                            (when thread-ts
                              (list (cons "thread_ts" thread-ts))))))
        (slack-request
         (slack-request-create
          slack-activity-feed-mark-read-url
          team
          :type "POST"
          :params params
          :success (cl-function
                    (lambda (&key data &allow-other-keys)
                      (slack-request-handle-error
                       (data "slack-activity-feed-mark-read")
                       (slack-activity-feed--on-marked-read
                        feed-buffer source-buffer pos feed-ts))))))))
     ((and channel ts)
      (when-let ((room (slack-room-find channel team)))
        (slack-conversations-mark
         room team ts
         (lambda ()
           (when (or (string= "0" (oref room last-read))
                     (string< (oref room last-read) ts))
             (oset room last-read ts))
           (slack-activity-feed--on-marked-read
            feed-buffer source-buffer pos feed-ts)
           (slack-counts-update team))))))))

(defun slack-activity-feed--on-marked-read (feed-buffer source-buffer pos feed-ts)
  "Reflect the server-confirmed mark-read for FEED-TS in FEED-BUFFER.
SOURCE-BUFFER is the buffer the user invoked the mark from; POS
is the cursor position at invocation.  Flip the matching visible
and cached `slack-activity' read state and erase the header bullet
for that entry."
  (let ((changed nil))
    (when (and feed-buffer (slot-boundp feed-buffer 'activity-feed))
      (when-let ((activity (slack-activity-feed--find-activity
                            feed-buffer feed-ts)))
        (when (oref activity is-unread)
          (oset activity is-unread nil)
          (setq changed t))))
    (when feed-buffer
      (setq changed
            (or (slack-activity-feed--cache-mark-read
                 (slack-buffer-team feed-buffer) feed-ts)
                changed)))
    (when changed
      (slack-activity-feed--decrement-unread-summary)))
  (when (buffer-live-p source-buffer)
    (with-current-buffer source-buffer
      (save-excursion
        (goto-char pos)
        (slack-activity-feed--erase-unread-bullet)))))

(defun slack-activity-feed--erase-unread-bullet ()
  "Replace the unread bullet on the current activity item's header line.
The bullet sits on the context-header line that precedes the
message body, so search backward from point (bounded by the
previous blank separator between items) to locate it."
  (let ((limit (save-excursion
                 (if (re-search-backward "^$" nil t)
                     (point)
                   (point-min))))
        (inhibit-read-only t))
    (goto-char (line-end-position))
    (when (search-backward "\u25cf" limit t)
      (replace-match " "))))

(define-key slack-activity-feed-buffer-mode-map (kbd "RET") 'slack-feed-open-at-point)
(define-key slack-activity-feed-buffer-mode-map (kbd "n") 'slack-feed-goto-next)
(define-key slack-activity-feed-buffer-mode-map (kbd "p") 'slack-feed-goto-prev)
(define-key slack-activity-feed-buffer-mode-map (kbd "u") 'slack-activity-feed-toggle-unread)
(define-key slack-activity-feed-buffer-mode-map (kbd "g") 'slack-activity-feed-refresh)

(defun slack-activity-feed-refresh-unread-summary ()
  "Update `slack-has-unreads' and `slack-unread-count' from Activity feed.
Calls `activity.feed' in unread-only mode for each registered
team, counts the items, and sets the global indicator variables.
Works over HTTP and does not require an active WebSocket."
  (let ((total-count 0)
        (any-unreads nil)
        (pending (list 0)))
    (maphash
     (lambda (_token team)
       (cl-incf (car pending))
       (slack-activity-feed--fetch-unread-count
        team
        (lambda (count)
          (when (< 0 count)
            (setq any-unreads t)
            (cl-incf total-count count))
          (when (= 0 (cl-decf (car pending)))
            (setq slack-has-unreads any-unreads
                  slack-unread-count total-count)
            (force-mode-line-update)))))
     slack-teams-by-token)
    (when (= 0 (car pending))
      (setq slack-has-unreads nil
            slack-unread-count 0)
      (force-mode-line-update))))

(defun slack-activity-feed--unread-count (activities)
  "Return the number of unread entries in ACTIVITIES."
  (cl-loop for activity in activities
           count (oref activity is-unread)))

(defun slack-activity-feed--fetch-unread-count (team callback)
  "Fetch unread Activity item count for TEAM.
CALLBACK receives a single integer argument."
  (let ((slack-activity-feed-mode-show-only-unread t))
    (slack-activity-feed-request
     team
     (lambda (data)
       (let ((pagination (plist-get (plist-get data :response_metadata)
                                    :next_cursor)))
         (slack-activity-feed--with-watched-activities
          (mapcar #'slack-activity-feed--parse-item
                  (plist-get data :items))
          team
          (lambda (activities)
            (let ((unread-activities
                   (cl-remove-if-not
                    (lambda (activity)
                      (oref activity is-unread))
                    activities)))
              (slack-activity-feed--cache-put team unread-activities pagination)
              (slack-activity-feed--cache-merge-activities
               team unread-activities)
              (funcall callback (length unread-activities))))))))))

(provide 'slack-activity-feed-buffer)
;;; slack-activity-feed-buffer.el ends here
