;;; slack-websocket.el --- slack websocket interface  -*- lexical-binding: t; -*-

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
(require 'websocket)
(require 'slack-util)
(require 'slack-request)
(require 'slack-team)
(require 'slack-team-ws)
(require 'slack-file)
(require 'slack-dialog-buffer)
(require 'slack-user)
(require 'slack-group)
(require 'slack-channel)
(require 'slack-im)
(require 'slack-thread)
(require 'slack-bot)
(require 'slack-usergroup)
(require 'slack-slash-commands)
(declare-function slack-download-emoji "slack-emoji")
(declare-function slack-fetch-team-emojis "slack-emoji")
(declare-function slack-emoji-fetch-master-data-async "slack-emoji")
(require 'slack-star)
(require 'slack-message-notification)
(require 'slack-room-buffer)
(require 'slack-typing)
(require 'slack-stars-buffer)
(require 'slack-conversations)
(require 'slack-dnd-status)
(require 'slack-message-event)
(require 'slack-reply-event)
(require 'slack-reaction-event)
(require 'slack-star-event)
(require 'slack-room-event)
(require 'slack-thread-event)
(require 'slack-defcustoms)
(require 'dash)

(defconst slack-api-test-url "https://slack.com/api/api.test")
(defconst slack-rtm-connect-url "https://slack.com/api/rtm.connect")
(defconst slack-api-client-userboot-url "https://slack.com/api/client.userBoot")


(defun slack-ws-on-timeout (team-id)
  "Close and reconnect the websocket when opening timed out for TEAM-ID."
  (let* ((team (slack-team-find team-id))
         (ws (oref team ws)))
    (let ((debug-on-error t))
      (slack-log (format "websocket open timeout")
                 team)
      (slack-ws--close ws team)
      (slack-ws-reconnect ws team))))

(cl-defmethod slack-ws-open ((ws slack-team-ws) team &key (on-open nil) (ws-url nil))
  "Attempt to open Slack websocket for interactive experience.
The websocket makes sure your status is communicated, your
message buffer reacts to new messages and emacs-slack is aware of
what is happening in your team."
  (let ((websocket-nowait-p (oref ws websocket-nowait))
        (ws-url (or ws-url
                    (concat
                     "wss://wss-primary.slack.com/?token="
                     (slack-team-token team)
                     "&sync_desync=1&slack_client=desktop&start_args=%3Fagent%3Dclient%26org_wide_aware%3Dtrue%26agent_version%3D1730299661%26eac_cache_ts%3Dtrue%26cache_ts%3D0%26name_tagging%3Dtrue%26only_self_subteams%3Dtrue%26connect_only%3Dtrue%26ms_latest%3Dtrue&no_query_on_subscribe=1&flannel=3&lazy_channels=1&gateway_server="
                     (slack-team-id team)
                     "-4&batch_presence_aware=1"))))
    (slack-url-cookie-store team)
    (unless websocket-nowait-p
      (slack-ws-set-connect-timeout-timer ws
                                          #'slack-ws-on-timeout
                                          (slack-team-id team)))
    (cl-labels
        ((on-message (_websocket frame)
           (slack-ws-on-message ws frame team))
         (handle-on-open (_websocket)
           (oset ws reconnect-count 0)
           (oset ws connected t)
           (slack-ws-reconnect-reset-backoff ws)
           (slack-log "WebSocket on-open"
                      team :level 'debug)
           (when (functionp on-open)
             (funcall on-open)))
         (on-close (websocket)
           (oset ws connected nil)
           (slack-log (format "Websocket on-close: STATE: %s"
                              (websocket-ready-state websocket))
                      team :level 'debug))
         (on-error (_websocket type err)
           (slack-log (format "Error on `websocket-open'. TYPE: %s, ERR: %s"
                              type err)
                      team
                      :level 'error)))
      (slack-log (format "Opening websocket connection. NOWAIT: %s, URL: %s \n(not using anymore ws object url: %s)"
                         (oref ws websocket-nowait)
                         ws-url
                         (oref ws url))
                 team
                 :level 'debug)
      (oset ws conn
            (condition-case error-var
                (websocket-open ws-url
                                :on-message #'on-message
                                :on-open #'handle-on-open
                                :on-close #'on-close
                                :on-error #'on-error
                                :nowait websocket-nowait-p
                                ;; these are not necessary as slack-start sets them already
                                ;; :custom-header-alist (list (cons "Cookie" (format "d=%s" (slack-team-cookie team))))
                                )
              (error
               (slack-log (format "An Error occured while opening websocket connection: %s"
                                  error-var)
                          team
                          :level 'error)
               nil)))
      (if websocket-nowait-p
          (slack-ws-set-connect-timeout-timer ws
                                              #'slack-ws-on-timeout
                                              (slack-team-id team)))
      (slack-log (format "Called `websocket-open' URL: %s"
                         ws-url)
                 team :level 'debug))))

(defvar slack-presence-timers (make-hash-table :test 'equal)
  "Keep track of the teams presence timers.
Set when SLACK-EMIT-PERIODIC-PRESENCE-P is set.")

(defun slack-ws-close ()
  "Close the websocket for every team and stop background workers."
  (interactive)
  (slack-counts-stop-refresh-timer)
  (mapc #'(lambda (team) (slack-ws--close (oref team ws) team t))
        (hash-table-values slack-teams-by-token))
  (ignore-errors (mapcar 'cancel-timer (hash-table-values slack-presence-timers)))
  (slack-request-worker-quit))

(cl-defun slack-ws--close (ws team &optional (close-reconnection nil))
  "Close websocket WS for TEAM, optionally disabling reconnection.
Non-nil CLOSE-RECONNECTION also cancels the reconnect timer and sets
`inhibit-reconnection'."
  (cl-labels
      ((close (ws team)
              (slack-ws-cancel-ping-timer ws)
              (slack-ws-cancel-ping-check-timers ws)
              (when close-reconnection
                (slack-ws-cancel-reconnect-timer ws)
                (oset ws inhibit-reconnection t))
              (let ((conn (oref ws conn)))
                (condition-case error-var
                    (websocket-close conn)
                  (error (slack-log (format "An Error occured while closing websocket connection: %s"
                                            error-var)
                                    team
                                    :level 'error)))
                (when (and conn (process-live-p (websocket-conn conn)))
                  (slack-log "websocket prosess still alive. call `delete-process' again."
                             team
                             :level 'debug)
                  (delete-process (websocket-conn conn)))
                (slack-log "Slack Websocket Closed" team))))
    (close ws team)
    (slack-request-worker-remove-request team)))

(defun slack-ws-payload-ping-p (payload)
  "Return non-nil when PAYLOAD is a ping-type websocket message."
  (string= "ping" (plist-get payload :type)))

(defun slack-ws-payload-presence-sub-p (payload)
  "Check if websocket PAYLOAD is of type presence_sub."
  (string= "presence_sub" (plist-get payload :type)))

(defun slack-ws-payload-presence-query-p (payload)
  "Check if websocket PAYLOAD is of type presence_query."
  (string= "presence_query" (plist-get payload :type)))

(defun slack-ws-retryable-payload-p (payload)
  "Return non-nil when PAYLOAD should be queued for retry on reconnect."
  (and (not (slack-ws-payload-ping-p payload))
       (not (slack-ws-payload-presence-sub-p payload))
       (not (slack-ws-payload-presence-query-p payload))))

(cl-defmethod slack-ws-send ((ws slack-team-ws) payload team)
  "Send PAYLOAD over websocket WS for TEAM, queueing it for retry if needed."
  (slack-log-websocket-payload payload team t)
  (with-slots (waiting-send conn) ws
    (when (slack-ws-retryable-payload-p payload)
      (push payload waiting-send))
    (cl-labels
        ((reconnect ()
                    (slack-ws--close ws team)
                    (slack-ws-reconnect ws team)))
      (if (websocket-openp conn)
          (condition-case err
              (websocket-send-text conn (json-encode payload))
            (error
             (slack-log (format "Error in `slack-ws-send`: %s" err)
                        team :level 'debug)
             (reconnect)))
        (reconnect)))))

(cl-defmethod slack-ws-resend ((ws slack-team-ws) team)
  "Re-send every payload queued on WS for TEAM after a reconnect."
  (with-slots (waiting-send) ws
    (let ((candidate waiting-send))
      (setq waiting-send nil)
      (cl-loop for msg in candidate
               do (slack-ws-send ws msg team)))))

(defun slack-ws-on-ping-timeout (team-id)
  "Reconnect the websocket for TEAM-ID after a missed ping response."
  (let* ((team (slack-team-find team-id))
         (ws (oref team ws)))
    (slack-log "Slack Websocket PING Timeout." team :level 'warn)
    (slack-ws--close ws team)
    (slack-ws-reconnect ws team)))

(defun slack-ws-ping (team-id)
  "Send a websocket ping for TEAM-ID and arm the ping-timeout check."
  (let ((team (slack-team-find team-id)))
    (when (and team (slack-team-id team))
      (with-slots (message-id ws) team
        (let* ((time (number-to-string (time-to-seconds (current-time))))
               (ping-message (list :id message-id
                                   :type "ping"
                                   :time time)))
          (slack-team-send-message team ping-message)
          (slack-log (format "Send PING: %s" time) team :level 'trace)
          (slack-ws-set-ping-check-timer ws time
                                         #'slack-ws-on-ping-timeout
                                         (slack-team-id team))
          (slack-log (format "Set PING timeout timer. timeout in %s sec"
                             (oref ws check-ping-timeout-sec))
                     team :level 'trace))))))

(defvar slack-disconnected-timer nil)
(defun slack-schedule-abandon-reconnect-notice (team)
  "Schedule a recurring idle-timer warning that reconnection was abandoned.
The timer fires every 5 seconds of idle time so the user sees the
message when they return to Emacs."
  (unless slack-disconnected-timer
    (setq slack-disconnected-timer
          (run-with-idle-timer 5 t
                               #'(lambda ()
                                   (slack-log
                                    "Reconnect Count Exceeded. Manually invoke `slack-start'."
                                    team :level 'error))))))

(defun slack-cancel-abandon-reconnect-notice ()
  "Cancel the recurring disconnection warning timer."
  (if (and slack-disconnected-timer
           (timerp slack-disconnected-timer))
      (progn
        (cancel-timer slack-disconnected-timer)
        (setq slack-disconnected-timer nil))))

(defun slack-request-api-test (team &optional after-success)
  "A call to Slack test API for TEAM to see if connection succeeded.
Provide AFTER-SUCCESS to run a side effect."
  (cl-labels
      ((on-success (&key data &allow-other-keys)
         (slack-request-handle-error
          (data "slack-request-api-test")
          (if after-success
              (funcall after-success)))))
    (slack-request
     (slack-request-create
      slack-api-test-url
      team
      :type "POST"
      :success #'on-success
      :error (lambda (&rest _args)
               (let ((ws (oref team ws)))
                 ;; sometimes there seems to be a network connectivity that breaks reconnection at this point
                 (slack-ws--close ws team)
                 (slack-ws-reconnect ws team)))))))


(defun slack-ws-abort-reconnect (team-id)
  "Give up reconnection for TEAM-ID and surface a persistent warning."
  (let* ((team (slack-team-find team-id))
         (ws (oref team ws)))
    (slack-schedule-abandon-reconnect-notice team)
    (slack-ws--close ws team t)))

(defun slack-ws-reconnect-with-reconnect-url (team-id)
  "Reconnect TEAM-ID using the cached reconnect URL from Slack."
  (let* ((team (slack-team-find team-id))
         (ws (oref team ws)))
    (slack-log "Reconnect with reconnect-url" team)
    (slack-ws-open ws team
                   :ws-url (oref ws reconnect-url))))

(defvar slack--lock-user-list-update nil
  "Lock expensive user list request to run less often.
This is just a mitigation because sometimes it will run at the
same time as other updates and rate limit the token.")
(defun slack--lock-user-list-update-release ()
  "Release `slack--lock-user-list-update'."
  (setq slack--lock-user-list-update nil))

(defun slack--update-user-list-with-lock (team)
  "Call slack-user-list-update for TEAM.
Locking the operation via `slack--lock-user-list-update' to avoid
 multiple calls that rate limit the token and make emacs-slack
 unusable."
  (unless slack--lock-user-list-update
    (slack-user-list-update team)
    (setq slack--lock-user-list-update t)
    ;; 45s cooldown: users.list is tier-2 (~20 req/min) and this
    ;; request is large; spacing it out avoids rate-limiting the token
    ;; for other concurrent requests.
    (run-with-timer 45 nil #'slack--lock-user-list-update-release)))

(defcustom slack-prefetch-channel-messages-p t
  "If non-nil, prefetch messages for channels with unreads after connecting.
This makes opening unread channels near-instant at the cost of
background API calls after WebSocket connection."
  :type 'boolean
  :group 'slack)

(defun slack-prefetch-unread-channels (team)
  "Prefetch conversations.history for TEAM's channels with unread messages.
Fires staggered async requests (~50 req/min, tier 3) for channels
that have unreads and no cached messages, so opening them is instant."
  (when (and slack-prefetch-channel-messages-p (oref team counts))
    (let* ((all-rooms (append (slack-team-channels team)
                              (slack-team-groups team)
                              (slack-team-ims team)))
           (unread-rooms
            (cl-remove-if-not
             (lambda (room)
               (and (slack-room-has-unread-p room team)
                    (not (slack-room-muted-p room team))
                    ;; Skip rooms that already have cached messages
                    (= 0 (hash-table-count (oref room messages)))))
             all-rooms))
           (count (length unread-rooms)))
      (when (> count 0)
        (slack-log (format "Prefetching messages for %d unread channels" count)
                   team :level 'info)
        (cl-loop for room in unread-rooms
                 for delay from 0 by 1.2  ; ~50 req/min (tier 3)
                 do (run-at-time
                     delay nil
                     (lambda (r)
                       (condition-case err
                           (slack-conversations-history
                            r team
                            :limit "50"
                            :after-success
                            (lambda (messages &rest _)
                              (when messages
                                (slack-room-set-messages r messages team))))
                         (error
                          (slack-log
                           (format "Prefetch error for %s: %S"
                                   (oref r id) err)
                           team :level 'warn))))
                     room))))))

(defun slack-ws-on-reconnect-open (team-id)
  "Refresh data after websocket reconnection for TEAM-ID.
This also closes unnecessary buffers and refresh message buffer contents."
  (let* ((team (slack-team-find team-id)))
    (slack-conversations-list-update team)
    (slack-counts-update team)
    ;; Delay 3s: let conversations-list and counts finish first, since
    ;; user-list is large and would compete for the same rate-limit budget.
    (run-with-timer 3 nil #'slack--update-user-list-with-lock
                    team)
    (slack-team-presence-query-and-subscribe team)
    (slack-dnd-status-team-info team)
    (when (hash-table-p (oref team slack-message-buffer))
      (cl-loop for sb in (hash-table-values (oref team slack-message-buffer))
               do (slack-if-let* ((buffer (and sb (slack-buffer-buffer sb)))
                                  (live-p (buffer-live-p buffer)))
                      (slack-buffer-load-missing-messages sb))))

    (slack-team-kill-buffers
     team :except '(slack-message-buffer
                    slack-thread-message-buffer
                    slack-message-edit-buffer
                    slack-message-share-buffer
                    slack-room-message-compose-buffer
                    slack-search-result-buffer-mode
                    slack-pinned-items-buffer-mode))
    ;; Delay 5s: counts-update must complete first so we know which
    ;; channels have unreads; also avoids piling onto the rate limit.
    (run-with-timer 5 nil #'slack-prefetch-unread-channels team)))

(defun slack-ws--reconnect (team-id &optional force)
  "Reconnect the websocket for TEAM-ID, honoring retry limits unless FORCE."
  (let* ((team (slack-team-find team-id))
         (ws (oref team ws)))
    (cl-labels
        ((on-authorize-error (&key error-thrown symbol-status &allow-other-keys)
           (slack-log (format "Reconnect Failed: %s, %s"
                              error-thrown
                              symbol-status)
                      team)
           (slack-ws-reconnect ws team))
         (on-authorize-success (data)
           (let ((team-data (plist-get data :team))
                 (self-data (plist-get data :self)))
             (slack-team-set-ws-url team (plist-get data :url))
             (oset team domain (plist-get team-data :domain))
             (oset team id (plist-get team-data :id))
             (oset team name (plist-get team-data :name))
             (oset team self self-data)
             (oset team self-id (plist-get self-data :id))
             (oset team self-name (plist-get self-data :name))
             (slack-ws-open ws team
                            :on-open #'(lambda ()
                                         (slack-ws-on-reconnect-open team-id))))))
      (if (and (not force) (slack-ws-reconnect-count-exceed-p ws))
          (slack-ws-abort-reconnect team-id)
        (progn
          (slack-ws-inc-reconnect-count ws)
          (slack-ws--close ws team)
          (if (slack-ws-use-reconnect-url-p ws)
              (slack-request-api-test team
                                      #'(lambda ()
                                          (slack-ws-reconnect-with-reconnect-url team-id)))
            (slack-authorize team
                             #'on-authorize-error
                             #'on-authorize-success))
          (slack-log (format "Reconnecting... [%s/%s]"
                             (oref ws reconnect-count)
                             (oref ws reconnect-count-max))
                     team
                     :level 'warn))))))

(cl-defmethod slack-ws-reconnect ((ws slack-team-ws) team)
  "Reconnect if `reconnect-count' does not exceed `reconnect-count-max'.
Uses exponential backoff: delay doubles each attempt (capped at
`reconnect-after-sec-max'), resets on successful reconnection.
TEAM is one of `slack-teams'."
  (unless (or (oref ws inhibit-reconnection)
              (null (slack-team-id team)))
    (let ((delay (slack-ws-reconnect-backoff ws)))
      (slack-log (format "Scheduling reconnect in %s seconds" delay)
                 team :level 'info)
      (slack-ws-set-reconnect-timer ws
                                    #'slack-ws--reconnect
                                    (slack-team-id team)))))

;; Message handler

(cl-defmethod slack-ws-handle-pong ((ws slack-team-ws) payload team)
  "Handle the Slack websocket `pong' event with PAYLOAD for TEAM on WS."
  (slack-ws-remove-from-resend-queue ws payload team)
  (let* ((key (plist-get payload :time))
         (timer (gethash key (oref ws ping-check-timers))))
    (slack-team-presence-query-and-subscribe team)
    (slack-log (format "Receive PONG: %s. RTT is %s sec"
                       key
                       (- (time-to-seconds (current-time)) (string-to-number key)))
               team :level 'trace)
    (when timer
      (cancel-timer timer)
      (remhash key (oref ws ping-check-timers))
      (slack-log (format "Remove PING Check Timer: %s" key)
                 team :level 'trace))

    (slack-ws-set-ping-timer ws #'slack-ws-ping (slack-team-id team))
    ))

;; (:type error :error (:msg Socket URL has expired :code 1))
(cl-defmethod slack-ws-handle-error ((ws slack-team-ws) payload team)
  "Try to recover from a websocket error given its PAYLOAD."
  (let* ((err (plist-get payload :error))
         (code (plist-get err :code)))
    (cond
     ((or (eq 1 code) (eq 6 code)) ;; code 6 is about a network error, apparently caused by leaving computer to sleep over night
      (slack-ws--close ws team)
      (slack-ws-reconnect ws team))
     (t (slack-log (format "Unknown Error: %s, MSG: %s"
                           code (plist-get err :msg))
                   team)))))


(cl-defmethod slack-ws-on-message ((ws slack-team-ws) frame team)
  "Dispatch a websocket FRAME received on WS for TEAM to the right handler."
  ;; (message "%s" (slack-request-parse-payload
  ;;                (websocket-frame-payload frame)))
  (when (websocket-frame-completep frame)
    (let* ((payload (slack-request-parse-payload
                     (websocket-frame-payload frame)))
           (decoded-payload (and payload (slack-decode payload)))
           (type (and decoded-payload
                      (plist-get decoded-payload :type))))
      ;; (message "%s" decoded-payload)
      (when (slack-team-event-log-enabledp team)
        (slack-log-websocket-payload decoded-payload team))
      (when decoded-payload
        (cond
         ((string= type "error")
          (slack-ws-handle-error ws decoded-payload team))
         ((string= type "pong")
          (slack-ws-handle-pong ws decoded-payload team))
         ((string= type "hello")
          (slack-ws-cancel-connect-timeout-timer ws)
          (slack-ws-cancel-reconnect-timer ws)
          (slack-cancel-abandon-reconnect-notice)
          (slack-ws-set-ping-timer ws #'slack-ws-ping (slack-team-id team))
          (slack-ws-resend ws team)
          (slack-log "Slack Websocket Is Ready!" team :level 'info)
          (slack-counts-update team)
          ;; Prefetch unread channels after counts arrive
          (run-with-timer 5 nil #'slack-prefetch-unread-channels team))
         ((plist-get decoded-payload :reply_to)
          (slack-ws-handle-reply ws decoded-payload team))
         ((string= type "message")
          (slack-ws-handle-message decoded-payload team))
         ((string= type "reaction_added")
          (slack-ws-handle-reaction-added decoded-payload team))
         ((string= type "reaction_removed")
          (slack-ws-handle-reaction-removed decoded-payload team))
         ((string= type "channel_created")
          (slack-ws-handle-channel-created decoded-payload team))
         ((or (string= type "channel_archive")
              (string= type "group_archive"))
          (slack-ws-handle-room-archive decoded-payload team))
         ((or (string= type "channel_unarchive")
              (string= type "group_unarchive"))
          (slack-ws-handle-room-unarchive decoded-payload team))
         ((string= type "channel_deleted")
          (slack-ws-handle-channel-deleted decoded-payload team))
         ((or (string= type "channel_rename")
              (string= type "group_rename"))
          (slack-ws-handle-room-rename decoded-payload team))
         ((or (string= type "channel_left")
              (string= type "group_left"))
          (slack-ws-handle-room-left decoded-payload team))
         ((string= type "channel_joined")
          (slack-ws-handle-channel-joined decoded-payload team))
         ((string= type "group_joined")
          (slack-ws-handle-group-joined decoded-payload team))
         ((string= type "presence_change")
          (slack-ws-handle-presence-change decoded-payload team))
         ((or (string= type "bot_added")
              (string= type "bot_changed"))
          (slack-ws-handle-bot decoded-payload team))
         ((string= type "file_created")
          (slack-ws-handle-file-created decoded-payload team))
         ((or (string= type "file_deleted")
              (string= type "file_unshared"))
          (slack-ws-handle-file-deleted decoded-payload team))
         ((or (string= type "im_marked")
              (string= type "channel_marked")
              (string= type "group_marked")
              (string= type "mpim_marked"))
          (slack-ws-handle-room-marked decoded-payload team))
         ((string= type "thread_marked")
          (slack-ws-handle-thread-marked decoded-payload team))
         ((string= type "thread_subscribed")
          (slack-ws-handle-thread-subscribed decoded-payload team))
         ((string= type "thread_unsubscribed")
          (slack-ws-handle-thread-unsubscribed decoded-payload team))
         ((string= type "im_open")
          (slack-ws-handle-im-open decoded-payload team))
         ((or (string= type "im_close")
              (string= type "group_close"))
          (slack-ws-handle-close decoded-payload team))
         ((string= type "team_join")
          (slack-ws-handle-team-join decoded-payload team))
         ((string= type "user_typing")
          (slack-ws-handle-user-typing decoded-payload team))
         ((string= type "user_change")
          (slack-ws-handle-user-change decoded-payload team))
         ((string= type "member_joined_channel")
          (slack-ws-handle-member-joined-channel decoded-payload team))
         ((string= type "member_left_channel")
          (slack-ws-handle-member-left_channel decoded-payload team))
         ((or ;; (string= type "dnd_updated")
           (string= type "dnd_updated_user"))
          (slack-ws-handle-dnd-updated decoded-payload team))
         ((string= type "star_added")
          (slack-ws-handle-star-added decoded-payload team))
         ((string= type "star_removed")
          (slack-ws-handle-star-removed decoded-payload team))
         ((string= type "reconnect_url")
          (slack-ws-handle-reconnect-url ws decoded-payload team))
         ((string= type "app_conversation_invite_request")
          (slack-ws-handle-app-conversation-invite-request decoded-payload team))
         ((string= type "commands_changed")
          (slack-ws-handle-commands-changed decoded-payload team))
         ((string= type "dialog_opened")
          (slack-ws-handle-dialog-opened decoded-payload team))
         ((string= type "subteam_created")
          (slack-ws-handle-subteam-created decoded-payload team))
         ((string= type "subteam_updated")
          (slack-ws-handle-subteam-updated decoded-payload team))
         ((string= type "pin_removed")
          (slack-ws-handle-pin-removed decoded-payload team))
         ((string= type "pin_added")
          (slack-ws-handle-pin-added decoded-payload team))
         ((string= type "update_thread_state")
          (slack-ws-handle-update-thread-state decoded-payload team))
         ((string= type "app_mention")
          (slack-ws-handle-app-mention decoded-payload team))
         ((string= type "app_rate_limited")
          (slack-ws-handle-app-rate-limited decoded-payload team))
         (t
          (slack-log (format "Unhandled WebSocket event type: %s" type)
                     team :level 'debug))
         )))))

(defun slack-ws-handle-app-rate-limited (payload team)
  "Handle an app_rate_limited event from Slack.
PAYLOAD contains :minute_rate_limited indicating when rate limiting began."
  (let ((minute (plist-get payload :minute_rate_limited)))
    (slack-log (format "Rate limited by Slack API at minute %s. Requests will be throttled."
                       minute)
               team :level 'warn)))

(defun slack-ws-handle-update-thread-state (payload team)
  "Handle update_thread_state: update thread counts and modeline."
  (let* ((has-unreads (eq t (plist-get payload :has_unreads)))
         (mention-count (plist-get payload :mention_count))
         (channel (plist-get payload :channel))
         (latest-ts (plist-get payload :latest_ts)))
    (slack-if-let* ((counts (oref team counts)))
        (slack-counts-update-threads counts has-unreads mention-count))
    (slack-log (format "Thread state updated: channel=%s ts=%s unreads=%s mentions=%s"
                       channel latest-ts has-unreads mention-count)
               team :level 'debug)))

(defun slack-ws-handle-app-mention (payload team)
  "Handle app_mention events by routing through the message handler.
The app_mention payload is structurally identical to a message event."
  (slack-ws-handle-message payload team))

(defun slack-ws-handle-pin-added (payload team)
  "Handle the Slack websocket `pin added' event with PAYLOAD for TEAM."
  (let* ((item (plist-get payload :item))
         (message (plist-get item :message))
         (ts (plist-get message :ts))
         (channel-id (plist-get payload :channel_id)))
    (slack-if-let*
        ((room (slack-room-find channel-id team))
         (message (slack-room-find-message room ts)))
        (cl-pushnew channel-id (oref message pinned-to)
                    :test #'string=))))

(defun slack-ws-handle-pin-removed (payload team)
  "Handle the Slack websocket `pin removed' event with PAYLOAD for TEAM."
  (let* ((item (plist-get payload :item))
         (message (plist-get item :message))
         (ts (plist-get message :ts))
         (channel-id (plist-get payload :channel_id)))
    (slack-if-let*
        ((room (slack-room-find channel-id team))
         (message (slack-room-find-message room ts)))
        (oset message pinned-to (cl-remove-if #'(lambda (e) (string= channel-id e))
                                              (oref message pinned-to))))))

(cl-defmethod slack-ws-handle-reconnect-url ((ws slack-team-ws) payload _team)
  "Handle the Slack websocket `reconnect_url' event by caching it on WS."
  (oset ws reconnect-url (plist-get payload :url)))

(defun slack-ws-handle-user-typing (payload team)
  "Handle the Slack websocket `user typing' event with PAYLOAD for TEAM."
  (slack-if-let*
      ((user-id (plist-get payload :user))
       (room (slack-room-find (plist-get payload :channel) team))
       (buf (slack-buffer-find 'slack-message-buffer team room))
       (show-typing-p (slack-buffer-show-typing-p (get-buffer
                                                   (slack-buffer-name buf)))))
      (cl-labels
          ((update-typing (user)
             (let ((limit (+ 3 (float-time))))
               (slack-if-let* ((typing (oref team typing))
                               (typing-room (slack-room-find (oref typing room-id) team))
                               (same-room-p (string= (oref room id) (oref typing-room id))))
                   (progn
                     (slack-typing-set-limit typing limit)
                     (slack-typing-add-user typing user limit))
                 (oset team
                       typing
                       (slack-typing-create room limit user))
                 (oset team
                       typing-timer
                       (run-with-timer t 1
                                       #'slack-typing-display
                                       (slack-team-id team)))))))
        (slack-if-let*
            ((user (slack-user-name user-id team)))
            (update-typing user)
          (slack-user-info-request
           user-id team
           :after-success
           #'(lambda ()
               (update-typing (slack-user-name user-id team))))))))

(defun slack-ws-handle-team-join (payload team)
  "Handle the Slack websocket `team join' event with PAYLOAD for TEAM."
  (let ((user (slack-decode (plist-get payload :user))))
    (cl-labels
        ((after-success ()
                        (let ((user-id (plist-get user :id)))
                          (slack-log (format "User %s Joind Team: %s"
                                             (slack-user-name user-id team)
                                             (slack-team-name team))
                                     team
                                     :level 'info))))
      (slack-user-info-request (plist-get user :id)
                               team
                               :after-success #'after-success))))

(defun slack-ws-handle-im-open (payload team)
  "Handle the Slack websocket `im open' event with PAYLOAD for TEAM."
  (slack-event-update (slack-create-im-open-event payload)
                      team))

(defun slack-ws-handle-close (payload team)
  "Handle the Slack websocket `close' event with PAYLOAD for TEAM."
  (slack-event-update (slack-create-room-close-event payload)
                      team))

(defun slack-ws-handle-message (payload team)
  "Handle the Slack websocket `message' event with PAYLOAD for TEAM."
  (slack-event-update (slack-create-message-event payload)
                      team))

(defun slack-ws-payload-pong-p (payload)
  "Return non-nil when PAYLOAD is a pong-type websocket message."
  (string= "pong" (plist-get payload :type)))

(cl-defmethod slack-ws-remove-from-resend-queue ((ws slack-team-ws) payload team)
  "Drop the entry matching PAYLOAD from the resend queue on WS for TEAM."
  (unless (slack-ws-payload-pong-p payload)
    (with-slots (waiting-send) ws
      (slack-log (format "waiting-send: %s" (length waiting-send))
                 team :level 'trace)
      (setq waiting-send
            (cl-remove-if #'(lambda (e) (eq (plist-get e :id)
                                       (plist-get payload :reply_to)))
                          waiting-send))
      (slack-log (format "waiting-send: %s" (length waiting-send))
                 team :level 'trace))))

(cl-defmethod slack-ws-handle-reply ((ws slack-team-ws) payload team)
  "Handle a websocket reply PAYLOAD for TEAM on WS, logging any error."
  (let ((ok (plist-get payload :ok)))
    (if (eq ok :json-false)
        (let* ((err (plist-get payload :error))
               (code (plist-get err :code))
               (msg (plist-get err :msg))
               (template "Failed to send message. Error code: %s msg: %s"))
          (slack-log (format template code msg) team :level 'error))
      (slack-event-update (slack-create-reply-event payload) team)
      (slack-ws-remove-from-resend-queue ws payload team))))

(defun slack-ws-handle-reaction-added (payload team)
  "Handle the Slack websocket `reaction added' event with PAYLOAD for TEAM."
  (slack-if-let* ((event (slack-create-reaction-event payload)))
      (slack-event-update event team)))

(defun slack-ws-handle-reaction-removed (payload team)
  "Handle the Slack websocket `reaction removed' event with PAYLOAD for TEAM."
  (slack-if-let* ((event (slack-create-reaction-event payload)))
      (slack-event-update event team)))

(defun slack-ws-handle-channel-created (payload team)
  "Handle the Slack websocket `channel created' event with PAYLOAD for TEAM."
  (slack-event-update (slack-create-channel-created-event payload)
                      team))

(defun slack-ws-handle-room-archive (payload team)
  "Handle the Slack websocket `room archive' event with PAYLOAD for TEAM."
  (slack-event-update (slack-create-room-archive-event payload)
                      team))

(defun slack-ws-handle-room-unarchive (payload team)
  "Handle the Slack websocket `room unarchive' event with PAYLOAD for TEAM."
  (slack-event-update (slack-create-room-unarchive-event payload)
                      team))

(defun slack-ws-handle-channel-deleted (payload team)
  "Handle the Slack websocket `channel deleted' event with PAYLOAD for TEAM."
  (slack-event-update (slack-create-channel-deleted-event payload)
                      team))

(defun slack-ws-handle-room-rename (payload team)
  "Handle the Slack websocket `room rename' event with PAYLOAD for TEAM."
  (slack-event-update (slack-create-room-rename-event payload)
                      team))

(defun slack-ws-handle-group-joined (payload team)
  "Handle the Slack websocket `group joined' event with PAYLOAD for TEAM."
  (slack-event-update (slack-create-group-joined-event payload)
                      team))

(defun slack-ws-handle-channel-joined (payload team)
  "Handle the Slack websocket `channel joined' event with PAYLOAD for TEAM."
  (slack-event-update (slack-create-channel-joined-event payload)
                      team))

(defun slack-ws-handle-presence-change (payload team)
  "Handle user presence changes for RTM API."
  (let ((presence (plist-get payload :presence))
        (users (plist-get payload :users)))
    (--each users (puthash it presence (oref team presence)))))

(defun slack-ws-handle-bot (payload team)
  "Handle the Slack websocket `bot' event with PAYLOAD for TEAM."
  (let ((bot (plist-get payload :bot)))
    (slack-team-set-bots team (list bot))))

(defun slack-ws-handle-file-created (payload team)
  "Handle the Slack websocket `file created' event with PAYLOAD for TEAM."
  (slack-if-let* ((file-id (plist-get (plist-get payload :file) :id))
                  (buffer (slack-buffer-find 'slack-file-list-buffer team)))
      (slack-file-request-info file-id 1 team
                               #'(lambda (file &rest _args)
                                   (slack-buffer-update buffer file)))))

(defun slack-ws-handle-file-deleted (payload team)
  "Handle the Slack websocket `file deleted' event with PAYLOAD for TEAM."
  (let ((file-id (plist-get payload :file_id)))
    (remhash file-id (oref team files))))

(defun slack-ws-handle-room-marked (payload team)
  "Handle the Slack websocket `room marked' event with PAYLOAD for TEAM."
  (slack-event-update (slack-create-room-marked-event payload)
                      team))

(defun slack-ws-handle-thread-marked (payload team)
  "Handle the Slack websocket `thread marked' event with PAYLOAD for TEAM."
  (let* ((type (plist-get payload :type)))
    (slack-counts-update team)
    (when (string= type "thread")
      (slack-event-update (slack-create-thread-marked-event payload)
                          team))))

(defun slack-ws-handle-thread-subscribed (payload team)
  "Handle the Slack websocket `thread subscribed' event with PAYLOAD for TEAM."
  (slack-event-update (slack-create-thread-subscribed-event payload)
                      team))

(defun slack-ws-handle-thread-unsubscribed (payload team)
  "Handle the Slack websocket `thread unsubscribed' event with PAYLOAD for TEAM."
  (slack-event-update (slack-create-thread-unsubscribed-event payload)
                      team))

(defun slack-ws-handle-user-change (payload team)
  "Handle the Slack websocket `user change' event with PAYLOAD for TEAM."
  (let ((user (plist-get payload :user)))
    (slack-team-set-users team (list user))))

(defun slack-ws-handle-member-joined-channel (payload team)
  "Handle the Slack websocket `member joined channel' event with PAYLOAD for TEAM."
  (slack-event-update (slack-create-member-joined-room-event payload)
                      team))

(defun slack-ws-handle-member-left_channel (payload team)
  "Handle the Slack websocket `member left channel' event with PAYLOAD for TEAM."
  (slack-event-update (slack-create-member-left-room-event payload)
                      team))

(defun slack-ws-handle-dnd-updated (payload team)
  "Handle the Slack websocket `dnd updated' event with PAYLOAD for TEAM."
  (let* ((user (plist-get payload :user))
         (payload (plist-get payload :dnd_status))
         (status (slack-create-dnd-status payload)))
    (puthash user status (oref team dnd-status))))

(defun slack-ws-handle-star-added (payload team)
  "Handle the Slack websocket `star added' event with PAYLOAD for TEAM."
  (slack-if-let* ((event (slack-create-star-event payload)))
      (slack-event-update event team)))

(defun slack-ws-handle-star-removed (payload team)
  "Handle the Slack websocket `star removed' event with PAYLOAD for TEAM."
  (slack-if-let* ((event (slack-create-star-event payload)))
      (slack-event-update event team)))

(defun slack-ws-handle-app-conversation-invite-request (payload team)
  "Handle the Slack websocket `app conversation invite request' event with PAYLOAD for TEAM."
  (let* ((app-user (plist-get payload :app_user))
         (channel-id (plist-get payload :channel_id))
         (invite-message-ts (plist-get payload :invite_message_ts))
         (scope-info (plist-get payload :scope_info))
         (room (slack-room-find channel-id team)))
    (if (yes-or-no-p (format "%s\n%s\n"
                             (format "%s would like to do following in %s"
                                     (slack-user-name app-user team)
                                     (slack-room-name room team))
                             (mapconcat #'(lambda (scope)
                                            (format "* %s"
                                                    (plist-get scope
                                                               :short_description)))
                                        scope-info
                                        "\n")))
        (slack-app-conversation-allow-invite-request team
                                                     :channel channel-id
                                                     :app-user app-user
                                                     :invite-message-ts invite-message-ts)
      (slack-app-conversation-deny-invite-request team
                                                  :channel channel-id
                                                  :app-user app-user
                                                  :invite-message-ts invite-message-ts))))

(cl-defun slack-app-conversation-allow-invite-request (team &key channel
                                                            app-user
                                                            invite-message-ts)
  "Approve the app invite to CHANNEL for APP-USER in TEAM.
INVITE-MESSAGE-TS identifies the original invite prompt."
  (let ((url "https://slack.com/api/apps.permissions.internal.add")
        (params (list (cons "channel" channel)
                      (cons "app_user" app-user)
                      (cons "invite_message_ts" invite-message-ts)
                      (cons "did_confirm" "true")
                      (cons "send_ephemeral_error_message" "true"))))
    (cl-labels
        ((log-error (err)
                    (slack-log (format "Error: %s, URL: %s, PARAMS: %s"
                                       err
                                       url
                                       params)
                               team :level 'error))
         (on-success (&key data &allow-other-keys)
                     (slack-request-handle-error
                      (data "slack-app-conversation-allow-invite-request"
                            #'log-error)
                      (message "DATA: %s" data))))
      (slack-request
       (slack-request-create
        url
        team
        :type "POST"
        :params params
        :success #'on-success)))))

(cl-defun slack-app-conversation-deny-invite-request (team &key channel
                                                           app-user
                                                           invite-message-ts)
  "Decline the app invite to CHANNEL for APP-USER in TEAM.
INVITE-MESSAGE-TS identifies the original invite prompt."
  (let ((url "https://slack.com/api/apps.permissions.internal.denyAdd")
        (params (list (cons "channel" channel)
                      (cons "app_user" app-user)
                      (cons "invite_message_ts" invite-message-ts))))
    (cl-labels
        ((log-error (err)
                    (slack-log (format "Error: %s, URL: %s, PARAMS: %s"
                                       err
                                       url
                                       params)
                               team :level 'error))
         (on-success (&key data &allow-other-keys)
                     (slack-request-handle-error
                      (data "slack-app-conversation-deny-invite-request"
                            #'log-error)
                      (message "DATA: %s" data))))
      (slack-request
       (slack-request-create
        url
        team
        :type "POST"
        :params params
        :success #'on-success)))))

(defun slack-ws-handle-commands-changed (payload team)
  "Handle the Slack websocket `commands changed' event with PAYLOAD for TEAM."
  (let ((commands-updated (mapcar #'slack-command-create
                                  (plist-get payload :commands_updated)))
        (commands-removed (mapcar #'slack-command-create
                                  (plist-get payload :commands_removed)))
        (commands '()))
    (cl-loop for command in (oref team commands)
             if (and (not (cl-find-if #'(lambda (e) (slack-equalp command e))
                                      commands-removed))
                     (not (cl-find-if #'(lambda (e) (slack-equalp command e))
                                      commands-updated)))
             do (push command commands))
    (cl-loop for command in commands-updated
             do (push command commands))
    (oset team commands commands)))

(defun slack-ws-handle-dialog-opened (payload team)
  "Handle the Slack websocket `dialog opened' event with PAYLOAD for TEAM."
  (slack-if-let*
      ((dialog-id (plist-get payload :dialog_id))
       (client-token (plist-get payload :client_token))
       (valid-client-tokenp (string= (slack-team-client-token team)
                                     client-token)))
      (slack-dialog-get dialog-id team)))

(defun slack-ws-handle-room-left (payload team)
  "Handle the Slack websocket `room left' event with PAYLOAD for TEAM."
  (slack-if-let* ((room (slack-room-find (plist-get payload :channel)
                                         team)))
      (progn
        (oset room is-member nil)
        (slack-log (format "You left %s" (slack-room-name room team))
                   team :level 'info))))

(defun slack-ws-handle-subteam-created (payload team)
  "Handle the Slack websocket `subteam created' event with PAYLOAD for TEAM."
  (let ((usergroup (slack-usergroup-create (plist-get payload :subteam))))
    (push usergroup (oref team usergroups))))

(defun slack-ws-handle-subteam-updated (payload team)
  "Handle the Slack websocket `subteam updated' event with PAYLOAD for TEAM."
  (let ((usergroup (slack-usergroup-create (plist-get payload :subteam))))
    (oset team usergroups (cons usergroup
                                (cl-remove-if #'(lambda (e)
                                                  (string= (oref e id)
                                                           (oref usergroup id)))
                                              (oref team usergroups))))))

(cl-defmethod slack-team-send-message ((this slack-team) message)
  "Send MESSAGE to THIS team websocket.
Note that the message type needs to be whitelisted in the or
statement to get a message id the ws can respond to."
  (if (or (slack-ws-payload-ping-p message)
          (slack-ws-payload-presence-sub-p message)
          (slack-ws-payload-presence-query-p message))
      (progn
        (slack-team-inc-message-id this)
        (with-slots (ws) this
          (slack-ws-send ws message this)))))

(cl-defmethod slack-team-open-ws ((this slack-team) &key on-open ws-url)
  "Open the websocket for team THIS, running ON-OPEN after, at WS-URL."
  (with-slots (ws) this
    (slack-ws-open ws this
                   :on-open on-open
                   :ws-url ws-url)))

(cl-defmethod slack-team-disconnect ((team slack-team))
  "Close the websocket connection for TEAM."
  (slack-ws--close (oref team ws) team))

(defun slack-team-delete ()
  "Prompt for a team and remove it from `slack-teams' after disconnecting."
  (interactive)
  (let ((selected (slack-team-select t)))
    (if (yes-or-no-p (format "Delete %s from `slack-teams'?"
                             (oref selected name)))
        (progn
          (slack-team--delete selected)
          (slack-team-disconnect selected)
          (message "Delete %s from `slack-teams'" (oref selected name))))))

(cl-defmethod slack-team-send-presence-subscription ((this slack-team) user-ids)
  "Subscribe to the user presence for THIS team USER-IDS."
  (slack-team-send-message this
                           (list :id (oref this message-id)
                                 :type "presence_sub"
                                 :ids user-ids)))

(cl-defmethod slack-team-send-presence-query ((this slack-team) user-ids)
  "Request the USER-IDS presence via websocket rtm api of THIS team."
  (slack-team-send-message this
                           (list :id (oref this message-id)
                                 :type "presence_query"
                                 :ids user-ids)))

(defun slack-team-presence-query-and-subscribe (team)
  "Query and subscribe to first 499 users presence status for TEAM.
499 is the maximum number supported by the websocket. The
query (subscription is limited) should rather be batched to cover
all users, but for simplicity we take the first users."
  ;; 499 is the websocket protocol maximum for presence_sub/presence_query
  (let ((first-499-users-ids (--> (oref team users)
                                  hash-table-values
                                  (--map (plist-get it :id) it)
                                  (append
                                   ;; Prioritize IM users: DM presence is most
                                   ;; visible in the UI (online dot in sidebar).
                                   (--keep
                                    (and
                                     (slack-room-open-p it)
                                     (oref it user))
                                    (slack-team-ims team))
                                   it)
                                  -distinct
                                  (-take 499 it))))
    (slack-team-send-presence-query team first-499-users-ids)
    (slack-team-send-presence-subscription team first-499-users-ids)
    (slack-log "Queried first 499 users presence via RTT"
               team :level 'trace)))

(defun slack-maybe-start-presence-timer (team)
  "If SLACK-EMIT-PERIODIC-PRESENCE-P is t, every 7s we call set-presence for TEAM.
That keeps the user active. It is clear this is a workaround, but it is
easier than inject activity in each action the user is doing that
represent activity."
  (when (and
         ;; the user wants the timer
         slack-emit-periodic-presence-p
         ;; the timer is not set already for the team
         (not (gethash (oref team id) slack-presence-timers))
         )
    (puthash (oref team id) (run-with-timer 7 t 'slack-request-set-presence team "auto") slack-presence-timers)))

(defun slack-authorize (team &optional error-callback success-callback)
  "Authorize TEAM with Slack, invoking ERROR-CALLBACK or SUCCESS-CALLBACK."
  (let ((authorize-request (oref team authorize-request)))
    (if (and authorize-request (not (request-response-done-p authorize-request)))
        (slack-log "Authorize Already Requested" team)
      (cl-labels
          ((on-error (&key error-thrown symbol-status response data)
             (oset team authorize-request nil)
             (message ">> Slack authorization failed for \"%s\": %s"
                      (oref team name) error-thrown)
             (slack-log (format "Authorize Failed: %s" error-thrown)
                        team)
             (when (functionp error-callback)
               (funcall error-callback
                        :error-thrown error-thrown
                        :symbol-status symbol-status
                        :response response
                        :data data)))
           (on-success (&key data &allow-other-keys)
             (oset team authorize-request nil)
             (slack-request-handle-error
              (data "slack-authorize")
              (slack-log "Authorization Finished" team)
              (if success-callback
                  (funcall success-callback data)
                (cl-labels
                    ((on-emoji-download (paths)
                       (oset team
                             emoji-download-watch-timer
                             (run-with-idle-timer 5 t
                                                  #'slack-team-watch-emoji-download-complete
                                                  team paths)))
                     (on-open ()
                       (slack-conversations-list-update team)
                       (slack-counts-update team)
                       ;; (slack-user-list-update team)
                       (slack-dnd-status-team-info team)
                       (when slack-buffer-emojify
                         (if (slack-native-emoji-p)
                             (progn
                               (slack-emoji-fetch-master-data-async team)
                               (slack-fetch-team-emojis team))
                           (slack-download-emoji team #'on-emoji-download)))
                       (slack-command-list-update team)
                       (slack-usergroup-list-update team)
                       (slack-update-modeline)
                       (slack-maybe-start-presence-timer team)))
                  (let ((self (plist-get data :self))
                        (team-data (plist-get data :team)))
                    (oset team id (plist-get team-data :id))
                    (oset team name (plist-get team-data :name))
                    (oset team self self)
                    (oset team self-id (plist-get self :id))
                    (oset team self-name (plist-get self :name))
                    (slack-team-set-ws-url team (plist-get data :url))
                    (oset team domain (plist-get team-data :domain)))
                  (puthash (oref team id) (oref team token) slack-tokens-by-id)
                  (slack-team-open-ws team :on-open #'on-open))))))
        (let ((request (slack-request
                        (slack-request-create
                         slack-rtm-connect-url
                         team
                         :params (list (cons "batch_presence_aware" "1")
                                       (cons "presence_sub" "true"))
                         :success #'on-success
                         :error #'on-error
                         :no-retry t))))
          (oset team authorize-request request))))))

(defun slack-conversations-list-update-quick (&optional team)
  "Like `slack-conversations-list-update' but uses userboot endpoint.
This way instead of getting all channels in the workspace, you
only get the ones you are a member of, which reduces the amount
of requests that are being made to Slack and therefore lowers the
risk of getting rate-limited.  Especially good for workspaces
with lots of public channels."
  (interactive)
  (let ((team (or team (slack-team-select))))
    (slack-request
     (slack-request-create
      slack-api-client-userboot-url
      team
      :data (list (cons "min_channel_updated"
                        (car (s-split "\\."
                                      (number-to-string (let ((six-months-in-seconds (* 6 30 24 60 60)))
                                                          (time-to-seconds (time-subtract (current-time) six-months-in-seconds))))))))
      :success
      (cl-function
       (lambda (&key data &allow-other-keys)
         (let ((channels nil)
               (groups nil)
               (ims nil))
           (cl-loop for channel in (plist-get data :channels)
                    do (let ((c (plist-put channel :is_member t)))
                         (cond
                          ((and
                            slack-exclude-archived-channels
                            (eq t (plist-get c :is_archived))))
                          ((eq t (plist-get c :is_channel))
                           (push (slack-room-create c 'slack-channel)
                                 channels))
                          ((eq t (plist-get c :is_im))
                           (push (slack-room-create c 'slack-im)
                                 ims))
                          ((eq t (plist-get c :is_group))
                           (push (slack-room-create c 'slack-group)
                                 groups)))))
           (slack-team-set-channels team channels)
           (slack-team-set-groups team groups)
           (slack-team-set-ims team ims)
           (slack-team-set-conversations-loaded team))))
      :error
      (cl-function
       (lambda (&key error-thrown symbol-status &allow-other-keys)
         (slack-log (format "conversations-list-update-quick: HTTP error %S (%S), marking loaded anyway"
                            error-thrown symbol-status)
                    team :level 'error)
         (slack-team-set-conversations-loaded team)))))))

(defalias 'slack-room-list-update 'slack-conversations-list-update)
(defun slack-conversations-list-update (&optional team after-success)
  "Refresh TEAM's list of conversations from the Slack API."
  (interactive)
  (message ">> slack-conversations-list-update running!")
  (let ((team (or team (slack-team-select))))
    (when slack-update-quick (slack-conversations-list-update-quick team))
    (cl-labels
        ((success (channels groups ims)
           (slack-team-set-channels team channels)
           (slack-team-set-groups team groups)
           (slack-team-set-ims team ims)
           (slack-team-set-conversations-loaded team)
           (slack-counts-update team)
           (slack-user-info-request
            (mapcar #'(lambda (im) (oref im user))
                    (slack-team-ims team))
            team)
           (when (functionp after-success)
             (funcall after-success team))
           (message ">> Slack is ready!")
           (slack-log "Slack Channel List Updated"
                      team :level 'info)
           (slack-log "Slack Group List Updated"
                      team :level 'info)
           (slack-log "Slack Im List Updated"
                      team :level 'info)))
      (slack-conversations-list--safe-for-rate-limiting team #'success
                                                        ))))

(defun slack-im-list-update (&optional team after-success)
  "Update TEAM list of private slack conversations.
Run AFTER-SUCCESS taking TEAM if provided."
  (interactive)
  (let ((team (or team (slack-team-select))))
    (cl-labels
        ((success (_channels _groups ims)
           (slack-team-set-ims team ims)
           (when (functionp after-success)
             (funcall after-success team))
           (slack-log "Slack Im List Updated"
                      team :level 'info)))
      (slack-conversations-list team #'success (list "im")))))

(provide 'slack-websocket)
;;; slack-websocket.el ends here
