;;; slack-scheduled-messages-buffer.el --- List and manage scheduled drafts -*- lexical-binding: t; -*-

;; Copyright (C) 2025

;; Author:  <andrea-dev@hotmail.com>
;;; Commentary:
;;
;; This buffer allows you to list, schedule, and delete scheduled messages (drafts) for Slack.
;; Invoke with =slack-scheduled-messages-show=.
;; Schedule a new message with =slack-schedule-message=.
;; This version uses the internal drafts API.

;;; Code:

(require 'eieio)
(require 'json)
(require 'slack-util)
(require 'slack-buffer)
(require 'slack-team)
(require 'slack-room)
(require 'dash)
(require 's)

(declare-function uuidgen "uuidgen")

;; Internal API Endpoints
(defvar slack-drafts-create-url "https://slack.com/api/drafts.create")
(defvar slack-drafts-list-url "https://slack.com/api/drafts.list")
(defvar slack-drafts-delete-url "https://slack.com/api/drafts.delete")

;; A fixed boundary string for multipart requests, similar to slack-activity-feed-buffer.el
(defvar slack-scheduled-messages--boundary "----WebKitFormBoundaryEmacsSlackScheduled"
  "Boundary string for multipart/form-data requests.")

;;; API Request Functions

(defun slack-scheduled-messages--team (&optional team)
  "Return TEAM, `slack-current-team', or prompt for a team."
  (or team
      slack-current-team
      (slack-team-select)))

(defun slack-scheduled-messages--draft-blocks-json (text)
  "Encode scheduled draft TEXT blocks as JSON."
  (json-encode
   (vector
    `((type . "rich_text")
      (elements . [((type . "rich_text_section")
                    (elements . [((type . "text")
                                  (text . ,text))]))])))))

(defun slack-scheduled-messages--destinations-json (channel-id)
  "Encode CHANNEL-ID destinations as JSON."
  (json-encode
   (vector
    `((channel_id . ,channel-id)))))

(defun slack--build-multipart-part (name value)
  "Helper to build one part of a multipart/form-data body.
NAME is the name argument.
VALUE is the value argument."
  (format "Content-Disposition: form-data; name=\"%s\"\r\n\r\n%s" name value))

(defun slack--build-multipart-body (parts)
  "Build the full multipart/form-data body from an alist of PARTS."
  (let* ((boundary slack-scheduled-messages--boundary)
         (body-parts (--map (slack--build-multipart-part (car it) (cdr it)) parts)))
    (concat "--" boundary
            (format "\r\n%s\r\n" (s-join (format "\r\n--%s\r\n" boundary) body-parts))
            "--" boundary "--\r\n")))

(defun slack-schedule-message-request (team channel-id text post-at after-success)
  "Create a scheduled draft with TEXT for CHANNEL-ID in TEAM at POST-AT time."
  (let ((data-parts
         `(("token" . ,(oref team token))
           ("blocks" . ,(slack-scheduled-messages--draft-blocks-json text))
           ("destinations" . ,(slack-scheduled-messages--destinations-json channel-id))
           ("date_scheduled" . ,post-at)
           ("is_from_composer" . "true")
           ("_x_reason" . "schedule-draft")
           ("_x_mode" . "online")
           ("_x_sonic" . "true")
           ("client_msg_id" . ,(with-temp-buffer (uuidgen t) (buffer-string)))
           ("file_ids" . "[]"))))
    (slack-request
     (slack-request-create
      slack-drafts-create-url team
      :type "POST"
      :headers `(("content-type" . ,(format "multipart/form-data; boundary=%s" slack-scheduled-messages--boundary)))
      :data (slack--build-multipart-body data-parts)
      :success after-success
      :error (lambda (&rest data)
               (slack-log (format "Error scheduling message: %s" data) team :level 'error))
      ))))

(defun slack-list-scheduled-messages-request
    (team after-success &optional on-error)
  "Request a list of scheduled drafts for TEAM.
AFTER-SUCCESS is the after-success argument.  ON-ERROR receives API
transport failures."
  (let ((data-parts
         `(("token" . ,(oref team token))
           ("is_active" . "true")
           ("limit" . "100")
           ("_x_reason" . "client-v2-boot-team")
           ("_x_mode" . "online")
           ("_x_sonic" . "true"))))
    (slack-request
     (slack-request-create
      slack-drafts-list-url team
      :type "POST"
      :headers `(("content-type" . ,(format "multipart/form-data; boundary=%s" slack-scheduled-messages--boundary)))
      :data (slack--build-multipart-body data-parts)
      :success after-success
      :error (lambda (&rest args)
               (when (functionp on-error)
                 (apply on-error args)))))))

(defun slack-delete-scheduled-message-request (team draft-id last-updated-ts after-success)
  "Delete a scheduled draft with DRAFT-ID in TEAM."
  (let ((data-parts
         `(("token" . ,(oref team token))
           ("draft_id" . ,draft-id)
           ("_x_reason" . "DeleteDraftModal")
           ("_x_mode" . "online")
           ("_x_sonic" . "true")
           ("_x_app_name" . "client")
           ;; The API wants the dot placed before the final 3 digits;
           ;; short or dot-less values pass through unchanged instead
           ;; of producing a negative substring position.
           ("client_last_updated_ts" . ,(let* ((str (s-join "" (s-split "\\." last-updated-ts)))
                                               (pos (- (length str) 3)))
                                          (if (< 0 pos)
                                              (concat (s-left pos str) "." (s-right (- (length str) pos) str))
                                            last-updated-ts))))))
    (slack-request
     (slack-request-create
      slack-drafts-delete-url team
      :type "POST"
      :headers `(("content-type" . ,(format "multipart/form-data; boundary=%s" slack-scheduled-messages--boundary)))
      :data (slack--build-multipart-body data-parts)
      :success after-success))))

;;; Data and Buffer Classes

(defclass slack-scheduled-message ()
  ((draft-id :initarg :draft-id :type string)
   (channel-id :initarg :channel-id :type string)
   (post-at :initarg :post-at :type integer)
   (last-updated-ts :initarg :last-updated-ts :type string)
   (text :initarg :text :type string)))

(defclass slack-scheduled-messages-buffer (slack-buffer)
  ((messages :initarg :messages :type list)))

(defconst slack-scheduled-messages-buffer--page-key 'scheduled-messages
  "Durable page-state key for scheduled drafts.")

(define-derived-mode slack-scheduled-messages-buffer-mode slack-buffer-mode "Slack Scheduled"
  "Major mode for listing scheduled Slack messages (drafts).")

;;; Class Methods

(cl-defmethod slack-buffer-name ((this slack-scheduled-messages-buffer))
  "Return the display buffer name for THIS buffer."
  (format "*slack %s Scheduled Msgs*" (slack-team-name (slack-buffer-team this))))

(cl-defmethod slack-buffer-key ((_class (subclass slack-scheduled-messages-buffer)) &rest _args)
  "Return the class-level buffer key for the scheduled-messages buffer."
  "scheduled-messages")

(cl-defmethod slack-buffer-key ((_this slack-scheduled-messages-buffer))
  "Return the lookup key identifying the buffer for THIS buffer."
  "scheduled-messages")

(cl-defmethod slack-team-buffer-key ((_class (subclass slack-scheduled-messages-buffer)))
  "Return the team-scoped buffer key for the scheduled messages buffer."
  'slack-scheduled-messages-buffer)

(cl-defmethod slack-scheduled-message-to-string ((msg slack-scheduled-message) team)
  "Format a scheduled message for display.
MSG is the msg argument.
TEAM is the team argument."
  (with-slots (draft-id channel-id post-at last-updated-ts text) msg
    (let ((room (slack-room-find channel-id team)))
      (propertize
       (format "#%s at %s: %s"
               (or (and room (slack-room-name room team)) channel-id "unknown-channel")
               (format-time-string "%Y-%m-%d %H:%M" (seconds-to-time post-at))
               text)
       'team-id (oref team id)
       'channel-id channel-id
       'draft-id draft-id
       'last-updated-ts last-updated-ts))))

(cl-defmethod slack-buffer-insert ((this slack-scheduled-messages-buffer) msg)
  "Insert a single scheduled message MSG into the buffer THIS."
  (let ((team (slack-buffer-team this)))
    (lui-insert (slack-scheduled-message-to-string msg team))
    (lui-insert "\n\n")))

(defun slack-create-scheduled-messages-buffer (team)
  "Return TEAM's stable scheduled-messages buffer object."
  (let ((buffer
         (or (slack-buffer-find 'slack-scheduled-messages-buffer team)
             (make-instance 'slack-scheduled-messages-buffer
                            :team-id (oref team id)
                            :messages nil))))
    (slack-buffer-cache-team buffer team)
    buffer))

(cl-defmethod slack-buffer-init-buffer ((this slack-scheduled-messages-buffer))
  "Initialize THIS buffer."
  (let ((buffer (cl-call-next-method)))
    (with-current-buffer buffer
      (slack-scheduled-messages-buffer-mode)
      (slack-buffer-set-current-buffer this)
      (setq-local mode-line-format
                  '(" "
                    mode-line-buffer-identification "   "
                    "(=d=elete, =g= refresh, =q= quit)")))
    buffer))



(cl-defmethod slack-buffer-has-next-page-p ((_this slack-scheduled-messages-buffer))
  "Return non-nil when the scheduled messages buffer has more history to load.")

(cl-defmethod slack-buffer-insert-history ((_this slack-scheduled-messages-buffer))
  "Insert historical messages into the buffer for the scheduled messages buffer.")

(cl-defmethod slack-buffer-request-history ((_this slack-scheduled-messages-buffer) _after-success &optional _on-error)
  "Request older history for the scheduled messages buffer from the Slack API.")

(cl-defmethod slack-buffer-loading-message-end-point ((_this slack-scheduled-messages-buffer))
  "Return the buffer position where the loading indicator ends in the scheduled messages buffer.")

(cl-defmethod slack-buffer-delete-load-more-string ((_this slack-scheduled-messages-buffer))
  "Remove the \"load more\" marker from the buffer for the scheduled messages
buffer.")

(cl-defmethod slack-buffer-prepare-marker-for-history ((_this slack-scheduled-messages-buffer))
  "Position point so history can be inserted in the scheduled messages buffer.")

(cl-defmethod slack-buffer-insert--history ((_this slack-scheduled-messages-buffer))
  "Insert loaded history items into the buffer for the scheduled messages buffer.")

(defun slack-scheduled-messages-parse (payload)
  "Parse and sort scheduled messages from list-response PAYLOAD."
  (let ((messages
         (cl-loop
          for draft in (plist-get payload :drafts)
          for date-scheduled = (plist-get draft :date_scheduled)
          when (and date-scheduled (> date-scheduled 0))
          collect
          (make-instance
           'slack-scheduled-message
           :draft-id (plist-get draft :id)
           :channel-id
           (or (--> (plist-get draft :destinations)
                    car (plist-get it :channel_id))
               "??")
           :post-at date-scheduled
           :last-updated-ts (plist-get draft :last_updated_ts)
           :text
           (or (--> (plist-get draft :blocks)
                    car (plist-get it :elements)
                    car (plist-get it :elements)
                    car (plist-get it :text))
               "??")))))
    (sort messages (lambda (a b) (< (oref a post-at) (oref b post-at))))))

(defun slack-scheduled-messages-buffer-render-page-state (buffer state)
  "Render exact scheduled BUFFER from durable STATE."
  (when (and (slot-boundp buffer 'buf)
             (buffer-live-p (oref buffer buf)))
    (when (slack-page-state-loaded-p state)
      (oset buffer messages (slack-page-state-value state)))
    (with-current-buffer (oref buffer buf)
      (slack-buffer-widen
       (let ((inhibit-read-only t))
         (delete-region (point-min) lui-output-marker)
         (when (slack-page-state-loaded-p state)
           (if (oref buffer messages)
               (dolist (message (oref buffer messages))
                 (slack-buffer-insert buffer message))
             (lui-insert "(No scheduled messages.)\n")))
         (goto-char (point-min))
         (slack-buffer-insert-page-status buffer state)
         (goto-char (point-min)))))))

(defun slack-scheduled-messages-buffer--page-loader (team)
  "Return the immediate-ready scheduled-messages loader for TEAM."
  (lambda (_generation success error)
    (slack-list-scheduled-messages-request
     team
     (lambda (&rest response)
       (let ((payload (plist-get response :data)))
         (slack-request-handle-error
          (payload "slack-scheduled-messages-show"
                   (lambda (api-error) (funcall error api-error)))
          (let ((normalized
                 (slack-request-normalize-response
                  (lambda () (slack-scheduled-messages-parse payload))
                  error)))
            (when normalized
              (funcall success (cdr normalized) nil nil))))))
     (lambda (&rest errors)
       (apply error errors)))))

(defun slack-scheduled-messages-buffer--present (team refresh)
  "Present TEAM's scheduled messages, reloading when REFRESH is non-nil."
  (let* ((state (slack-team-page-state
                 team slack-scheduled-messages-buffer--page-key))
         (buffer (slack-create-scheduled-messages-buffer team)))
    (slack-buffer-present-page
     buffer state
     (slack-scheduled-messages-buffer--page-loader team)
     #'slack-scheduled-messages-buffer-render-page-state
     refresh)
    buffer))

(defun slack-scheduled-messages-buffer--refresh-after-mutation
    (team buffer emacs-buffer)
  "Refresh TEAM's exact scheduled BUFFER after a successful mutation.
EMACS-BUFFER is the display buffer that existed when the mutation completed.
If a list request is already running, wait for it to finish and then start a
new request so a stale pre-mutation response cannot become final."
  (cl-labels
      ((current-buffer-p ()
         (and (eq buffer
                  (slack-buffer-find 'slack-scheduled-messages-buffer team))
              (slot-boundp buffer 'buf)
              (eq emacs-buffer (oref buffer buf))
              (buffer-live-p emacs-buffer))))
    (when (current-buffer-p)
      (let ((state (slack-team-page-state
                    team slack-scheduled-messages-buffer--page-key)))
        (if (slack-page-state-in-flight-p state)
            (let ((refreshed nil))
              (cl-labels
                  ((refresh (&rest _ignored)
                     (unless refreshed
                       (setq refreshed t)
                       (when (current-buffer-p)
                         (slack-scheduled-messages-buffer--present team t)))))
                (slack-page-state-on-ready state #'refresh)
                (slack-page-state-on-error state #'refresh)))
          (slack-scheduled-messages-buffer--present team t))))))


;;; Interactive Functions

(defun slack-scheduled-messages-show (&optional team)
  "Show scheduled messages (drafts) for TEAM in a dedicated buffer."
  (interactive)
  (slack-scheduled-messages-buffer--present
   (slack-scheduled-messages--team team) t))

(defun slack-scheduled-messages-refresh-buffer ()
  "Refresh the current scheduled-messages buffer in place."
  (interactive)
  (if (cl-typep slack-current-buffer 'slack-scheduled-messages-buffer)
      (slack-scheduled-messages-buffer--present
       (slack-buffer-team slack-current-buffer) t)
    (user-error "Current buffer is not a Slack scheduled-messages buffer")))


(defun slack-schedule-message (channel-id text minutes-from-now &optional team)
  "Schedule TEXT to CHANNEL-ID in MINUTES-FROM-NOW as a draft for TEAM."
  (interactive
   (let* ((team (slack-scheduled-messages--team))
          (room (slack-room-select
                 (append (slack-team-ims team)
                         (slack-team-groups team)
                         (slack-team-channels team))
                 team)))
     (list (oref room id)
           (read-string "Message: ")
           (read-number "Minutes from now: " 30)
           team)))
  (let* ((team (slack-scheduled-messages--team team))
         (post-at (format "%d" (floor (+ (float-time) (* minutes-from-now 60))))))
    (slack-schedule-message-request
     team
     channel-id
     text
     post-at
     (lambda (&rest data)
       (if (not (plist-get (plist-get data :data) :error))
           (progn
             (message "Message scheduled for %s" (format-time-string "%H:%M:%S" (seconds-to-time (string-to-number post-at))))
             (let ((buffer
                    (slack-buffer-find
                     'slack-scheduled-messages-buffer team)))
               (when (and buffer (slot-boundp buffer 'buf))
                 (slack-scheduled-messages-buffer--refresh-after-mutation
                  team buffer (oref buffer buf)))))
         (message "Failed to schedule message: %S" data))))))

(defun slack-scheduled-messages-delete-at-point ()
  "Delete the scheduled message (draft) at point."
  (interactive)
  (if-let* ((buffer (and (cl-typep slack-current-buffer
                                   'slack-scheduled-messages-buffer)
                          slack-current-buffer))
            (emacs-buffer (current-buffer))
            (team-id (get-text-property (point) 'team-id))
            (draft-id (get-text-property (point) 'draft-id))
            (team (slack-team-find team-id))
            (last-updated-ts (get-text-property (point) 'last-updated-ts)))
      (when (y-or-n-p (format "Delete scheduled message: %s?" (buffer-substring-no-properties (line-beginning-position) (line-end-position))))
        (slack-delete-scheduled-message-request
         team draft-id last-updated-ts
         (lambda (&rest data)
           (if (not (plist-get (plist-get data :data) :error))
               (progn
                 (message "Scheduled message deleted.")
                 (slack-scheduled-messages-buffer--refresh-after-mutation
                  team buffer emacs-buffer))
             (message "Failed to delete message: %S" data)))))
    (message "No scheduled message at point.")))

(define-key slack-scheduled-messages-buffer-mode-map (kbd "d") #'slack-scheduled-messages-delete-at-point)
(define-key slack-scheduled-messages-buffer-mode-map (kbd "g") #'slack-scheduled-messages-refresh-buffer)

(provide 'slack-scheduled-messages-buffer)
;;; slack-scheduled-messages-buffer.el ends here
