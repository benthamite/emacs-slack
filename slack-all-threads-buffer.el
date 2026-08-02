;;; slack-all-threads-buffer.el ---                  -*- lexical-binding: t; -*-

;; Copyright (C) 2018

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
(require 'slack-buffer)
(require 'slack-create-message)
(require 'slack-message-buffer)

(defconst slack-subscriptions-thread-get-view-url "https://slack.com/api/subscriptions.thread.getView")
(defconst slack-subscriptions-thread-clear-all-url "https://slack.com/api/subscriptions.thread.clearAll")
(defconst slack-all-threads-buffer--page-key 'all-threads
  "Durable page-state key for the unified threads view.")

(define-derived-mode slack-all-threads-buffer-mode
  slack-buffer-mode
  "Slack All Threads"
  "Major mode for the Slack `all-threads' feed.

Message-region bindings (active when point is on a thread entry):
\\{slack-message-keymap}
Buffer-wide bindings:
\\{slack-all-threads-buffer-mode-map}"
  (add-hook 'lui-pre-output-hook 'slack-mrkdwn-add-face nil t)
  (add-hook 'lui-pre-output-hook 'slack-display-inline-action t t)
  (add-hook 'post-command-hook #'slack-buffer--maybe-load-more-at-end nil t)
  (cursor-sensor-mode))

(defface slack-all-thread-buffer-thread-header-face
  '((t (:weight bold :height 1.2)))
  "Face used to All Threads buffer's each threads header."
  :group 'slack)

(defclass slack-all-threads-buffer (slack-buffer)
  ((current-ts :initarg :current-ts :initform "0" :type string)
   (has-more :initarg :has-more :initform nil :type boolean)
   (total-unread-replies :initarg :total-unread-replies
                         :initform 0 :type integer)
   (new-threads-count :initarg :new-threads-count
                      :initform 0 :type integer)
   (threads :initarg :threads :type list '())))

(cl-defmethod slack-buffer-name ((this slack-all-threads-buffer))
  "Return the display buffer name for THIS buffer."
  (format "*slack: %s : All Threads"
          (slack-team-name (slack-buffer-team this))))

(cl-defmethod slack-buffer-key ((_class (subclass slack-all-threads-buffer)))
  "Return the class-level buffer key for the all threads buffer."
  'slack-all-threads-buffer)

(cl-defmethod slack-buffer-key ((_this slack-all-threads-buffer))
  "Return the lookup key identifying the buffer for the all threads buffer."
  (slack-buffer-key 'slack-all-threads-buffer))

(cl-defmethod slack-team-buffer-key ((_class (subclass slack-all-threads-buffer)))
  "Return the team-scoped class-level buffer key for the all threads buffer."
  'slack-all-threads-buffer)

(defclass slack-thread-view ()
  ((root-msg :initarg :root_msg :type slack-message)
   (latest-replies :initarg :latest_replies :type list :initform '())
   (unread-replies :initarg :unread_replies :type list :initform '())))

(cl-defmethod slack-message-user-ids ((this slack-thread-view))
  "Return the list of user IDs referenced by THIS thread view."
  (let ((ret (slack-message-user-ids (oref this root-msg))))

    (cl-loop for m in (oref this latest-replies)
             do (cl-loop for id in (slack-message-user-ids m)
                         do (cl-pushnew id ret :test #'string=)))

    (cl-loop for m in (oref this unread-replies)
             do (cl-loop for id in (slack-message-user-ids m)
                         do (cl-pushnew id ret :test #'string=)))
    ret))

(cl-defmethod slack-buffer-find-message ((this slack-all-threads-buffer) ts)
  "Return the message with timestamp TS across all threads in buffer THIS."
  (cl-block outer
    (cl-loop for thread in (reverse (oref this threads))
             do (cl-loop for message in (append (list (oref thread root-msg))
                                                (oref thread latest-replies)
                                                (oref thread unread-replies))
                         when (string= ts (slack-ts message))
                         do (cl-return-from outer message)))))

(cl-defmethod slack-buffer--replace ((this slack-all-threads-buffer) ts)
  "Replace the rendered message identified by the argument in THIS buffer.
TS is the ts argument."
  (let ((message (slack-buffer-find-message this ts)))
    (when message
      (slack-buffer-replace this message))))

(defun slack-create-thread-view (payload team)
  "Create and return a new thread view instance from PAYLOAD.
TEAM is the team argument."
  (let ((room (slack-room-find (plist-get (plist-get payload :root_msg)
                                          :channel)
                               team)))
    (make-instance 'slack-thread-view
                   :root_msg (slack-message-create (plist-get payload :root_msg) team room)
                   :latest_replies (mapcar #'(lambda (e) (slack-message-create e team room))
                                           (plist-get payload :latest_replies))
                   :unread_replies (mapcar #'(lambda (e) (slack-message-create e team room))
                                           (plist-get payload :unread_replies)))))

(defun slack-create-all-threads-buffer
    (team &optional total-unread-replies new-threads-count threads has-more
          current-ts)
  "Return TEAM's stable All Threads buffer.
TOTAL-UNREAD-REPLIES and NEW-THREADS-COUNT default to zero,
THREADS and HAS-MORE default to nil, and CURRENT-TS defaults to
the zero timestamp.  Existing live buffers retain their identity."
  (slack-if-let* ((buf (slack-buffer-find 'slack-all-threads-buffer team)))
      buf
    (let ((buf (slack-all-threads-buffer
                :team-id (oref team id)
                :total-unread-replies (or total-unread-replies 0)
                :new-threads-count (or new-threads-count 0)
                :threads (or threads nil)
                :has-more (and has-more t)
                :current-ts (or current-ts "0"))))
      (slack-buffer-cache-team buf team)
      buf)))

(cl-defmethod slack-buffer-init-buffer ((this slack-all-threads-buffer))
  "Initialize and return the display buffer for THIS buffer."
  (let ((buf (cl-call-next-method)))
    (with-current-buffer buf
      (slack-all-threads-buffer-mode)
      (slack-buffer-set-current-buffer this))
    buf))

(defun slack-all-threads--now-ts ()
  "Return the timestamp format expected by the All Threads API."
  (substring (number-to-string (time-to-seconds (current-time))) 0 15))

(defun slack-subscriptions-thread-get-view
    (team &optional current-ts after-success on-primary-page on-error)
  "Fetch TEAM's `all-threads' view at CURRENT-TS.
ON-PRIMARY-PAGE receives the same four page values as
AFTER-SUCCESS, but runs immediately after parsing and before missing
users are hydrated.  AFTER-SUCCESS retains its post-hydration timing.
ON-ERROR receives API or transport failures.  The first three
argument positions remain compatible with existing callers."
  (let ((current-ts (or current-ts
                        (slack-all-threads--now-ts)))
        primary-called-p)
    (cl-labels
        ((callback (total-unread-replies
                    new-threads-count
                    threads
                    has-more)
                   (when (functionp after-success)
                     (funcall after-success
                              total-unread-replies
                              new-threads-count
                              threads
                              has-more)))
         (fail (&rest errors)
           (when (functionp on-error)
             (apply on-error errors)))
         (success (&key data &allow-other-keys)
                  (slack-request-handle-error
                   (data "slack-subscriptions-thread-get-view" #'fail)
                   (let* ((total-unread-replies
                           (or (plist-get data :total_unread_replies) 0))
                          (new-threads-count
                           (or (plist-get data :new_threads_count) 0))
                          (threads
                           (mapcar
                            (lambda (payload)
                              (slack-create-thread-view payload team))
                            (plist-get data :threads)))
                          (has-more (eq (plist-get data :has_more) t))
                          (user-ids (slack-team-missing-user-ids
                                     team (cl-loop for thread in threads
                                                   nconc (slack-message-user-ids thread)))))
                     (when (and (not primary-called-p)
                                (functionp on-primary-page))
                       (setq primary-called-p t)
                       (funcall on-primary-page
                                total-unread-replies
                                new-threads-count
                                threads
                                has-more))
                     (if (< 0 (length user-ids))
                         (slack-users-info-request
                          user-ids team :after-success
                          #'(lambda () (callback total-unread-replies
                                                 new-threads-count
                                                 threads
                                                 has-more)))
                       (callback total-unread-replies
                                 new-threads-count
                                 threads
                                 has-more))))))
      (slack-request
       (slack-request-create
        slack-subscriptions-thread-get-view-url
        team
        :type "POST"
        :params (list (cons "current_ts" current-ts))
        :success #'success
        :error (lambda (&rest errors) (apply #'fail errors)))))))

(defun slack-all-threads--page-current-ts (threads fallback)
  "Return the pagination timestamp after THREADS, or FALLBACK when empty."
  (or (when-let* ((thread (car (last threads)))
                  (root (oref thread root-msg))
                  (last-read (oref root last-read))
                  ((stringp last-read)))
        last-read)
      fallback
      "0"))

(defun slack-all-threads--page-value
    (total-unread-replies new-threads-count threads has-more current-ts)
  "Build the durable All Threads value from the page arguments."
  (list :total-unread-replies (or total-unread-replies 0)
        :new-threads-count (or new-threads-count 0)
        :threads (or threads nil)
        :has-more (and has-more t)
        :current-ts (or current-ts "0")))

(defun slack-all-threads-buffer--apply-value (buffer value)
  "Copy durable All Threads VALUE into BUFFER's compatibility slots."
  (oset buffer total-unread-replies
        (or (plist-get value :total-unread-replies) 0))
  (oset buffer new-threads-count
        (or (plist-get value :new-threads-count) 0))
  (oset buffer threads (or (plist-get value :threads) nil))
  (oset buffer has-more (and (plist-get value :has-more) t))
  (oset buffer current-ts (or (plist-get value :current-ts) "0")))

(defun slack-all-threads-buffer--replace-live-contents (buffer state)
  "Replace exact BUFFER's output with its durable All Threads STATE."
  (when (and (slot-boundp buffer 'buf)
             (buffer-live-p (oref buffer buf)))
    (with-current-buffer (oref buffer buf)
      (let ((old-point (point)))
        (slack-buffer-widen
         (let ((inhibit-read-only t))
           (delete-region (point-min) lui-output-marker)
           (goto-char (point-min))
           (slack-buffer-with-deferred-hooks
             (let ((lui-time-stamp-position nil))
               (lui-insert (propertize "All Threads\n"
                                       'face '(:weight bold))
                           t))
             (when (slack-page-state-loaded-p state)
               (if-let ((threads (plist-get
                                  (slack-page-state-value state) :threads)))
                   (dolist (thread threads)
                     (slack-buffer-insert-thread buffer thread))
                 (let ((lui-time-stamp-position nil))
                   (lui-insert "No threads.\n" t)))))
           (goto-char (point-min))
           (slack-buffer-insert-page-status buffer state)
           (goto-char (min (max (point-min) old-point) (point-max)))))))))

(defun slack-all-threads-buffer-render-page-state (buffer state)
  "Render exact All Threads BUFFER from durable STATE."
  (when (slack-page-state-loaded-p state)
    (slack-all-threads-buffer--apply-value
     buffer (slack-page-state-value state)))
  (slack-all-threads-buffer--replace-live-contents buffer state))

(cl-defmethod slack-buffer-insert ((this slack-all-threads-buffer) message
                                   &optional not-tracked-p)
  "Insert MESSAGE into the `all-threads' buffer THIS.
Adds `room-id' property so `slack-feed-open-at-point' can find the channel."
  (let ((lui-time-stamp-format "[%Y-%m-%d %H:%M] ")
        (lui-time-stamp-time (slack-message-time-stamp message))
        (team (slack-buffer-team this)))
    (lui-insert-with-text-properties
     (slack-buffer--apply-message-keymap
      (slack-message-to-string message team))
     'not-tracked-p not-tracked-p
     'ts (slack-ts message)
     'room-id (oref message channel)
     'slack-last-ts lui-time-stamp-last
     'cursor-sensor-functions '(slack-buffer-subscribe-cursor-event))
    (lui-insert "" t)))

(cl-defmethod slack-buffer-insert-thread ((this slack-all-threads-buffer) thread)
  "Insert THREAD (header, root message and replies) into buffer THIS."
  (with-slots (root-msg latest-replies unread-replies) thread
    (oset this current-ts (oref root-msg last-read))
    (let* ((lui-time-stamp-position nil)
           (team (slack-buffer-team this))
           (channel (oref root-msg channel))
           (room (slack-room-find channel team))
           (prefix (or (and (slack-im-p room) "@") "#")))
      (lui-insert (slack-buffer-separator) t)
      (lui-insert (propertize (format "%s%s"
                                      prefix
                                      (slack-room-name room team))
                              'face
                              'slack-all-thread-buffer-thread-header-face)
                  t))

    (slack-buffer-insert this root-msg t)
    (cl-loop for reply in latest-replies
             do (slack-buffer-insert this reply t))
    (cl-loop for reply in unread-replies
             do (slack-buffer-insert this reply t))))

(cl-defmethod slack-buffer-has-next-page-p ((this slack-all-threads-buffer))
  "Return non-nil when THIS buffer has more history to load."
  (let* ((team (slack-buffer-team this))
         (state (and team
                     (slack-team-page-state
                      team slack-all-threads-buffer--page-key))))
    (if (and state (slack-page-state-loaded-p state))
        (and (slack-page-state-has-more state)
             (slack-page-state-continuation state))
      (oref this has-more))))

(cl-defmethod slack-buffer-delete-load-more-string ((_this slack-all-threads-buffer))
  "Remove the \"load more\" marker from the buffer for the all threads buffer.")

(cl-defmethod slack-buffer-prepare-marker-for-history ((_this slack-all-threads-buffer))
  "Position point so history can be inserted in the all threads buffer.")

(cl-defmethod slack-buffer-insert-history ((this slack-all-threads-buffer))
  "Insert historical messages into the buffer for THIS buffer."
  (with-slots (threads current-ts) this
    (cl-loop for thread in (cl-remove-if #'(lambda (e)
                                             (not (string< (oref (oref e root-msg)
                                                                 last-read)
                                                           current-ts)))
                                         threads)
             do (slack-buffer-insert-thread this thread))))

(cl-defmethod slack-buffer-insert--history ((this slack-all-threads-buffer))
  "Insert loaded history items into the buffer for THIS buffer."
  (slack-buffer-insert-history this))

(cl-defmethod slack-buffer-request-history ((this slack-all-threads-buffer) after-success &optional _on-error)
  "Request older history for THIS buffer from the Slack API.
AFTER-SUCCESS is the after-success argument."
  (let ((cur-point (point)))
    (with-slots (current-ts) this
      (cl-labels
          ((success (total-unread-replies new-threads-count threads has-more)
                    (oset this total-unread-replies total-unread-replies)
                    (oset this new-threads-count new-threads-count)
                    (oset this threads (append (oref this threads) threads))
                    (oset this has-more has-more)
                    (funcall after-success)
                    ;; The HTTP callback runs with an arbitrary buffer
                    ;; current; restore point in the feed buffer, not
                    ;; wherever the response happened to land.
                    (when (buffer-live-p (oref this buf))
                      (with-current-buffer (oref this buf)
                        (when (and (< (point-min) cur-point)
                                   (< cur-point (point-max)))
                          (goto-char cur-point))))))
        (slack-subscriptions-thread-get-view (slack-buffer-team this) current-ts #'success)))))

(defun slack-all-threads-buffer--commit-extension
    (state generation expected-current-ts value current-ts has-more)
  "Commit an All Threads page extension to ready STATE.
GENERATION and EXPECTED-CURRENT-TS must still identify the captured
page.  VALUE, CURRENT-TS, and HAS-MORE replace its durable data
without starting a new lifecycle generation."
  (when (and (= generation (slack-page-state-generation state))
             (= generation (slack-page-state-committed-generation state))
             (= generation (slack-page-state-ready-generation state))
             (eq 'ready (slack-page-state-status state))
             (slack-page-state-loaded-p state)
             (equal expected-current-ts
                    (slack-page-state-continuation state)))
    (setf (slack-page-state-value state) value
          (slack-page-state-continuation state) current-ts
          (slack-page-state-has-more state) has-more
          (slack-page-state-error state) nil
          (slack-page-state-updated-at state) (current-time))
    t))

(cl-defmethod slack-buffer-load-more ((this slack-all-threads-buffer))
  "Load All Threads' next page into THIS exact buffer and durable state."
  (let* ((team (slack-buffer-team this))
         (state (slack-team-page-state
                 team slack-all-threads-buffer--page-key)))
    (when (and (slack-buffer-has-next-page-p this)
               (not slack-buffer--loading-more-p)
               (eq 'ready (slack-page-state-status state))
               (slot-boundp this 'buf)
               (eq (current-buffer) (oref this buf)))
      (setq slack-buffer--loading-more-p t)
      (let* ((source-value (slack-page-state-value state))
             (old-threads (copy-sequence
                           (plist-get source-value :threads)))
             (generation (slack-page-state-generation state))
             (requested-current-ts
              (slack-page-state-continuation state))
             (buffer (current-buffer))
             primary-seen-p
             accepted-value)
        (cl-labels
            ((reset-loading-flag ()
               (when (buffer-live-p buffer)
                 (with-current-buffer buffer
                   (setq slack-buffer--loading-more-p nil))))
             (state-current-p ()
               (and (= generation (slack-page-state-generation state))
                    (eq 'ready (slack-page-state-status state))
                    (eq source-value (slack-page-state-value state))
                    (equal old-threads
                           (plist-get source-value :threads))
                    (equal requested-current-ts
                           (slack-page-state-continuation state))))
             (presentation-current-p ()
               (and (buffer-live-p buffer)
                    (slot-boundp this 'buf)
                    (eq buffer (oref this buf))))
             (accepted-current-p ()
               (and accepted-value
                    (= generation (slack-page-state-generation state))
                    (eq accepted-value (slack-page-state-value state))
                    (equal (plist-get accepted-value :current-ts)
                           (slack-page-state-continuation state))))
             (render-accepted ()
               (when (and (accepted-current-p)
                          (presentation-current-p))
                 (slack-all-threads-buffer-render-page-state this state)))
             (primary (total-unread-replies new-threads-count
                                            threads has-more)
               (unless primary-seen-p
                 (setq primary-seen-p t)
                 (condition-case primary-error
                     (if (state-current-p)
                         (let* ((all-threads (append old-threads threads))
                                (current-ts
                                 (slack-all-threads--page-current-ts
                                  threads requested-current-ts))
                                (candidate
                                 (slack-all-threads--page-value
                                  total-unread-replies new-threads-count
                                  all-threads has-more current-ts)))
                           (when (slack-all-threads-buffer--commit-extension
                                  state generation requested-current-ts
                                  candidate current-ts has-more)
                             (setq accepted-value candidate)
                             (render-accepted)))
                       (reset-loading-flag))
                   (error
                    (reset-loading-flag)
                    (message "slack-all-threads: page error: %S"
                             primary-error)))))
             (hydrated (&rest _args)
               (unwind-protect
                   (render-accepted)
                 (reset-loading-flag)))
             (failed (&rest _args)
               (reset-loading-flag)))
          (condition-case request-error
              (slack-subscriptions-thread-get-view
               team requested-current-ts #'hydrated #'primary #'failed)
            (error
             (reset-loading-flag)
             (signal (car request-error) (cdr request-error)))))))))


(cl-defmethod slack-buffer-display-thread ((this slack-all-threads-buffer) ts)
  "Open the thread associated with the message at point in THIS buffer.
TS is the ts argument."
  (with-slots (threads) this
    (slack-if-let* ((thread (cl-find-if #'(lambda (e) (string= ts (slack-ts (oref e root-msg))))
                                        threads))
                    (root (oref thread root-msg))
                    (room-id (oref root channel))
                    (team (slack-buffer-team this))
                    (room (slack-room-find room-id team)))
        (cl-labels
            ((display-thread (message)
                             (slack-thread-show-messages message
                                                         room
                                                         team)))
          (slack-if-let* ((ts (slack-ts root))
                          (message (slack-room-find-message room ts)))
              (display-thread message)
            (cl-labels
                ((success (messages _next-cursor)
                          (slack-room-set-messages room messages team)
                          (let ((message (slack-room-find-message room (slack-ts root))))
                            (display-thread message))))
              (slack-conversations-history room team
                                           :oldest (slack-ts root)
                                           :inclusive "true"
                                           :limit "1"
                                           :after-success #'success)))))))

(defun slack-all-threads-buffer--page-loader (team state)
  "Return an All Threads loader for TEAM's durable STATE."
  (lambda (generation success error)
    (let ((request-current-ts (slack-all-threads--now-ts))
          primary-seen-p
          unread-cleared-p)
      (cl-labels
          ((current-p ()
             (and (= generation (slack-page-state-generation state))
                  (slack-page-state-in-flight-p state)))
           (primary (total-unread-replies new-threads-count threads has-more)
             (unless primary-seen-p
               (setq primary-seen-p t)
               (when (current-p)
                 (let* ((current-ts
                         (slack-all-threads--page-current-ts
                          threads request-current-ts))
                        (value
                         (slack-all-threads--page-value
                          total-unread-replies new-threads-count
                          threads has-more current-ts)))
                   (when (funcall success value current-ts has-more t)
                     (unless unread-cleared-p
                       (setq unread-cleared-p t)
                       (condition-case clear-error
                           (slack-subscriptions-thread-clear-all team)
                         (error
                          (message
                           "slack-all-threads: unread clear failed: %S"
                           clear-error)))))))))
           (hydrated (&rest _args)
             (when (and (current-p)
                        (= generation
                           (slack-page-state-committed-generation state)))
               (slack-page-state-ready state generation)))
           (failed (&rest errors)
             (when (current-p)
               (if errors
                   (apply error errors)
                 (funcall error "All Threads request failed")))))
        (condition-case request-error
            (slack-subscriptions-thread-get-view
             team request-current-ts #'hydrated #'primary #'failed)
          (error
           (funcall error request-error)))))))

(defun slack-all-threads-buffer--present (team refresh)
  "Present TEAM's All Threads page, reloading when REFRESH is non-nil."
  (let* ((state (slack-team-page-state
                 team slack-all-threads-buffer--page-key))
         (buffer (slack-create-all-threads-buffer team)))
    (slack-buffer-present-page
     buffer state
     (slack-all-threads-buffer--page-loader team state)
     #'slack-all-threads-buffer-render-page-state
     refresh)
    buffer))

(defun slack-all-threads ()
  "Open or refresh the selected team's unified `all-threads' view."
  (interactive)
  (slack-all-threads-buffer--present (slack-team-select) t))

(defun slack-subscriptions-thread-clear-all (team)
  "Clear all thread subscription unread state for TEAM via the Slack API."
  (let ((current-ts (substring
                     (number-to-string (time-to-seconds (current-time)))
                     0 15)))
    (cl-labels
        ((success (&key data &allow-other-keys)
                  (slack-request-handle-error
                   (data "slack-subscriptions-thread-clear-all"))))
      (slack-request
       (slack-request-create
        slack-subscriptions-thread-clear-all-url
        team
        :type "POST"
        :params (list (cons "current_ts" current-ts))
        :success #'success)))))

(cl-defmethod slack-all-threads-buffer-find-thread ((buf slack-all-threads-buffer) ts)
  "Find the `slack-thread-view' containing TS in BUF.
TS may belong to a root message or any reply within a thread."
  (cl-find-if
   (lambda (thr)
     (or (string= ts (slack-ts (oref thr root-msg)))
         (cl-find ts (oref thr latest-replies)
                  :key #'slack-ts :test #'string=)
         (cl-find ts (oref thr unread-replies)
                  :key #'slack-ts :test #'string=)))
   (oref buf threads)))

(cl-defmethod slack-feed--open ((buf slack-all-threads-buffer) ts)
  "Open the thread containing TS in `all-threads' buffer BUF.
TS may belong to a root message or any reply within a thread."
  (if-let* ((thread (slack-all-threads-buffer-find-thread buf ts)))
      (slack-buffer-display-thread buf (slack-ts (oref thread root-msg)))
    (message "Thread not found for ts %s" ts)))

(defun slack-all-threads-unfollow-at-point ()
  "Unfollow the thread at point.
Works from both `slack-all-threads-buffer' and
`slack-thread-message-buffer'."
  (interactive)
  (let ((buf slack-current-buffer))
    (cond
     ((and buf (object-of-class-p buf 'slack-all-threads-buffer))
      (slack-all-threads--unfollow-from-feed buf))
     ((and buf (object-of-class-p buf 'slack-thread-message-buffer))
      (slack-all-threads--unfollow-from-thread buf))
     (t (message "Not in a thread buffer")))))

(defun slack-all-threads--unfollow-from-feed (buf)
  "Unfollow thread at point in `all-threads' BUF."
  (if-let* ((ts (get-text-property (point) 'ts))
            (thread (slack-all-threads-buffer-find-thread buf ts))
            (root (oref thread root-msg))
            (team (slack-buffer-team buf))
            (room (slack-room-find (oref root channel) team)))
      (slack-all-threads-unfollow-thread room (slack-ts root) team)
    (message "No thread at point")))

(defun slack-all-threads--unfollow-from-thread (buf)
  "Unfollow the thread displayed in thread-message BUF."
  (slack-all-threads-unfollow-thread
   (slack-buffer-room buf) (oref buf thread-ts) (slack-buffer-team buf)))

(defun slack-all-threads-unfollow-thread (room ts team)
  "Unfollow thread TS in ROOM on TEAM."
  (cl-labels
      ((after-success ()
                      (slack-log "Unfollowed thread" team :level 'info)
                      (message "Unfollowed thread")))
    (slack-subscriptions-thread-remove room ts team
                                       #'after-success)))

(define-key slack-all-threads-buffer-mode-map (kbd "RET") 'slack-feed-open-at-point)
(define-key slack-all-threads-buffer-mode-map (kbd "n") 'slack-feed-goto-next)
(define-key slack-all-threads-buffer-mode-map (kbd "p") 'slack-feed-goto-prev)
(define-key slack-all-threads-buffer-mode-map (kbd "g") 'slack-all-threads)
(define-key slack-all-threads-buffer-mode-map (kbd "U") 'slack-all-threads-unfollow-at-point)

(provide 'slack-all-threads-buffer)
;;; slack-all-threads-buffer.el ends here
