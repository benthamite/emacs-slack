;;; slack-stars-buffer.el ---                        -*- lexical-binding: t; -*-

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
(require 'slack-util)
(require 'slack-team)

(declare-function slack-star-item-file "slack-star")
(require 'slack-buffer)
(require 'slack-room-buffer)
(require 'slack-message-buffer)
(require 'slack-star)
(require 'slack-message)

(define-derived-mode slack-stars-buffer-mode slack-buffer-mode "Slack Saved Items"
  "Major mode for the Slack saved items feed.

Message-region bindings (active when point is on a saved item):
\\{slack-message-keymap}
Buffer-wide bindings:
\\{slack-stars-buffer-mode-map}"
  (add-hook 'post-command-hook #'slack-buffer--maybe-load-more-at-end nil t))

(defclass slack-stars-buffer (slack-room-buffer)
  ())

(defconst slack-stars-buffer--page-key 'saved-items
  "Durable page-state key for the saved-items index.")

(cl-defmethod slack-buffer-room ((this slack-stars-buffer))
  "Return the room for THIS message at point.
Each saved item may belong to a different room, so the room is
resolved from the `room-id' text property."
  (let ((team (slack-buffer-team this))
        (room-id (get-text-property (point) 'room-id)))
    (when room-id
      (slack-room-find room-id team))))

(defun slack-stars--prefetch-messages (star-items team callback)
  "Prefetch uncached messages for STAR-ITEMS in parallel, then call CALLBACK.
Fires async API requests for all messages not already in cache,
and invokes CALLBACK (with no arguments) once every request has
completed (or immediately if all messages are cached).
TEAM is the team argument."
  (let* ((tasks
          (cl-remove-if-not
           (lambda (item)
             (let* ((ts (oref item ts))
                    (room (slack-room-find (oref item item-id) team)))
               (and room (not (slack-room-find-message room ts)))))
           star-items))
         (barrier (slack-async-barrier-create (length tasks) callback)))
    (dolist (item tasks)
      (slack-stars--prefetch-single item team barrier))))

(defun slack-stars--prefetch-single (item team barrier)
  "Prefetch one saved ITEM for TEAM and settle BARRIER exactly once."
  (let* ((ts (oref item ts))
         (thread-ts (oref item thread-ts))
         (room-id (oref item item-id))
         (room (slack-room-find room-id team))
         (is-reply (and thread-ts (not (string-equal ts thread-ts))))
         settled-p)
    (cl-labels
        ((done (&rest _ignored)
           (unless settled-p
             (setq settled-p t)
             (slack-async-barrier-done barrier))))
      (condition-case err
          (if is-reply
              (slack-stars--prefetch-reply room ts thread-ts team #'done)
            (slack-stars--prefetch-from-history room ts team #'done))
        (error
         (message "slack-stars: prefetch error for %s: %S" ts err)
         (done))))))

(defun slack-stars--prefetch-done (room team done)
  "Return a callback that stores MESSAGES in ROOM for TEAM, then calls DONE."
  (lambda (messages &rest _)
    (condition-case cache-error
        (when messages
          (slack-room-set-messages room messages team))
      (error
       (message "slack-stars: message cache error: %S" cache-error)))
    (funcall done)))

(defun slack-stars--prefetch-reply (room ts thread-ts team done)
  "Prefetch reply at TS in thread THREAD-TS from ROOM for TEAM.
Call DONE exactly once for this saved item on success or failure."
  (condition-case request-error
      (slack-conversations-replies
       room thread-ts team
       :oldest ts
       :inclusive "true"
       :limit "1"
       :after-success (slack-stars--prefetch-done room team done)
       :on-error done)
    (error
     (message "slack-stars: reply prefetch error for %s: %S"
              ts request-error)
     (funcall done))))

(defun slack-stars--prefetch-from-history (room ts team done)
  "Prefetch message at TS from ROOM history for TEAM.
If the returned message has a different timestamp, the saved item
is a thread reply whose thread-ts was not provided by the API.
In that case, resolve the real thread parent via
`chat.getPermalink' and retry with `conversations.replies'.
Call DONE exactly once for this saved item on success or failure."
  (condition-case request-error
      (slack-conversations-history
       room team
       :latest ts
       :inclusive "true"
       :limit "1"
       :after-success
       (lambda (messages &rest _)
         (condition-case callback-error
             (let ((got (and messages (car messages))))
               (if (or (null got) (string-equal ts (slack-ts got)))
                   (progn
                     (condition-case cache-error
                         (when messages
                           (slack-room-set-messages room messages team))
                       (error
                        (message "slack-stars: message cache error: %S"
                                 cache-error)))
                     (funcall done))
                 (slack-stars--resolve-thread-and-prefetch
                  room ts team done)))
           (error
            (message "slack-stars: history callback error for %s: %S"
                     ts callback-error)
            (funcall done))))
       :on-error done)
    (error
     (message "slack-stars: history prefetch error for %s: %S"
              ts request-error)
     (funcall done))))

(defconst slack-stars--permalink-url "https://slack.com/api/chat.getPermalink")

(defun slack-stars--resolve-thread-and-prefetch (room ts team done)
  "Resolve the thread parent of reply TS in ROOM via permalink, then prefetch.
Uses a synchronous permalink request because chained async
requests do not complete reliably in the `request' library.
TEAM and DONE are forwarded to the reply prefetch."
  (condition-case request-error
      (let ((thread-ts (slack-stars--permalink-thread-ts room ts team)))
        (if thread-ts
            (slack-stars--prefetch-reply room ts thread-ts team done)
          (funcall done)))
    (error
     (message "slack-stars: thread resolution error for %s: %S"
              ts request-error)
     (funcall done))))

(defun slack-stars--permalink-thread-ts (room ts team)
  "Return the thread-ts for message TS in ROOM for TEAM, or nil.
Fetches the permalink synchronously and extracts thread_ts from
the URL."
  (condition-case nil
      (let* ((resp (slack-request
                    (slack-request-create
                     slack-stars--permalink-url team
                     :params (list (cons "channel" (oref room id))
                                   (cons "message_ts" ts))
                     :sync t)))
             (data (and resp (request-response-data (oref resp response)))))
        (slack-stars--extract-thread-ts (plist-get data :permalink)))
    (error nil)))

(defun slack-stars--extract-thread-ts (permalink)
  "Extract thread_ts from a Slack PERMALINK URL, or nil."
  (when (and permalink
             (string-match "thread_ts=\\([0-9.]+\\)" permalink))
    (match-string 1 permalink)))

(cl-defmethod slack-buffer-name ((this slack-stars-buffer))
  "Return the display buffer name for THIS buffer."
  (let ((team (slack-buffer-team this)))
    (format "*slack: %s : Saved items*" (oref team name))))

(cl-defmethod slack-buffer-key ((_class (subclass slack-stars-buffer)) &rest _args)
  "Return the class-level buffer key for the stars buffer."
  'slack-stars-buffer)

(cl-defmethod slack-buffer-key ((_this slack-stars-buffer))
  "Return the lookup key identifying the buffer for the stars buffer."
  (slack-buffer-key 'slack-stars-buffer))

(cl-defmethod slack-team-buffer-key ((_class (subclass slack-stars-buffer)))
  "Return the team-scoped class-level buffer key for the stars buffer."
  'slack-stars-buffer)

(cl-defmethod slack-buffer-toggle-email-expand ((this slack-stars-buffer) file-id)
  "Toggle the expanded/collapsed state of THIS buffer.
FILE-ID is the file-id argument."
  (slack-if-let* ((team (slack-buffer-team this))
                  (ts (get-text-property (point) 'ts))
                  (items (slack-star-items (oref team star)))
                  (item (cl-find-if #'(lambda (e) (string= ts (slack-ts e)))
                                    items))
                  (file (slack-star-item-file item file-id)))
      (progn
        (oset file is-expanded (not (oref file is-expanded)))
        (slack-buffer--replace this ts))))

(cl-defmethod slack-buffer-insert ((this slack-stars-buffer) message &optional not-tracked-p)
  "Insert a rendered representation of THIS buffer into the current buffer.
MESSAGE is the message argument."
  (let ((lui-time-stamp-format "[%Y-%m-%d %H:%M] ")
        (lui-time-stamp-time (seconds-to-time
                              (string-to-number
                               (slack-ts message)))))
    (lui-insert-with-text-properties
     (slack-buffer--apply-message-keymap
      (slack-message-to-string message (slack-buffer-team this)))
     'ts (slack-ts message)
     'team-id (oref (slack-buffer-team this) id)
     'room-id (oref message channel)
     'thread-ts (oref message thread-ts)
     'not-tracked-p not-tracked-p)
    (lui-insert "" t)))

(cl-defmethod slack-buffer-has-next-page-p ((this slack-stars-buffer))
  "Return non-nil when THIS buffer has more history to load."
  (let* ((team (slack-buffer-team this))
         (state (slack-team-page-state team slack-stars-buffer--page-key))
         (cursor (slack-page-state-continuation state)))
    (and (slack-page-state-has-more state)
         cursor
         (not (string-empty-p cursor)))))

(cl-defmethod slack-buffer-delete-load-more-string ((_this slack-stars-buffer))
  "Remove the \"load more\" marker from the buffer for the stars buffer.")

(cl-defmethod slack-stars--insert-items
    ((this slack-stars-buffer) star-items &optional missing-label)
  "Insert messages for STAR-ITEMS into THIS buffer.
File-type items carry the file id in `item-id', never a room, so
they get their own insert path instead of being silently dropped.
When MISSING-LABEL is non-nil, insert a placeholder for an
uncached message instead of dropping its saved-index row."
  (let ((team (slack-buffer-team this)))
    (cl-loop for i in star-items
             for file = (oref i file)
             for room = (and (not file)
                             (slack-room-find (oref i item-id) team))
             for m = (and room (slack-room-find-message room (oref i ts)))
             do (cond (file (slack-stars--insert-file-item this i file))
                      (m (slack-buffer-insert this m))
                      (missing-label
                       (slack-stars--insert-missing-item
                        this i missing-label))))))

(defun slack-stars--insert-missing-item (buffer item label)
  "Insert saved ITEM's missing-message LABEL into BUFFER."
  (let ((lui-time-stamp-format "[%Y-%m-%d %H:%M] ")
        (lui-time-stamp-time
         (seconds-to-time (string-to-number (oref item ts)))))
    (lui-insert-with-text-properties
     label
     'ts (oref item ts)
     'team-id (oref (slack-buffer-team buffer) id)
     'room-id (oref item item-id)
     'thread-ts (oref item thread-ts))
    (lui-insert "" t)))

(cl-defmethod slack-stars--insert-file-item ((this slack-stars-buffer) item file)
  "Insert a saved FILE entry for ITEM into THIS buffer."
  (let ((lui-time-stamp-format "[%Y-%m-%d %H:%M] ")
        (lui-time-stamp-time (seconds-to-time
                              (string-to-number (oref item ts)))))
    (lui-insert-with-text-properties
     (slack-message-to-string file (oref item ts) (slack-buffer-team this))
     'ts (oref item ts)
     'file-id (oref file id))
    (lui-insert "" t)))

(cl-defmethod slack-stars--insert-tail ((this slack-stars-buffer))
  "Insert end-of-list marker when all items have been loaded.
THIS is the slack-stars-buffer instance."
  (unless (slack-buffer-has-next-page-p this)
    (let ((lui-time-stamp-position nil))
      (lui-insert "(no more items)\n" t))))

(defun slack-stars-buffer--missing-label (state)
  "Return the missing saved-message label appropriate for STATE."
  (if (slack-page-state-in-flight-p state)
      "Loading saved message…"
    "Saved message unavailable."))

(defun slack-stars-buffer--replace-live-contents (buffer state)
  "Replace BUFFER's live output with its durable saved-items STATE."
  (when (buffer-live-p (oref buffer buf))
    (with-current-buffer (oref buffer buf)
      (slack-buffer-widen
       (let ((inhibit-read-only t))
         (delete-region (point-min) lui-output-marker)
         (goto-char (point-min))
         (when (slack-page-state-loaded-p state)
           (let ((star (slack-page-state-value state)))
             (slack-stars--insert-items
              buffer (slack-star-items star)
              (slack-stars-buffer--missing-label state))
             (slack-stars--insert-tail buffer)))
         (goto-char (point-min))
         (slack-buffer-insert-page-status buffer state)
         (goto-char (point-min)))))))

(defun slack-stars-buffer-render-page-state (buffer state)
  "Render exact BUFFER from durable saved-items STATE."
  (when (slack-page-state-loaded-p state)
    (oset (slack-buffer-team buffer) star
          (slack-page-state-value state)))
  (slack-stars-buffer--replace-live-contents buffer state))

(defun slack-stars-buffer--append-items (buffer items state)
  "Append saved ITEMS to exact BUFFER using durable STATE."
  (when (buffer-live-p (oref buffer buf))
    (with-current-buffer (oref buffer buf)
      (let ((inhibit-read-only t))
        (slack-stars--insert-items
         buffer items (slack-stars-buffer--missing-label state))
        (slack-stars--insert-tail buffer)))))

(defun slack-stars-buffer--commit-extension
    (state generation expected-cursor value cursor has-more)
  "Commit a saved-items page extension to ready STATE.
GENERATION and EXPECTED-CURSOR must still identify the captured
page.  VALUE, CURSOR, and HAS-MORE become its new durable data
without starting a new page lifecycle generation."
  (when (and (= generation (slack-page-state-generation state))
             (= generation (slack-page-state-committed-generation state))
             (= generation (slack-page-state-ready-generation state))
             (eq 'ready (slack-page-state-status state))
             (slack-page-state-loaded-p state)
             (equal expected-cursor
                    (slack-page-state-continuation state)))
    (setf (slack-page-state-value state) value
          (slack-page-state-continuation state) cursor
          (slack-page-state-has-more state) has-more
          (slack-page-state-error state) nil
          (slack-page-state-updated-at state) (current-time))
    t))

(cl-defmethod slack-buffer-load-more ((this slack-stars-buffer))
  "Load the next page of saved items and append at the bottom.
THIS is the slack-stars-buffer instance.  The in-flight flag is reset
on request failure too, and a buffer killed while the request is in
flight stays dead instead of being re-created."
  (let* ((team (slack-buffer-team this))
         (state (slack-team-page-state team slack-stars-buffer--page-key)))
    (when (and (slack-buffer-has-next-page-p this)
               (not slack-buffer--loading-more-p)
               (eq 'ready (slack-page-state-status state))
               (eq (current-buffer) (oref this buf)))
      (setq slack-buffer--loading-more-p t)
      (let* ((source-star (slack-page-state-value state))
             (old-items (copy-sequence (slack-star-items source-star)))
             (generation (slack-page-state-generation state))
             (requested-cursor (slack-page-state-continuation state))
             (buffer (current-buffer))
             primary-seen-p
             accepted-star
             accepted-items
             new-items)
        (cl-labels
            ((reset-loading-flag ()
               (when (buffer-live-p buffer)
                 (with-current-buffer buffer
                   (setq slack-buffer--loading-more-p nil))))
             (state-current-p ()
               (and (= generation (slack-page-state-generation state))
                    (eq 'ready (slack-page-state-status state))
                    (eq source-star (slack-page-state-value state))
                    (equal old-items (slack-star-items source-star))
                    (equal requested-cursor
                           (slack-page-state-continuation state))))
             (presentation-current-p ()
               (and (buffer-live-p buffer)
                    (eq buffer (oref this buf))))
             (accepted-current-p ()
               (and accepted-star
                    (= generation (slack-page-state-generation state))
                    (eq accepted-star (slack-page-state-value state))
                    (equal (oref accepted-star cursor)
                           (slack-page-state-continuation state))))
             (accepted-items-current-p ()
               (equal accepted-items (slack-star-items accepted-star)))
             (primary (page stored-star)
               (unless primary-seen-p
                 (setq primary-seen-p t)
                 (condition-case primary-error
                     (if (state-current-p)
                         (let* ((page-items (slack-star-items page))
                                (all-items (append old-items page-items))
                                (candidate
                                 (make-instance
                                  'slack-star
                                  :items all-items
                                  :cursor (oref page cursor))))
                           (when (slack-stars-buffer--commit-extension
                                  state generation requested-cursor
                                  candidate (oref page cursor)
                                  (slack-star-has-next-page-p page))
                             (setq new-items page-items
                                   accepted-star candidate
                                   accepted-items
                                   (copy-sequence all-items))
                             (oset team star candidate)))
                       (when (eq (oref team star) stored-star)
                         (oset team star (slack-page-state-value state)))
                       (reset-loading-flag))
                   (error
                    (reset-loading-flag)
                    (message "slack-stars: saved page error: %S"
                             primary-error)))))
             (hydrated ()
               (if (accepted-current-p)
                   (slack-stars--prefetch-messages
                    new-items team
                    (lambda ()
                      (unwind-protect
                          (when (and (accepted-current-p)
                                     (presentation-current-p))
                            (if (accepted-items-current-p)
                                (slack-stars-buffer--append-items
                                 this new-items state)
                              (slack-stars-buffer-render-page-state
                               this state)))
                        (reset-loading-flag))))
                 (reset-loading-flag)))
             (failed (&rest _args)
               (reset-loading-flag)))
          (condition-case request-error
              (slack-stars-list-request
               team requested-cursor #'hydrated #'failed #'primary)
            (error
             (reset-loading-flag)
             (signal (car request-error) (cdr request-error)))))))))

(cl-defmethod slack-buffer-init-buffer ((this slack-stars-buffer))
  "Initialize and return the display buffer for THIS buffer."
  (let ((buf (cl-call-next-method)))
    (with-current-buffer buf
      (slack-stars-buffer-mode)
      (slack-buffer-set-current-buffer this))
    buf))

(defun slack-create-stars-buffer (team)
  "Create and return a new stars buffer instance from PAYLOAD.
TEAM is the team argument."
  (slack-if-let* ((buf (slack-buffer-find 'slack-stars-buffer team)))
      buf
    (let ((buf (make-instance 'slack-stars-buffer
                              :team-id (oref team id)
                              :room-id "__saved-items__")))
      (slack-buffer-cache-team buf team)
      buf)))

(defun slack-stars-buffer--page-loader (team state)
  "Return a saved-items loader for TEAM's durable STATE."
  (lambda (generation success error)
    (let (primary-star hydration-started-p)
      (cl-labels
          ((current-p ()
             (and (= generation (slack-page-state-generation state))
                  (slack-page-state-in-flight-p state)))
           (primary (_page stored-star)
             (if (current-p)
                 (progn
                   (setq primary-star stored-star)
                   (funcall success
                            primary-star
                            (oref primary-star cursor)
                            (slack-star-has-next-page-p primary-star)
                            t))
               (when (eq (oref team star) stored-star)
                 (oset team star (slack-page-state-value state)))))
           (hydrated ()
             (when (and (not hydration-started-p)
                        (current-p)
                        (= generation
                           (slack-page-state-committed-generation state)))
               (setq hydration-started-p t)
               (slack-stars--prefetch-messages
                (slack-star-items primary-star) team
                (lambda ()
                  (when (current-p)
                    (slack-page-state-ready state generation))))))
           (failed (&rest errors)
             (when (current-p)
               (apply error errors))))
        (slack-stars-list-request
         team nil #'hydrated #'failed #'primary)))))

(defun slack-stars-buffer--present (team refresh)
  "Present TEAM's saved-items page, reloading it when REFRESH is non-nil."
  (let* ((state (slack-team-page-state team slack-stars-buffer--page-key))
         (buffer (slack-create-stars-buffer team)))
    (slack-buffer-present-page
     buffer state
     (slack-stars-buffer--page-loader team state)
     #'slack-stars-buffer-render-page-state
     refresh)
    buffer))

(cl-defmethod slack-buffer-remove-star ((this slack-stars-buffer) ts)
  "Remove THIS star at TS."
  (let* ((team (slack-buffer-team this))
         (room-id (get-text-property (point) 'room-id))
         (file-id (get-text-property (point) 'file-id))
         (item-type (cond (room-id "message") (file-id "file")))
         (item-id (or room-id file-id))
         (room (and room-id (slack-room-find room-id team)))
         (message (and room (slack-room-find-message room ts)))
         (file (and file-id (slack-file-find file-id team))))
    (with-slots (star) team
      (slack-star-remove-star
       star ts team item-type item-id
       (lambda ()
         (slack-team-mark-unsaved team ts item-type item-id)
         (when message
           (slack-message-star-removed message))
         (when file
           (slack-message-star-removed file))
         (when (or message file)
           (lambda ()
             (let ((saved-p
                    (slack-ts-saved-p team ts item-type item-id)))
               (dolist (object (delq nil (list message file)))
                 (if saved-p
                     (slack-message-star-added object)
                   (slack-message-star-removed object))))))))
      (let ((state (slack-team-page-state
                    team slack-stars-buffer--page-key)))
        (if (and (slack-page-state-loaded-p state)
                 (eq (slack-page-state-value state) star))
            (slack-stars-buffer-render-page-state this state)
          (slack-buffer-message-delete this ts))))))

(cl-defmethod slack-buffer-message-delete ((this slack-stars-buffer) ts)
  "Delete the message at point from THIS buffer.
TS is the ts argument."
  (let ((buffer (slack-buffer-buffer this))
        (inhibit-read-only t))
    (with-current-buffer buffer
      (slack-if-let* ((beg (slack-buffer-ts-eq (point-min) (point-max) ts))
                      (end (next-single-property-change beg 'ts)))
          (delete-region beg end)))))

(cl-defmethod slack-buffer--replace ((this slack-stars-buffer) ts)
  "Replace the message at TS in THIS buffer.
Locates the message by its `ts' text property rather than
`lui-replace', which cannot navigate backward past separator
lines inserted by `slack-buffer-insert'."
  (let ((team (slack-buffer-team this)))
    (with-slots (star) team
      (slack-if-let*
          ((star-items (slack-star-items star))
           (star-item (cl-find-if (lambda (i) (string= (oref i ts) ts))
                                  star-items))
           (room (slack-room-find (oref star-item item-id) team))
           (item (slack-room-find-message room ts))
           (pos (slack-buffer-ts-eq (point-min) (point-max) ts)))
          (save-excursion
            (goto-char pos)
            (lui-replace-message
             (slack-message-to-string item team)))))))

;;;###autoload
(defun slack-saved-items ()
  "Show the saved items buffer."
  (interactive)
  (let* ((team (slack-team-select))
         (_ (slack-team-ensure-conversations-loaded team)))
    (slack-stars-buffer--present team t)))

;;;###autoload
(defalias 'slack-stars-list 'slack-saved-items)

(cl-defmethod slack-buffer-display-thread ((this slack-stars-buffer) ts)
  "Open the thread of the saved item at TS in THIS buffer.
The room is resolved from the `room-id' text property at point,
since each saved item may belong to a different room."
  (slack-if-let* ((team (slack-buffer-team this))
                  (room (slack-buffer-room this))
                  (message (slack-room-find-message room ts)))
      (slack-thread-show-messages message room team)
    (error "Not possible to open thread")))

(cl-defmethod slack-feed--open ((_buf slack-stars-buffer) ts)
  "Open the saved item at TS in its channel or thread buffer."
  (if-let* ((room-id (get-text-property (point) 'room-id))
            (buf slack-current-buffer)
            (team (slack-buffer-team buf))
            (room (slack-room-find room-id team)))
      (let ((thread-ts (get-text-property (point) 'thread-ts)))
        (if thread-ts
            (slack-open-message team room thread-ts thread-ts ts)
          (slack-open-message team room ts nil ts)))
    (error "Not possible to jump to message")))

(defalias 'slack-saved-items-open-message 'slack-feed-open-at-point)
(defalias 'slack-stars-open-message 'slack-feed-open-at-point)
(define-key slack-stars-buffer-mode-map (kbd "RET") 'slack-feed-open-at-point)
(define-key slack-stars-buffer-mode-map (kbd "n") 'slack-feed-goto-next)
(define-key slack-stars-buffer-mode-map (kbd "p") 'slack-feed-goto-prev)

(defun slack-message-remove-from-saved ()
  "Remove the saved item at point."
  (interactive)
  (slack-if-let* ((buffer slack-current-buffer))
      (progn
        (slack-buffer-remove-star buffer (slack-get-ts))
        (message "Message removed from saved"))))

(defalias 'slack-message-remove-star 'slack-message-remove-from-saved)
(define-key slack-stars-buffer-mode-map (kbd "K") 'slack-message-remove-from-saved)

(defun slack-saved-items-refresh-buffer ()
  "Refresh the current saved-items buffer without replacing it."
  (interactive)
  (if (cl-typep slack-current-buffer 'slack-stars-buffer)
      (slack-stars-buffer--present
       (slack-buffer-team slack-current-buffer) t)
    (user-error "Current buffer is not a Slack saved-items buffer")))

(defalias 'slack-stars-refresh-buffer 'slack-saved-items-refresh-buffer)
(define-key slack-stars-buffer-mode-map (kbd "G") 'slack-saved-items-refresh-buffer)

(provide 'slack-stars-buffer)
;;; slack-stars-buffer.el ends here
