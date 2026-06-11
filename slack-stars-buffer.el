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
  (let ((pending (list 0)))
    (dolist (item star-items)
      (let* ((ts (oref item ts))
             (room-id (oref item item-id))
             (room (slack-room-find room-id team)))
        (when (and room (not (slack-room-find-message room ts)))
          (cl-incf (car pending)))))
    (if (= 0 (car pending))
        (funcall callback)
      (dolist (item star-items)
        (slack-stars--prefetch-single item team pending callback)))))

(defun slack-stars--prefetch-single (item team pending callback)
  "Prefetch a single ITEM for TEAM, decrementing PENDING and calling CALLBACK."
  (let* ((ts (oref item ts))
         (thread-ts (oref item thread-ts))
         (room-id (oref item item-id))
         (room (slack-room-find room-id team))
         (is-reply (and thread-ts (not (string-equal ts thread-ts)))))
    (when (and room (not (slack-room-find-message room ts)))
      (condition-case err
          (let ((fail (lambda (&rest _)
                        (when (= 0 (cl-decf (car pending)))
                          (funcall callback)))))
            (if is-reply
                (slack-stars--prefetch-reply room ts thread-ts team pending callback fail)
              (slack-stars--prefetch-from-history room ts team pending callback fail)))
        (error
         (message "slack-stars: prefetch error for %s: %S" ts err)
         (when (= 0 (cl-decf (car pending)))
           (funcall callback)))))))

(defun slack-stars--prefetch-done (room team pending callback)
  "Return a callback that stores MESSAGES in ROOM for TEAM.
Calls CALLBACK when PENDING reaches zero."
  (lambda (messages &rest _)
    (when messages
      (slack-room-set-messages room messages team))
    (when (= 0 (cl-decf (car pending)))
      (funcall callback))))

(defun slack-stars--prefetch-reply (room ts thread-ts team pending callback fail)
  "Prefetch reply at TS in thread THREAD-TS from ROOM for TEAM.
Decrement PENDING and call CALLBACK when done, or FAIL on error."
  (slack-conversations-replies
   room thread-ts team
   :oldest ts
   :inclusive "true"
   :limit "1"
   :after-success (slack-stars--prefetch-done room team pending callback)
   :on-error fail))

(defun slack-stars--prefetch-from-history (room ts team pending callback fail)
  "Prefetch message at TS from ROOM history for TEAM.
If the returned message has a different timestamp, the saved item
is a thread reply whose thread-ts was not provided by the API.
In that case, resolve the real thread parent via
`chat.getPermalink' and retry with `conversations.replies'.
Decrement PENDING and call CALLBACK when done, or FAIL on error."
  (slack-conversations-history
   room team
   :latest ts
   :inclusive "true"
   :limit "1"
   :after-success
   (lambda (messages &rest _)
     (let ((got (and messages (car messages))))
       (if (or (null got) (string-equal ts (slack-ts got)))
           (progn
             (when messages
               (slack-room-set-messages room messages team))
             (when (= 0 (cl-decf (car pending)))
               (funcall callback)))
         (slack-stars--resolve-thread-and-prefetch
          room ts team pending callback fail))))
   :on-error fail))

(defconst slack-stars--permalink-url "https://slack.com/api/chat.getPermalink")

(defun slack-stars--resolve-thread-and-prefetch (room ts team pending callback fail)
  "Resolve the thread parent of reply TS in ROOM via permalink, then prefetch.
Uses a synchronous permalink request because chained async
requests do not complete reliably in the `request' library.
TEAM, PENDING, CALLBACK, and FAIL are forwarded to the prefetch."
  (let ((thread-ts (slack-stars--permalink-thread-ts room ts team)))
    (if thread-ts
        (slack-stars--prefetch-reply
         room ts thread-ts team pending callback fail)
      (funcall fail))))

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
  (let ((team (slack-buffer-team this)))
    (slack-star-has-next-page-p (oref team star))))

(cl-defmethod slack-buffer-delete-load-more-string ((_this slack-stars-buffer))
  "Remove the \"load more\" marker from the buffer for the stars buffer.")

(cl-defmethod slack-stars--insert-items ((this slack-stars-buffer) star-items)
  "Insert messages for STAR-ITEMS into THIS buffer.
File-type items carry the file id in `item-id', never a room, so
they get their own insert path instead of being silently dropped."
  (let ((team (slack-buffer-team this)))
    (cl-loop for i in star-items
             for file = (oref i file)
             for room = (and (not file)
                             (slack-room-find (oref i item-id) team))
             for m = (and room (slack-room-find-message room (oref i ts)))
             do (cond (file (slack-stars--insert-file-item this i file))
                      (m (slack-buffer-insert this m))))))

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

(cl-defmethod slack-buffer-load-more ((this slack-stars-buffer))
  "Load the next page of saved items and append at the bottom.
THIS is the slack-stars-buffer instance."
  (when (and (slack-buffer-has-next-page-p this)
             (not slack-buffer--loading-more-p))
    (setq slack-buffer--loading-more-p t)
    (let* ((team (slack-buffer-team this))
           (star (oref team star))
           (old-count (length (slack-star-items star))))
      (slack-stars-list-request
       team (oref star cursor)
       (lambda ()
         (let ((new-items (nthcdr old-count (slack-star-items star))))
           (slack-stars--prefetch-messages
            new-items team
            (lambda ()
              (with-current-buffer (slack-buffer-buffer this)
                (let ((inhibit-read-only t))
                  (slack-stars--insert-items this new-items)
                  (slack-stars--insert-tail this))
                (setq slack-buffer--loading-more-p nil))))))))))

(cl-defmethod slack-buffer-init-buffer ((this slack-stars-buffer))
  "Initialize and return the display buffer for THIS buffer."
  (let* ((buf (cl-call-next-method))
         (team (slack-buffer-team this))
         (star (oref team star))
         (items (slack-star-items star)))
    (with-current-buffer buf
      (slack-stars-buffer-mode)
      (slack-buffer-set-current-buffer this))
    (slack-stars--prefetch-messages
     items team
     (lambda ()
       (with-current-buffer buf
         (let ((inhibit-read-only t))
           (slack-stars--insert-items this items)
           (slack-stars--insert-tail this))
         (goto-char (point-min)))))
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

(cl-defmethod slack-buffer-remove-star ((this slack-stars-buffer) ts)
  "Remove THIS star at TS."
  (let ((team (slack-buffer-team this)))
    (with-slots (star) team
      (slack-star-remove-star star ts team)
      (slack-team-mark-unsaved team ts)
      (when-let* ((room-id (get-text-property (point) 'room-id))
                  (room (slack-room-find room-id team))
                  (message (slack-room-find-message room ts)))
        (slack-message-star-removed message))
      (slack-buffer-message-delete this ts))))

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
         (buf (slack-buffer-find 'slack-stars-buffer team)))
    (if buf (slack-buffer-display buf)
      (slack-stars-list-request
       team nil
       #'(lambda () (slack-buffer-display (slack-create-stars-buffer team)))))))

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
  "Close and reopen saved items buffer to refresh contents."
  (interactive)
  (kill-buffer)
  (slack-saved-items))

(defalias 'slack-stars-refresh-buffer 'slack-saved-items-refresh-buffer)
(define-key slack-stars-buffer-mode-map (kbd "G") 'slack-saved-items-refresh-buffer)

(provide 'slack-stars-buffer)
;;; slack-stars-buffer.el ends here
