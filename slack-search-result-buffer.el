;;; slack-search-result-buffer.el ---                -*- lexical-binding: t; -*-

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
(require 'slack-buffer)
(require 'slack-search)
(require 'slack-room-buffer)

(declare-function slack-search-empty-pagination "slack-search")

(define-derived-mode slack-search-result-buffer-mode slack-buffer-mode "Slack Search Result"
  "Major mode for a Slack search results buffer.

Message-region bindings (active when point is on a result message):
\\{slack-message-keymap}
Buffer-wide bindings:
\\{slack-search-result-buffer-mode-map}"
  (remove-hook 'lui-post-output-hook 'slack-display-image t)
  (add-hook 'post-command-hook #'slack-buffer--maybe-load-more-at-end nil t))

(defclass slack-search-result-buffer (slack-buffer)
  ((search-result :initarg :search-result :type slack-search-result)))

(defun slack-search-result-page-key (search-result)
  "Return the durable query key for SEARCH-RESULT."
  (with-slots (query sort sort-dir) search-result
    (list 'search
          (if (slack-file-search-result-p search-result) 'file 'messages)
          query sort sort-dir)))

(defun slack-search-result-empty (class query sort sort-dir)
  "Return an empty CLASS result shell for QUERY, SORT, and SORT-DIR."
  (make-instance class
                 :query query
                 :sort sort
                 :sort-dir sort-dir
                 :total 0
                 :matches nil
                 :pagination (slack-search-empty-pagination)))

(defun slack-search-result-next-page (search-result)
  "Return SEARCH-RESULT's next page number, or nil when exhausted."
  (slack-search-paging-next-page (oref search-result pagination)))

(cl-defmethod slack-buffer-name ((_class (subclass slack-search-result-buffer)) search-result team)
  "Return the display buffer name for the search result buffer.
SEARCH-RESULT is the search-result argument.
TEAM is the team argument."
  (with-slots (query sort sort-dir) search-result
    (format "*slack: %s : %s Search Result - QUERY: %s, ORDER BY: %s %s"
            (oref team name)
            (if (slack-file-search-result-p search-result)
                "File"
              "Message")
            query
            sort
            (upcase sort-dir))))

(cl-defmethod slack-buffer-name ((this slack-search-result-buffer))
  "Return the display buffer name for THIS buffer."
  (with-slots (search-result) this
    (with-slots (query sort sort-dir) search-result
      (format "*slack: %s : %s Search Result - QUERY: %s, ORDER BY: %s %s"
              (slack-team-name (slack-buffer-team this))
              (if (slack-file-search-result-p search-result)
                  "File"
                "Message")
              query
              sort
              (upcase sort-dir)))))

(cl-defmethod slack-buffer-key ((_class (subclass slack-search-result-buffer)) search-result)
  "Return the class-level buffer key for the search result buffer.
SEARCH-RESULT is the search-result argument."
  (with-slots (query sort sort-dir) search-result
    (concat query
            ":"
            (if (slack-file-search-result-p search-result)
                "File"
              "Message")
            ":"
            sort
            ":"
            sort-dir)))

(cl-defmethod slack-buffer-key ((this slack-search-result-buffer))
  "Return the lookup key identifying the buffer for THIS buffer."
  (slack-buffer-key 'slack-search-result-buffer (oref this search-result)))

(cl-defmethod slack-team-buffer-key ((_class (subclass slack-search-result-buffer)))
  "Return the team-scoped class-level buffer key for the search result buffer."
  'slack-search-result-buffer)

(defun slack-create-search-result-buffer (search-result team)
  "Create or update the stable buffer for SEARCH-RESULT on TEAM.
SEARCH-RESULT is the search-result argument.
TEAM is the team argument.  Re-running an identical query updates the
existing object without killing its live Emacs buffer; the durable
page renderer replaces its contents in place."
  (let ((buffer
         (or (slack-buffer-find
              'slack-search-result-buffer team search-result)
             (make-instance 'slack-search-result-buffer
                            :team-id (oref team id)
                            :search-result search-result))))
    (oset buffer search-result search-result)
    (slack-buffer-cache-team buffer team)
    buffer))
(cl-defmethod slack-buffer-file-search-result-to-string ((this slack-search-result-buffer) file)
  "Render FILE as a single line entry in the search result buffer THIS."
  (let ((title (slack-file-title file))
        (type (slack-file-type file))
        (user-name (slack-user-name (oref file user)
                                    (slack-buffer-team this)))
        (id (oref file id)))
    (format "%s\n%s"
            (slack-file-link-info id title)
            (propertize (format "%s %s" user-name type)
                        'face 'slack-attachment-footer))))

(cl-defmethod slack-buffer-insert ((this slack-search-result-buffer) match)
  "Insert a rendered representation of THIS buffer into the current buffer.
MATCH is the match argument."
  (let* ((team (slack-buffer-team this))
         (time (slack-ts-to-time (slack-ts match)))
         (lui-time-stamp-time time)
         (lui-time-stamp-format "[%Y-%m-%d %H:%M] "))
    (if (slack-file-p match)
        (lui-insert (slack-buffer-file-search-result-to-string this match) t)
      (lui-insert (slack-buffer--apply-message-keymap
                   (slack-message-to-string match team))
                  t))
    (lui-insert "" t)))

(cl-defmethod slack-buffer-has-next-page-p ((this slack-search-result-buffer))
  "Return non-nil when THIS buffer has more history to load."
  (let* ((team (slack-buffer-team this))
         (key (slack-search-result-page-key (oref this search-result)))
         (state (slack-team-page-state team key)))
    (and (slack-page-state-has-more state)
         (slack-page-state-continuation state))))

(cl-defmethod slack-buffer-insert-history ((this slack-search-result-buffer))
  "Insert historical messages into the buffer for THIS buffer."
  (let* ((search-result (oref this search-result))
         (pagination (oref search-result pagination))
         (first (oref pagination first))
         (last (oref pagination last))
         (matches (last (oref search-result matches) (1+ (- last first))))
         (cur-point (point)))
    (cl-loop for match in matches
             do (slack-buffer-insert this match))
    (goto-char cur-point)))

(cl-defmethod slack-buffer-request-history ((this slack-search-result-buffer) after-success &optional on-error)
  "Request older history for THIS buffer from the Slack API.
AFTER-SUCCESS is the after-success argument.  ON-ERROR is invoked on
request failure."
  (with-slots (search-result) this
    (slack-search-request search-result after-success (slack-buffer-team this)
                          (slack-search-paging-next-page
                           (oref search-result pagination))
                          on-error)))

(cl-defmethod slack-buffer-init-buffer ((this slack-search-result-buffer))
  "Initialize and return the display buffer for THIS buffer."
  (let ((buffer (cl-call-next-method)))
    (with-current-buffer buffer
      (slack-search-result-buffer-mode)
      (slack-buffer-set-current-buffer this))
    buffer))

(defun slack-search-result-buffer-render-page-state (buffer state)
  "Render exact BUFFER from durable search STATE."
  (when (and (slot-boundp buffer 'buf)
             (buffer-live-p (oref buffer buf)))
    (when (slack-page-state-loaded-p state)
      (oset buffer search-result (slack-page-state-value state)))
    (with-current-buffer (oref buffer buf)
      (slack-buffer-widen
       (let ((inhibit-read-only t))
         (delete-region (point-min) lui-output-marker)
         (when (slack-page-state-loaded-p state)
           (dolist (match (oref (slack-page-state-value state) matches))
             (slack-buffer-insert buffer match))
           (if (slack-page-state-has-more state)
               (slack-buffer-insert-load-more buffer)
             (let ((lui-time-stamp-position nil))
               (lui-insert "(no more messages)\n" t))))
         (goto-char (point-min))
         (slack-buffer-insert-page-status buffer state)
         (goto-char (point-min)))))))

(cl-defmethod slack-buffer-delete-load-more-string ((_this slack-search-result-buffer))
  "Remove the \"load more\" marker from the buffer for the search result
buffer.")

(cl-defmethod slack-buffer-prepare-marker-for-history ((_this slack-search-result-buffer))
  "Position point so history can be inserted in the search result buffer.")

(cl-defmethod slack-buffer-insert--history ((this slack-search-result-buffer))
  "Insert loaded history items into the buffer for THIS buffer."
  (slack-buffer-insert-history this)
  (unless (slack-buffer-has-next-page-p this)
    (let ((lui-time-stamp-position nil))
      (lui-insert "(no more messages)\n" t))))

(defun slack-search-result-buffer--page-loader (search-result team state)
  "Return a primary-before-hydration loader for SEARCH-RESULT on TEAM and STATE."
  (lambda (generation success error)
    (let (primary-result hydration-started-p)
      (cl-labels
          ((current-p ()
             (and (= generation (slack-page-state-generation state))
                  (slack-page-state-in-flight-p state)))
           (primary (result)
             (when (current-p)
               (setq primary-result result)
               (let ((next-page (slack-search-result-next-page result)))
                 (funcall success result next-page (and next-page t) t))))
           (hydrated ()
             (when (and primary-result
                        (not hydration-started-p)
                        (current-p)
                        (= generation
                           (slack-page-state-committed-generation state)))
               (setq hydration-started-p t)
               (slack-page-state-ready state generation)))
           (failed (&rest errors)
             (when (current-p)
               (apply error errors))))
        (slack-search-request
         search-result #'hydrated team 1 #'failed #'primary)))))

(defun slack-search-result-buffer--present (search-result team)
  "Present SEARCH-RESULT for TEAM immediately, then refresh it."
  (let* ((key (slack-search-result-page-key search-result))
         (state (slack-team-page-state team key))
         (buffer (slack-create-search-result-buffer search-result team)))
    (slack-buffer-present-page
     buffer state
     (slack-search-result-buffer--page-loader search-result team state)
     #'slack-search-result-buffer-render-page-state
     t)
    buffer))

(cl-defmethod slack-buffer-load-more ((this slack-search-result-buffer))
  "Load the next durable search page for THIS without replacing its buffer."
  (let* ((team (slack-buffer-team this))
         (key (slack-search-result-page-key (oref this search-result)))
         (state (slack-team-page-state team key)))
    (when (and (slack-buffer-has-next-page-p this)
               (not slack-buffer--loading-more-p)
               (eq 'ready (slack-page-state-status state))
               (eq (current-buffer) (oref this buf)))
      (setq slack-buffer--loading-more-p t)
      (let* ((source-result (slack-page-state-value state))
             (source-matches (copy-sequence (oref source-result matches)))
             (generation (slack-page-state-generation state))
             (requested-page (slack-page-state-continuation state))
             (origin-buffer (current-buffer))
             (page-result
              (slack-search-result-empty
               (eieio-object-class-name source-result)
               (oref source-result query)
               (oref source-result sort)
               (oref source-result sort-dir)))
             accepted-result
             primary-seen-p)
        (cl-labels
            ((reset-loading-flag ()
               (when (buffer-live-p origin-buffer)
                 (with-current-buffer origin-buffer
                   (setq slack-buffer--loading-more-p nil))))
             (state-current-p ()
               (and (= generation (slack-page-state-generation state))
                    (eq 'ready (slack-page-state-status state))
                    (eq source-result (slack-page-state-value state))
                    (equal source-matches (oref source-result matches))
                    (equal requested-page
                           (slack-page-state-continuation state))))
             (accepted-current-p ()
               (and accepted-result
                    (= generation (slack-page-state-generation state))
                    (eq accepted-result (slack-page-state-value state))))
             (presentation-current-p ()
               (and (buffer-live-p origin-buffer)
                    (eq origin-buffer (oref this buf))))
             (primary (result)
               (unless primary-seen-p
                 (setq primary-seen-p t)
                 (condition-case page-error
                     (if (state-current-p)
                         (let* ((candidate
                                 (slack-search-result-empty
                                  (eieio-object-class-name source-result)
                                  (oref source-result query)
                                  (oref source-result sort)
                                  (oref source-result sort-dir)))
                                (next-page
                                 (slack-search-result-next-page result)))
                           (oset candidate total (oref source-result total))
                           (oset candidate matches source-matches)
                           (oset candidate pagination
                                 (oref source-result pagination))
                           (slack-merge candidate result)
                           (when (slack-page-state-commit-extension
                                  state generation requested-page candidate
                                  next-page (and next-page t))
                             (setq accepted-result candidate)
                             (when (presentation-current-p)
                               (slack-search-result-buffer-render-page-state
                                this state))))
                       (reset-loading-flag))
                   (error
                    (reset-loading-flag)
                    (message "slack-search: page processing failed: %S"
                             page-error)))))
             (hydrated ()
               (unwind-protect
                   (when (and (accepted-current-p)
                              (presentation-current-p))
                     (slack-search-result-buffer-render-page-state this state))
                 (reset-loading-flag)))
             (failed (&rest _errors)
               (reset-loading-flag)))
          (condition-case request-error
              (slack-search-request
               page-result #'hydrated team requested-page #'failed #'primary)
            (error
             (reset-loading-flag)
             (signal (car request-error) (cdr request-error)))))))))

(defun slack-search-from-messages (query)
  "Run a Slack message search for QUERY and display the result buffer."
  (interactive
   (list (when (region-active-p)
           (substring-no-properties (funcall region-extract-function)))))
  (cl-destructuring-bind (team query sort sort-dir) (slack-search-query-params query)
    (slack-search-result-buffer--present
     (slack-search-result-empty
      'slack-search-result query sort sort-dir)
     team)))

(defun slack-search-from-files ()
  "Run a Slack file search prompted interactively and display the result buffer."
  (interactive)
  (cl-destructuring-bind (team query sort sort-dir) (slack-search-query-params)
    (slack-search-result-buffer--present
     (slack-search-result-empty
      'slack-file-search-result query sort sort-dir)
     team)))

(cl-defmethod slack-buffer-display-thread ((this slack-search-result-buffer) ts)
  "Open the thread of the search match at TS in THIS buffer.
Each match may belong to a different room, so the room is resolved
from the matching message's channel id."
  (slack-if-let* ((team (slack-buffer-team this))
                  (match (slack-search-result--match-at this ts))
                  (room (slack-room-find (oref (oref match channel) id) team)))
      (slack-thread-show-messages (oref match message) room team)
    (error "Not possible to open thread")))

(defun slack-search-result--match-at (buffer ts)
  "Return the `slack-search-message' in BUFFER whose timestamp is TS."
  (cl-find-if (lambda (m)
                (and (slack-search-message-p m)
                     (string= ts (slack-ts m))))
              (oref (oref buffer search-result) matches)))

(defun slack-search-result-open-message ()
  "Open url in search result page."
  (interactive)
  (if-let ((url (get-text-property (point) 'permalink)))
      (slack-open-url url)
    (error "Not possible to jump to message because permalink is not defined")))

(define-key slack-search-result-buffer-mode-map (kbd "RET") 'slack-search-result-open-message)

(provide 'slack-search-result-buffer)
;;; slack-search-result-buffer.el ends here
