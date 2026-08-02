;;; slack-file-list-buffer.el ---                    -*- lexical-binding: t; -*-

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
(require 'slack-buffer)
(require 'slack-message-buffer)
(require 'slack-file-info-buffer)
(require 'slack-star)
(declare-function slack-team-remove-file "slack-file")
(declare-function slack-team-replace-files "slack-file")

(defvar slack-file-download-button-keymap
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "RET") 'slack-download-file-at-point)
    (define-key map [mouse-1] 'slack-download-file-at-point)
    map))

(define-derived-mode slack-file-list-buffer-mode slack-buffer-mode "Slack Files")

;; TODO impl without slack-message-buffer
(defclass slack-file-list-buffer (slack-buffer)
  ((page :initarg :page :type integer)
   (pages :initarg :pages :type integer)
   (oldest :initform nil :type (or null integer))
   (oldest-id :initform nil :type (or null string))))

(cl-defmethod slack-buffer-name ((this slack-file-list-buffer))
  "Return the display buffer name for THIS buffer."
  (format "*slack: %s : Files" (slack-team-name (slack-buffer-team this))))

(cl-defmethod slack-buffer-key ((_class (subclass slack-file-list-buffer)))
  "Return the class-level buffer key for the file list buffer."
  'slack-file-list-buffer)

(cl-defmethod slack-buffer-key ((_this slack-file-list-buffer))
  "Return the lookup key identifying the buffer for the file list buffer."
  (slack-buffer-key 'slack-file-list-buffer))

(cl-defmethod slack-team-buffer-key ((_class (subclass slack-file-list-buffer)))
  "Return the team-scoped class-level buffer key for the file list buffer."
  'slack-file-list-buffer)

(defun slack-create-file-list-buffer (page pages team)
  "Return TEAM's stable file-list buffer with PAGE and PAGES metadata."
  (slack-if-let* ((buffer (slack-buffer-find 'slack-file-list-buffer team)))
      buffer
    (let ((buffer (slack-file-list-buffer
                   :team-id (oref team id) :page page :pages pages)))
      (slack-buffer-cache-team buffer team)
      buffer)))

(cl-defmethod slack-buffer-has-next-page-p ((this slack-file-list-buffer))
  "Return non-nil when THIS buffer has more history to load."
  (with-slots (page pages) this
    (< page pages)))

(cl-defmethod slack-buffer-set-oldest ((this slack-file-list-buffer) file)
  "Remember the oldest visible FILE in buffer THIS for paging purposes."
  (when file
    (oset this oldest (oref file created))
    (oset this oldest-id (oref file id))))

(cl-defmethod slack-buffer-init-buffer ((this slack-file-list-buffer))
  "Initialize and return the display buffer for THIS buffer."
  (let ((buf (cl-call-next-method)))
    (with-current-buffer buf
      (slack-file-list-buffer-mode)
      (slack-buffer-set-current-buffer this)
      (goto-char (point-max)))
    buf))

(defun slack-file-list--next-page (page pages)
  "Return the page after PAGE when it is within PAGES, or nil."
  (and (integerp page)
       (integerp pages)
       (< page pages)
       (1+ page)))

(defun slack-file-list-buffer-render-page-state (object state)
  "Render OBJECT's durable file-list payload and status from STATE."
  (let* ((value (slack-page-state-value state))
         (files (plist-get value :files))
         (page (or (plist-get value :page) 0))
         (pages (or (plist-get value :pages) 0))
         (file-id (get-text-property (point) 'ts))
         (output-offset (- (point) (point-min))))
    (oset object page page)
    (oset object pages pages)
    (oset object oldest nil)
    (oset object oldest-id nil)
    (slack-buffer-widen
      (let ((inhibit-read-only t))
        (delete-region (point-min) (marker-position lui-output-marker))
        (set-marker lui-output-marker (point-min))
        (goto-char lui-output-marker)
        (slack-buffer-with-deferred-hooks
          (when (slack-page-state-has-more state)
            (slack-buffer-insert-load-more object))
          (when (and (slack-page-state-loaded-p state)
                     (eq 'ready (slack-page-state-status state))
                     (null files))
            (let ((lui-time-stamp-position nil))
              (lui-insert "(no files)" t)))
          (dolist (file files)
            (slack-buffer-insert object file t))
          (slack-buffer-set-oldest object (car files)))
        (goto-char lui-output-marker)
        (slack-buffer-insert-page-status object state)
        (set-marker lui-output-marker (point))))
    (unless (and file-id (slack-buffer-goto file-id))
      (goto-char (min (+ (point-min) output-offset)
                      (marker-position lui-output-marker))))))

(cl-defmethod slack-buffer-load-more ((this slack-file-list-buffer))
  "Load and durably append the next file-list page for THIS buffer."
  (let* ((team (slack-buffer-team this))
         (state (and team (slack-team-page-state team 'file-list)))
         (requested-page (and state (slack-page-state-continuation state))))
    (when (and team
               state
               (integerp requested-page)
               (slack-page-state-has-more state)
               (slack-buffer-has-next-page-p this)
               (eq 'ready (slack-page-state-status state))
               (slot-boundp this 'buf)
               (eq (current-buffer) (oref this buf))
               (not slack-buffer--loading-more-p))
      (setq slack-buffer--loading-more-p t)
      (let ((buffer (current-buffer))
            (generation (slack-page-state-generation state))
            (active-page requested-page)
            (primary-committed-p nil)
            (finished-p nil))
        (cl-labels
            ((state-current-p (cursor)
               (and (= generation (slack-page-state-generation state))
                    (eq 'ready (slack-page-state-status state))
                    (equal cursor (slack-page-state-continuation state))))
             (buffer-current-p ()
               (and (buffer-live-p buffer)
                    (slot-boundp this 'buf)
                    (eq buffer (oref this buf))
                    (eq this
                        (slack-buffer-find
                         'slack-file-list-buffer team))))
             (owner-acceptable-p ()
               (or (not (buffer-live-p buffer))
                   (buffer-current-p)))
             (finish ()
               (unless finished-p
                 (setq finished-p t)
                 (when (buffer-live-p buffer)
                   (with-current-buffer buffer
                     (setq slack-buffer--loading-more-p nil)))))
             (fail (&rest errors)
               (unless finished-p
                 (message "Slack: failed to load more files: %s"
                          (slack-buffer--normalize-page-error errors))
                 (finish)))
             (render-current ()
               (when (buffer-current-p)
                 (with-current-buffer buffer
                   (slack-file-list-buffer-render-page-state this state))))
             (primary (page pages page-files)
               (unless finished-p
                 (condition-case pagination-error
                     (if (and (not primary-committed-p)
                              (equal page requested-page)
                              (state-current-p requested-page)
                              (owner-acceptable-p))
                         (let* ((next-page
                                 (slack-file-list--next-page page pages))
                                (current-files
                                 (plist-get (slack-page-state-value state)
                                            :files))
                                (files
                                 (slack-file-list--merge-files
                                  current-files page-files)))
                           (if (slack-page-state-commit-extension
                                state generation requested-page
                                (list :files files :page page :pages pages)
                                next-page (and next-page t))
                               (progn
                                 (setq primary-committed-p t
                                       active-page next-page)
                                 (slack-team-replace-files team files)
                                 (oset this page page)
                                 (oset this pages pages)
                                 (render-current))
                             (finish)))
                       (finish))
                   (error (funcall #'fail pagination-error)))))
             (hydrated (&rest _ignored)
               (unless finished-p
                 (unwind-protect
                     (when (and primary-committed-p
                                (state-current-p active-page))
                       (render-current))
                   (finish)))))
          (condition-case request-error
              (slack-file-list-request
               team
               :page (number-to-string requested-page)
               :on-primary-page #'primary
               :after-success #'hydrated
               :on-error #'fail)
            (error (funcall #'fail request-error))))))))

(defun slack-file-list--sync-state-files (team state)
  "Replace STATE's durable file list with the current files from TEAM."
  (when (slack-page-state-loaded-p state)
    (setf (slack-page-state-value state)
          (plist-put (copy-sequence (slack-page-state-value state))
                     :files (slack-team-files team))
          (slack-page-state-updated-at state) (current-time))))

(defun slack-file-list-handle-deleted (team file-id)
  "Remove FILE-ID from TEAM's durable file-list state and current view."
  (let ((state (slack-team-page-state team 'file-list)))
    (slack-team-remove-file team file-id)
    (slack-file-list--sync-state-files team state)
    (when-let* ((object (slack-buffer-find 'slack-file-list-buffer team))
                (buffer (and (slot-boundp object 'buf) (oref object buf)))
                ((buffer-live-p buffer)))
      (with-current-buffer buffer
        (slack-file-list-buffer-render-page-state object state)))))

(cl-defmethod slack-buffer-update ((this slack-file-list-buffer) message &key replace)
  "Update THIS buffer after new data arrives.
MESSAGE is the message argument.  With REPLACE non-nil, replace it in place."
  (let* ((team (slack-buffer-team this))
         (state (and team (slack-team-page-state team 'file-list)))
         (current (and team
                       (slack-buffer-find 'slack-file-list-buffer team)))
         (target (or current this))
         (buffer (and (slot-boundp target 'buf) (oref target buf))))
    (when state
      (slack-file-list--sync-state-files team state))
    (when (buffer-live-p buffer)
      (with-current-buffer buffer
        (if (and state (slack-page-state-loaded-p state))
            (slack-file-list-buffer-render-page-state target state)
          (if replace
              (slack-buffer-replace target message)
            (slack-buffer-insert target message)))))))

(cl-defmethod slack-buffer-download-file ((this slack-file-list-buffer) file-id)
  "Download the file at point in THIS buffer.
FILE-ID is the file-id argument."
  (slack-if-let* ((team (slack-buffer-team this))
                  (file (slack-file-find file-id team)))
      (slack-file-download file team)))

(defun slack-download-file-at-point ()
  "Download the file referenced at point in the current Slack buffer."
  (interactive)
  (slack-if-let* ((file-id (get-text-property (point) 'file-id))
                  (buf slack-current-buffer))
      (slack-buffer-download-file buf file-id)))

(defun slack-buffer--run-file-action ()
  "Run the action associated with the file referenced at point."
  (interactive)
  (slack-if-let* ((buf slack-current-buffer)
                  (file-id (get-text-property (point) 'file-id)))
      (slack-buffer-run-file-action buf file-id)))

(cl-defmethod slack-buffer-run-file-action ((this slack-file-list-buffer) file-id)
  "Run THIS buffer.
FILE-ID is the file-id argument."
  (let* ((team (slack-buffer-team this))
         (file (slack-file-find file-id team)))
    (slack-file-run-action file this)))

(cl-defmethod slack-buffer-file-to-string ((this slack-file-list-buffer) file)
  "Render a FILE attached to a message in THIS buffer as a string."
  (let* ((team (slack-buffer-team this))
         (lui-time-stamp-format "[%Y-%m-%d %H:%M] ")
         (lui-time-stamp-time (slack-message-time-stamp file))
         (thumb (slack-image-string (slack-file-thumb-image-spec file 80)))
         (header (format "%s%s"
                         (if (slack-string-blankp thumb) ""
                           (format "%s " thumb))
                         (slack-file-link-info (slack-file-id file)
                                               (slack-file-title file))))
         (user-name (propertize (or (slack-user-name (oref file user) team) "")
                                'face '(:weight bold :height 0.8)))
         (timestamp (and (oref file timestamp)
                         (format-time-string "%Y-%m-%d %H:%M:%S"
                                             (seconds-to-time
                                              (oref file timestamp)))))
         (description (format "%s %s %s%s"
                              user-name
                              timestamp
                              (if (slack-file-downloadable-p file)
                                  (format "%s "
                                          (slack-file-download-button file))
                                "")
                              (slack-file-action-button file))))
    (slack-format-message header description)))

(cl-defmethod slack-buffer-insert ((this slack-file-list-buffer) message &optional not-tracked-p)
  "Insert a rendered representation of THIS buffer into the current buffer.
MESSAGE is the message argument."
  (let ((lui-time-stamp-format "[%Y-%m-%d %H:%M] ")
        (lui-time-stamp-time (slack-message-time-stamp message))
        (ts (slack-file-id message)))
    (lui-insert-with-text-properties
     (slack-buffer-file-to-string this message)
     'not-tracked-p not-tracked-p
     'ts ts
     'slack-last-ts lui-time-stamp-last)
    (lui-insert "" t)))

(cl-defmethod slack-buffer--replace ((this slack-file-list-buffer) ts)
  "Replace the rendered message identified by the argument in THIS buffer.
TS is the ts argument."
  (slack-if-let* ((team (slack-buffer-team this))
                  (file (slack-file-find ts team)))
      (slack-buffer-replace this file)))

(cl-defmethod slack-buffer-replace ((this slack-file-list-buffer) message)
  "Replace the rendered MESSAGE identified by the argument in THIS buffer."
  (with-current-buffer (slack-buffer-buffer this)
    (lui-replace (slack-buffer-file-to-string this message)
                 (lambda ()
                   (equal (get-text-property (point) 'ts)
                          (slack-file-id message))))))

(cl-defmethod slack-buffer-toggle-email-expand ((this slack-file-list-buffer) file-id)
  "Toggle the expanded/collapsed state of THIS buffer.
FILE-ID is the file-id argument."
  (slack-if-let* ((team (slack-buffer-team this))
                  (file (slack-file-find file-id team)))
      (progn
        (oset file is-expanded (not (oref file is-expanded)))
        (slack-buffer-update this file :replace t))))

(defun slack-file-list ()
  "Open the file list buffer for a team selected interactively."
  (interactive)
  (when-let* ((team (slack-team-select)))
    (let* ((existing (slack-buffer-find 'slack-file-list-buffer team))
           (state (slack-team-page-state team 'file-list))
           (buffer (or existing
                       (slack-create-file-list-buffer 0 0 team))))
      (slack-buffer-present-page
       buffer state
       (lambda (generation success error)
         (cl-labels
             ((current-p ()
                (and (= generation (slack-page-state-generation state))
                     (slack-page-state-in-flight-p state)))
              (primary (page pages files)
                (when (current-p)
                  (let* ((next-page
                          (slack-file-list--next-page page pages))
                         (accepted-files
                          (slack-file-list--merge-files nil files)))
                    (when (funcall success
                                   (list :files accepted-files
                                         :page page
                                         :pages pages)
                                   next-page
                                   (and next-page t)
                                   t)
                      (slack-team-replace-files team accepted-files)))))
              (hydrated (&rest _ignored)
                (when (current-p)
                  (slack-page-state-ready state generation))))
           (slack-file-list-request
            team
            :on-primary-page #'primary
            :after-success #'hydrated
            :on-error error)))
       #'slack-file-list-buffer-render-page-state
       t))))

(defun slack-file-list--merge-files (files new-files)
  "Return FILES and NEW-FILES deduplicated and sorted oldest first."
  (let ((seen (make-hash-table :test 'equal))
        merged)
    (dolist (file (append files new-files))
      (when file
        (let ((id (slack-file-id file)))
          (unless (gethash id seen)
            (puthash id t seen)
            (push file merged)))))
    (cl-sort merged #'< :key #'slack-file-sort-key)))

(cl-defmethod slack-buffer-add-star ((this slack-file-list-buffer) ts)
  "Star the item at point in THIS buffer.
TS is the ts argument."
  (slack-if-let* ((team (slack-buffer-team this))
                  (file (slack-file-find ts team)))
      (slack-star-api-request slack-message-stars-add-url
                              (slack-message-star-api-params file)
                              team)))

(cl-defmethod slack-buffer-remove-star ((this slack-file-list-buffer) ts)
  "Remove the star from THIS buffer.
TS is the ts argument."
  (slack-if-let* ((team (slack-buffer-team this))
                  (file (slack-file-find ts team)))
      (slack-star-api-request slack-message-stars-remove-url
                              (slack-message-star-api-params file)
                              team)))

(provide 'slack-file-list-buffer)
;;; slack-file-list-buffer.el ends here
