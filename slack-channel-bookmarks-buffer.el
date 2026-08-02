;;; slack-channel-bookmarks-buffer.el --- Channel bookmarks -*- lexical-binding: t; -*-

;; Copyright (C) 2026

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;;; Commentary:

;; Display a channel's remote bookmarks through the shared visible-first page
;; lifecycle.

;;; Code:

(require 'eieio)
(require 'org)
(require 'slack-buffer)
(require 'slack-channel)
(require 'slack-team)
(require 'slack-util)

(define-derived-mode slack-channel-bookmarks-buffer-mode org-mode
  "Slack Bookmarks"
  "Major mode for a Slack channel's bookmarks.")

(define-key slack-channel-bookmarks-buffer-mode-map (kbd "q") #'bury-buffer)

(defclass slack-channel-bookmarks-buffer (slack-buffer)
  ((channel-id :initarg :channel-id :type string)
   (bookmarks :initarg :bookmarks :initform nil :type list)))

(defun slack-channel-bookmarks-page-key (channel-id)
  "Return the durable bookmarks page key for CHANNEL-ID."
  (list 'channel-bookmarks channel-id))

(cl-defmethod slack-buffer-name ((this slack-channel-bookmarks-buffer))
  "Return the display buffer name for THIS bookmarks buffer."
  (format "*slack: %s : Bookmarks - %s*"
          (slack-team-name (slack-buffer-team this))
          (oref this channel-id)))

(cl-defmethod slack-buffer-key
  ((_class (subclass slack-channel-bookmarks-buffer)) channel-id)
  "Return the class-level bookmarks buffer key for CHANNEL-ID."
  channel-id)

(cl-defmethod slack-buffer-key ((this slack-channel-bookmarks-buffer))
  "Return the lookup key identifying THIS bookmarks buffer."
  (slack-buffer-key
   'slack-channel-bookmarks-buffer (oref this channel-id)))

(cl-defmethod slack-team-buffer-key
  ((_class (subclass slack-channel-bookmarks-buffer)))
  "Return the team slot containing channel bookmarks buffers."
  'slack-channel-bookmarks-buffer)

(cl-defmethod slack-buffer-init-buffer ((this slack-channel-bookmarks-buffer))
  "Initialize and return THIS bookmarks buffer."
  (let ((buffer (cl-call-next-method)))
    (with-current-buffer buffer
      (slack-channel-bookmarks-buffer-mode)
      (slack-buffer-set-current-buffer this))
    buffer))

(defun slack-create-channel-bookmarks-buffer (channel-id team)
  "Find or create CHANNEL-ID's bookmarks buffer on TEAM."
  (let ((buffer
         (or (slack-buffer-find
              'slack-channel-bookmarks-buffer team channel-id)
             (make-instance 'slack-channel-bookmarks-buffer
                            :team-id (oref team id)
                            :channel-id channel-id))))
    (slack-buffer-cache-team buffer team)
    buffer))

(defun slack-channel-bookmarks-buffer--insert-bookmark (bookmark)
  "Insert one BOOKMARK as an Org link."
  (let ((link (or (plist-get bookmark :link) ""))
        (title (or (plist-get bookmark :title) "untitled")))
    (insert (format "- [[%s][%s]]\n"
                    (replace-regexp-in-string "\\]\\]" "]​]" link)
                    (replace-regexp-in-string "\\]\\]" "]​]" title)))))

(defun slack-channel-bookmarks-buffer-render-page-state (buffer state)
  "Render exact bookmarks BUFFER from durable page STATE."
  (when (and (slot-boundp buffer 'buf)
             (buffer-live-p (oref buffer buf)))
    (when (slack-page-state-loaded-p state)
      (oset buffer bookmarks (slack-page-state-value state)))
    (with-current-buffer (oref buffer buf)
      (let ((inhibit-read-only t))
        (erase-buffer)
        (insert "* Bookmarks\n\n")
        (slack-buffer-insert-page-status buffer state)
        (when (slack-page-state-loaded-p state)
          (if (oref buffer bookmarks)
              (mapc #'slack-channel-bookmarks-buffer--insert-bookmark
                    (oref buffer bookmarks))
            (insert "(No bookmarks.)\n")))
        (font-lock-flush)
        (goto-char (point-min))))))

(defun slack-channel-bookmarks-buffer--page-loader (channel-id team)
  "Return the bookmarks page loader for CHANNEL-ID on TEAM."
  (lambda (_generation success error)
    (slack-bookmarks-request
     channel-id team
     (lambda (data)
       (let ((normalized
              (slack-request-normalize-response
               (lambda ()
                 (slack-seq-to-list (plist-get data :bookmarks)))
               error)))
         (when normalized
           (funcall success (cdr normalized) nil nil))))
     (lambda (&rest errors)
       (apply error errors)))))

(defun slack-channel-bookmarks-buffer--present (channel-id team refresh)
  "Present CHANNEL-ID's bookmarks on TEAM.
When REFRESH is non-nil, refresh an already ready page in place."
  (let* ((state (slack-team-page-state
                 team (slack-channel-bookmarks-page-key channel-id)))
         (buffer (slack-create-channel-bookmarks-buffer channel-id team)))
    (slack-buffer-present-page
     buffer state
     (slack-channel-bookmarks-buffer--page-loader channel-id team)
     #'slack-channel-bookmarks-buffer-render-page-state
     refresh)
    buffer))

(provide 'slack-channel-bookmarks-buffer)
;;; slack-channel-bookmarks-buffer.el ends here
