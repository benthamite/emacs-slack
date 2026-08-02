;;; slack-pinned-items-buffer.el ---                 -*- lexical-binding: t; -*-

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
(require 'slack-room-buffer)
(require 'slack-pinned-item)

(declare-function slack-pins-list "slack-pinned-item"
                  (room team after-success &optional on-primary-page on-error))

(define-derived-mode slack-pinned-items-buffer-mode slack-buffer-mode "Slack Pinned Items"
  "Major mode for a Slack pinned items buffer.

Message-region bindings (active when point is on a pinned message):
\\{slack-message-keymap}
Buffer-wide bindings:
\\{slack-pinned-items-buffer-mode-map}")

(defclass slack-pinned-items-buffer (slack-room-buffer)
  ((items :initarg :items :initform nil :type list)))

(defun slack-pinned-items-buffer-page-key (room)
  "Return the durable pinned-items page key for ROOM."
  (list 'pins (oref room id)))

(cl-defmethod slack-buffer-name ((this slack-pinned-items-buffer))
  "Return the display buffer name for THIS buffer."
  (let ((team (slack-buffer-team this))
        (room (slack-buffer-room this)))
    (concat "*Slack - "
            (slack-team-name team)
            " : "
            (slack-room-name room team)
            " Pinned Items")))

(cl-defmethod slack-buffer-key ((_class (subclass slack-pinned-items-buffer)) room)
  "Return the class-level buffer key for the pinned items buffer.
ROOM is the room argument."
  (oref room id))

(cl-defmethod slack-buffer-key ((this slack-pinned-items-buffer))
  "Return the lookup key identifying the buffer for THIS buffer."
  (slack-buffer-key 'slack-pinned-items-buffer (slack-buffer-room this)))

(cl-defmethod slack-team-buffer-key ((_class (subclass slack-pinned-items-buffer)))
  "Return the team-scoped class-level buffer key for the pinned items buffer."
  'slack-pinned-items-buffer)

(cl-defmethod slack-pinned-items-buffer-insert-items ((this slack-pinned-items-buffer))
  "Insert the header and pinned items of buffer THIS into the current buffer."
  (let* ((header-face '(:underline t :weight bold))
         (buf-header (propertize "Pinned Items\n" 'face header-face)))
    (let ((inhibit-read-only t))
      (delete-region (point-min) lui-output-marker))
    (let ((lui-time-stamp-position nil))
      (lui-insert buf-header t))
    (let ((items (oref this items)))
      (if (< 0 (length items))
          (cl-loop for m in items
                   do (slack-buffer-insert this m t))
        (let ((inhibit-read-only t))
          (lui-insert "No Pinned Items" t))))))

(cl-defmethod slack-buffer-init-buffer ((this slack-pinned-items-buffer))
  "Initialize and return the display buffer for THIS buffer."
  (let* ((buf (cl-call-next-method)))
    (with-current-buffer buf
      (slack-pinned-items-buffer-mode)
      (slack-buffer-set-current-buffer this))
    buf))

(cl-defun slack-create-pinned-items-buffer
    (room team &optional (items nil items-supplied-p))
  "Find or create ROOM's pinned-items buffer on TEAM.
When ITEMS-SUPPLIED-P is non-nil, update the buffer object's items.
ROOM is the room argument.
TEAM is the team argument."
  (let ((buffer
         (or (slack-buffer-find 'slack-pinned-items-buffer team room)
             (slack-pinned-items-buffer :room-id (oref room id)
                                        :team-id (oref team id)
                                        :items items))))
    (when items-supplied-p
      (oset buffer items items))
    (slack-buffer-cache-team buffer team)
    buffer))

(defun slack-pinned-items-buffer-render-page-state (buffer state)
  "Render exact pinned-items BUFFER from durable page STATE."
  (when (and (slot-boundp buffer 'buf)
             (buffer-live-p (oref buffer buf)))
    (when (slack-page-state-loaded-p state)
      (oset buffer items (slack-page-state-value state)))
    (with-current-buffer (oref buffer buf)
      (slack-buffer-widen
       (let ((inhibit-read-only t))
         (delete-region (point-min) lui-output-marker)
         (if (slack-page-state-loaded-p state)
             (slack-pinned-items-buffer-insert-items buffer)
           (let ((lui-time-stamp-position nil))
             (lui-insert
              (propertize "Pinned Items\n"
                          'face '(:underline t :weight bold))
              t)))
         (goto-char (point-min))
         (slack-buffer-insert-page-status buffer state)
         (goto-char (point-min)))))))

(defun slack-pinned-items-buffer--page-loader (room team state)
  "Return the primary-then-hydrated page loader for ROOM on TEAM using STATE."
  (lambda (generation success error)
    (slack-pins-list
     room team
     (lambda (&rest _ignored)
       (slack-page-state-ready state generation))
     (lambda (items)
       (funcall success items nil nil t))
     (lambda (&rest errors)
       (apply error errors)))))

(defun slack-pinned-items-buffer--present (room team refresh)
  "Present ROOM's pinned items on TEAM, reloading when REFRESH is non-nil."
  (let* ((state (slack-team-page-state
                 team (slack-pinned-items-buffer-page-key room)))
         (buffer (slack-create-pinned-items-buffer room team)))
    (slack-buffer-present-page
     buffer state
     (slack-pinned-items-buffer--page-loader room team state)
     #'slack-pinned-items-buffer-render-page-state
     refresh)
    buffer))

(defun slack-pinned-items-refresh ()
  "Refresh the current pinned-items buffer in place."
  (interactive)
  (if (cl-typep slack-current-buffer 'slack-pinned-items-buffer)
      (slack-pinned-items-buffer--present
       (slack-buffer-room slack-current-buffer)
       (slack-buffer-team slack-current-buffer)
       t)
    (user-error "Current buffer is not a Slack pinned-items buffer")))

(cl-defmethod slack-buffer--replace ((this slack-pinned-items-buffer) ts)
  "Replace the rendered message identified by the argument in THIS buffer.
TS is the ts argument."
  (with-slots (items) this
    (slack-if-let* ((message (cl-find-if #'(lambda (m) (string= ts (slack-ts m)))
                                         items)))
        (slack-buffer-replace this message))))

(cl-defmethod slack-buffer-display-thread ((this slack-pinned-items-buffer) ts)
  "Open the thread of the pinned message at TS in THIS buffer.
Items are `slack-pinned-item' wrappers; the thread machinery needs
the wrapped `slack-message', so unwrap before dispatching (file-type
pins carry a file, not a message, and cannot open a thread)."
  (slack-if-let* ((team (slack-buffer-team this))
                  (room (slack-buffer-room this))
                  (item (cl-find-if (lambda (m) (string= ts (slack-ts m)))
                                    (oref this items)))
                  (message (if (cl-typep item 'slack-pinned-item)
                               (oref item message)
                             item))
                  (message-p (cl-typep message 'slack-message)))
      (slack-thread-show-messages message room team)
    (error "Not possible to open thread")))

(defun slack-pinned-items-open-message ()
  "Open url in pinned items page."
  (interactive)
  (if-let ((url (get-text-property (point) 'permalink)))
      (slack-open-url url)
    (error "Not possible to jump to message because permalink is not defined")))

(define-key slack-pinned-items-buffer-mode-map (kbd "RET") 'slack-pinned-items-open-message)
(define-key slack-pinned-items-buffer-mode-map (kbd "g") #'slack-pinned-items-refresh)

(provide 'slack-pinned-items-buffer)
;;; slack-pinned-items-buffer.el ends here
