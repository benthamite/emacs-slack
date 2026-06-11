;;; slack-message-editor.el ---  edit message interface  -*- lexical-binding: t; -*-

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
(require 'slack-util)
(require 'slack-room)
(require 'slack-message-sender)

(defconst slack-message-edit-url "https://slack.com/api/chat.update")
(defconst slack-message-edit-buffer-name "*Slack - Edit message*")
(defconst slack-message-write-buffer-name "*Slack - Write message*")
(defconst slack-message-share-buffer-name "*Slack - Share message*")
(defconst slack-share-url "https://slack.com/api/chat.shareMessage")
(defvar slack-completing-read-function)
(defvar slack-buffer-function)
(defvar-local slack-target-ts nil)
(defvar-local slack-message-edit-buffer-type nil)

(defvar slack-edit-message-mode-map
  (let ((keymap (make-sparse-keymap)))
    (define-key keymap (kbd "C-s C-m") #'slack-message-embed-mention)
    (define-key keymap (kbd "C-s C-c") #'slack-message-embed-channel)
    (define-key keymap (kbd "C-c C-k") #'slack-message-cancel-edit)
    (define-key keymap (kbd "C-c C-c") #'slack-message-send-from-buffer)
    keymap))

(define-derived-mode slack-edit-message-mode fundamental-mode "Slack Edit Msg"
  ""
  (slack-enable-wysiwyg)
  (slack-buffer-enable-emojify))

(cl-defun slack-message-share--send (team room ts msg &key on-success on-error)
  "Share message at TS from ROOM on TEAM to a user-selected channel with body MSG.
Call ON-SUCCESS once the server accepts the share, or ON-ERROR with the
error when the request fails at the API or transport level."
  (let* ((slack-room-list (slack-message-room-list team))
         (share-channel-id (oref (slack-select-from-list
                                     (slack-room-list
                                      "Select Channel: "))
                                 id)))
    (cl-labels
        ((on-share (&key data &allow-other-keys)
           (slack-request-handle-error
            (data "slack-message-share" on-error)
            (when (functionp on-success)
              (funcall on-success))))
         (on-request-error (&key error-thrown &allow-other-keys)
           (when (functionp on-error)
             (funcall on-error error-thrown))))
      (slack-request
       (slack-request-create
        slack-share-url
        team
        :type "POST"
        :params (append
                 (list (cons "channel" (oref room id))
                       (cons "timestamp" ts)
                       (cons "share_channel" share-channel-id))
                 ;; A blank comment would encode as blocks with null
                 ;; elements; omit the param entirely instead.
                 (unless (slack-string-blankp msg)
                   (list (cons "blocks"
                               (json-encode (cdr (car (with-temp-buffer
                                                        (insert msg)
                                                        (slack-create-blocks-from-buffer)))))))))
        :success #'on-share
        :error #'on-request-error)))))

(defun slack-message-cancel-edit ()
  "Abort the current edit/compose and close its buffer."
  (interactive)
  (let ((buffer (slack-buffer-buffer slack-current-buffer)))
    (with-current-buffer buffer
      (kill-buffer)
      (unless (and (equal slack-buffer-function #'switch-to-buffer) (> (count-windows) 1))
        (delete-window)))))

(defun slack-message-send-from-buffer ()
  "Send the entire current buffer text as a Slack message."
  (interactive)
  (slack-if-let* ((buf slack-current-buffer)
                  (text (buffer-substring-no-properties (point-min) (point-max))))
      (slack-buffer-send-message buf text)))

(cl-defun slack-message--edit (channel team ts text &key on-success on-error)
  "Edit the message at TS in CHANNEL for TEAM, replacing its body with TEXT.
Call ON-SUCCESS once the server accepts the edit, or ON-ERROR with the
error when the request fails at the API or transport level."
  (cl-labels ((on-edit (&key data &allow-other-keys)
                (slack-request-handle-error
                 (data "slack-message--edit" on-error)
                 (when (functionp on-success)
                   (funcall on-success))))
              (on-request-error (&key error-thrown &allow-other-keys)
                (when (functionp on-error)
                  (funcall on-error error-thrown))))
    (slack-request
     (slack-request-create
      slack-message-edit-url
      team
      :type "POST"
      :headers (list (cons "Content-Type"
                           "application/json;charset=utf-8"))
      :data (json-encode (apply #'list
                                (cons "channel" channel)
                                (cons "ts" ts)
                                (with-temp-buffer
                                  (insert text)
                                  (slack-create-blocks-from-buffer))))
      :success #'on-edit
      :error #'on-request-error))))

(provide 'slack-message-editor)
;;; slack-message-editor.el ends here
