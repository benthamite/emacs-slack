;;; slack-message-formatter.el --- format message text  -*- lexical-binding: t; -*-

;; Copyright (C) 2015  yuya.minami

;; Author: yuya.minami <yuya.minami@yuyaminami-no-MacBook-Pro.local>
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
(require 'slack-user)
(require 'slack-room)
(require 'slack-message)
(require 'slack-user-message)
(require 'slack-usergroup)
(require 'slack-thread)
(require 'slack-file)
(require 'slack-attachment)
(require 'slack-image)
(require 'slack-block)
(require 'slack-unescape)
(require 'slack-bot-message)
(require 'slack-message-faces)
(require 'slack-defcustoms)

(declare-function slack-emoji-resolve "slack-emoji")

(defvar slack-current-buffer)

(defcustom slack-visible-thread-sign ":left_speech_bubble: "
  "Used to thread message sign if visible-threads mode."
  :type 'string
  :group 'slack)

(defvar slack-reaction-keymap
  (let ((keymap (make-sparse-keymap)))
    (define-key keymap (kbd "RET") #'slack-reaction-toggle)
    (define-key keymap [mouse-1] #'slack-reaction-toggle)
    keymap))

(cl-defgeneric slack-buffer-toggle-reaction (buffer reaction)
  "Toggle REACTION on the current message of BUFFER.")

(defun slack-reaction-toggle ()
  "Toggle the reaction at point in the current Slack buffer."
  (interactive)
  (slack-if-let* ((buffer slack-current-buffer)
                  (reaction (get-text-property (point) 'reaction)))
      (slack-buffer-toggle-reaction buffer reaction)))

(cl-defgeneric slack-buffer-reaction-help-text (buffer reaction)
  "Return help-echo text describing REACTION for BUFFER.")

(defun slack-reaction-help-echo (_window _string pos)
  "Return the help-echo string for the reaction at POS."
  (slack-if-let* ((buffer slack-current-buffer)
                  (reaction (get-text-property pos 'reaction)))
      (slack-buffer-reaction-help-text buffer reaction)))


(cl-defmethod slack-reaction-to-string ((r slack-reaction) team)
  "Format reaction R for TEAM as a propertized string.
Eagerly resolves the emoji shortcode to a Unicode glyph when
available, so the result does not depend on post-hoc display
property rendering."
  (let* ((current-user-id (oref team self-id))
         (reaction-face (if (slack-reaction-user-reacted-p r current-user-id)
                            'slack-message-output-reaction-pressed
                          'slack-message-output-reaction))
         (glyph (slack-emoji-resolve (oref r name))))
    (propertize (format " %s %d " glyph (oref r count))
                'face reaction-face
                'mouse-face 'highlight
                'keymap slack-reaction-keymap
                'reaction r
                'help-echo #'slack-reaction-help-echo)))

(cl-defmethod slack-message-to-string ((m slack-message) team)
  "Create a propertized string for the message M of TEAM."
  (let* ((header (slack-message-header m team))
         (attachment (mapconcat #'(lambda (attachment)
                                    (slack-message-to-string attachment team))
                                (when slack-show-attachments-p (oref m attachments))
                                "\n"))
         (body (format "%s%s"
                       (if (slack-message-display-thread-sign-p m team)
                           slack-visible-thread-sign
                         "")
                       (slack-message-body m team)))
         (files (let ((ts (slack-ts m)))
                  (mapconcat #'(lambda (file)
                                 (slack-message-to-string file ts team))
                             (oref m files)
                             "\n\n")))
         (reactions (mapconcat #'(lambda (r) (slack-reaction-to-string r team))
                               (slack-message-reactions m)
                               " "))
         (thread (slack-thread-to-string m team)))
    (propertize
     (slack-format-message (propertize header
                                       'slack-message-header t)
                           (if (oref m deleted-at)
                               (slack-message-put-deleted-property body)
                             body)
                           files
                           attachment
                           (if (slack-string-blankp reactions) reactions
                             (concat "\n" reactions))
                           (if (slack-string-blankp thread) thread
                             (concat "\n" thread)))
     'permalink (oref m permalink))))

(cl-defmethod slack-file-deleted-p ((file slack-file))
  "Return non-nil when FILE has been deleted on Slack."
  (let ((mode (oref file mode)))
    (string= mode "tombstone")))

(defun slack-file-error-fallback (file team)
  "Build a fallback string for FILE when `slack-file-summary' fails.
Includes the file title, a link to open the file info buffer, and a
download button when the file is downloadable."
  (let ((title (ignore-errors (slack-file-title file)))
        (file-id (ignore-errors (oref file id))))
    (concat
     "<slack-file-summary error>"
     (when file-id
       (concat ": "
               (slack-file-link-info file-id
                                    (or (and title (slack-unescape title team))
                                        "untitled"))
               (when (slack-file-downloadable-p file)
                 (concat " " (slack-file-download-button file))))))))

(cl-defmethod slack-file-summary ((file slack-file) _ts team)
  "Return a short summary string for the file."
  (if (slot-boundp file 'permalink)
      (with-slots (mode permalink) file
        (if (slack-file-deleted-p file)
            "This file was deleted."
          (let ((type (slack-file-type file))
                (title (slack-file-title file)))
            (concat
             (when (string= (oref file mode) "snippet")
               (propertize (format "```\n%s\n```\n"(oref file preview)) 'face 'slack-preview-face))
             (format "uploaded this %s: %s <%s|open in browser>"
                     type
                     (slack-file-link-info (oref file id)
                                           (slack-unescape title team))
                     permalink))
            )))
    (slack-log (format "No permalink: %S" file)
               team :level 'warn)))

(defvar slack-expand-email-keymap
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "RET") #'slack-toggle-email-expand)
    map))

(cl-defgeneric slack-buffer-toggle-email-expand (buffer file-id)
  "Toggle expand/collapse of the email file FILE-ID in BUFFER.")

(defun slack-toggle-email-expand ()
  "Toggle expansion of the email file at point."
  (interactive)
  (let ((buffer slack-current-buffer))
    (slack-if-let* ((file-id (get-text-property (point) 'id)))
        (slack-buffer-toggle-email-expand buffer file-id))))

(cl-defmethod slack-file-summary ((this slack-file-email) ts team)
  "Return a short summary string for the file email."
  (with-slots (preview-plain-text plain-text is-expanded) this
    (let* ((has-more (< (length preview-plain-text)
                        (length plain-text)))
           (body (slack-unescape
                  (or (and is-expanded plain-text)
                      (or (and has-more (format "%s…" preview-plain-text))
                          preview-plain-text))
                  team)))
      (format "%s\n\n%s\n\n%s"
              (cl-call-next-method)
              (propertize body
                          'slack-defer-face #'slack-put-preview-overlay)
              (propertize (or (and is-expanded "Collapse ↑")
                              "+ Click to expand inline")
                          'ts ts
                          'id (oref this id)
                          'face '(:underline t)
                          'keymap slack-expand-email-keymap)))))

(cl-defmethod slack-message-to-string ((this slack-file) ts team)
  "Render the file as a displayable string."
  (if (slack-file-hidden-by-limit-p this)
      (slack-file-hidden-by-limit-message this)
    (let ((body (or (condition-case err
                        (slack-file-summary this ts team)
                      (error
                       (message "slack-message: file summary error: %S" err)
                       nil))
                    (slack-file-error-fallback this team)))
          (thumb (slack-image-string (slack-file-thumb-image-spec this))))
      (slack-format-message
       body
       (when thumb
         (propertize thumb
                     'slack-file-url (oref this url-private-download)))))))

(cl-defmethod slack-message-to-alert ((m slack-message) team)
  "Return a plain-text alert string for message M on TEAM."
  (with-slots (text attachments files) m
    (let ((alert-text
           (cond
            ((and text (< 0 (length text))) text)
            ((and attachments (< 0 (length attachments)))
             (mapconcat #'slack-attachment-to-alert attachments " "))
            ((and files (< 0 (length files)))
             (mapconcat #'(lambda (file) (oref file title)) files " ")))))
      (slack-unescape alert-text team))))

(provide 'slack-message-formatter)
;;; slack-message-formatter.el ends here
