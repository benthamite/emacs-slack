;;; slack-mrkdwn.el ---                              -*- lexical-binding: t; -*-

;; Copyright (C) 2019

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
(require 'slack-util)

(defconst slack-mrkdwn-regex-bold
  "\\(?:^\\|\\W\\)\\(\\(*\\)\\([^ \n\t\\]\\|[^ \n\t*]\\(?:.\\)*?[^\\ ]\\)\\(\\2\\)\\(\\W\\|$\\)\\)")

(defface slack-mrkdwn-bold-face
  '((t (:weight bold)))
  "Face used to between `*'"
  :group 'slack)

(defconst slack-mrkdwn-regex-italic
  "\\(?:^\\|\\W\\)\\(\\(_\\)\\([^\n\t]\\|[^\n\t*]\\(?:.\\)*?\\)\\(\\2\\)\\(\\W\\|$\\)\\)")

(defface slack-mrkdwn-italic-face
  '((t (:slant italic)))
  "Face used to between `_'"
  :group 'slack)

(defconst slack-mrkdwn-regex-strike
  "\\(?:^\\|\\W\\)\\(\\(~\\)\\([^ \n\t\\]\\|[^ \n\t]\\(?:.\\)*?[^\\ ]\\)\\(\\2\\)\\(\\W\\|$\\)\\)")

(defface slack-mrkdwn-strike-face
  '((t (:strike-through t)))
  "Face used to between `~'"
  :group 'slack)

(defconst slack-mrkdwn-regex-code
  "\\(\\(`\\)\\(\\(?:.\\)*?[^`]\\)\\(\\2\\)\\)\\(?:[^`]\\|\\'\\)")

(defface slack-mrkdwn-code-face
  '((t (:inherit slack-preview-face)))
  "Face used to between ``'"
  :group 'slack)

(defconst slack-mrkdwn-regex-code-block "\\(?:^\\|[[:blank:]]\\)\\(```\\)\\(?:\n\\)?\\(\\(.\\|\n\\)*?\\)\\(\n?```\\)[[:blank:]]*$")

;; take from markdown-mode
(defconst slack-mrkdwn-regex-link-inline
  "\\(?1:!\\)?\\(?2:\\[\\)\\(?3:\\^?\\(?:\\\\\\]\\|[^]]\\)*\\|\\)\\(?4:\\]\\)\\(?5:(\\)\\s-*\\(?6:[^)]*?\\)\\(?:\\s-+\\(?7:\"[^\"]*\"\\)\\)?\\s-*\\(?8:)\\)"
  "Regular expression for a [text](file) or an image link ![text](file).
Group 1 matches the leading exclamation point (optional).
Group 2 matches the opening square bracket.
Group 3 matches the text inside the square brackets.
Group 4 matches the closing square bracket.
Group 5 matches the opening parenthesis.
Group 6 matches the URL.
Group 7 matches the title (optional).
Group 8 matches the closing parenthesis.")

(defface slack-mrkdwn-code-block-face
  '((t (:inherit slack-preview-face)))
  "Face used to between ````'"
  :group 'slack)

(defconst slack-mrkdwn-regex-blockquote
  "^[ \t]*\\([A-Z]?>\\)\\([ \t]*\\)\\(.+\\)$")

(defface slack-mrkdwn-blockquote-face
  '((t (:inherit font-lock-doc-face)))
  "Face used to `>'"
  :group 'slack)

(defcustom slack-mrkdwn-blockquote-sign "┃"
  "Used to display > when blockquote"
  :group 'slack
  :type 'string)

(defconst slack-mrkdwn-regex-list "^\\([[:blank:]]*\\)\\([0-9]+\\.\\|[-*]\\)\\([[:blank:]]\\)\\(.*\\)$")

(defface slack-mrkdwn-list-face
  '((t (:foreground "#A9A9A9" :weight bold)))
  "Face used to mrkdwn list"
  :group 'slack)

(defcustom slack-mrkdwn-list-bullet "•"
  "Used to display unordered list bullet"
  :group 'slack
  :type 'string)

(defun slack-mrkdwn-plain-text-p (point)
  "Return non-nil when text at POINT is plain text (not wysiwyg-marked)."
  (unless (slack-wysiwyg-enabled-p)
    (let ((text-type (get-text-property point 'slack-text-type)))
      (or (null text-type)
          (eq 'plain text-type)))))

(defun slack-mrkdwn-add-face ()
  "Apply all mrkdwn faces (bold, italic, code, list, etc.) in the current buffer."
  (slack-mrkdwn-mark-code-block)
  (slack-mrkdwn-add-bold-face)
  (slack-mrkdwn-add-italic-face)
  (slack-mrkdwn-add-strike-face)
  (slack-mrkdwn-add-code-face)
  (slack-mrkdwn-add-code-block-face)
  (slack-mrkdwn-add-blockquote-face)
  (slack-mrkdwn-add-list-face))

(defun slack-mrkdwn-inside-code-p (point)
  "Return non-nil when POINT is inside an inline code span or code block."
  (or (slack-mrkdwn-inside-code-block-p point)
      (slack-mrkdwn-inside-inline-code-p point)))

(defun slack-mrkdwn-inside-code-block-p (point)
  "Return non-nil when POINT is inside a fenced code block."
  (let ((block-type (get-text-property point 'slack-code-block-type)))
    (eq 'block block-type)))

(defun slack-mrkdwn-inside-inline-code-p (point)
  "Return non-nil when POINT is inside an inline code span."
  (let ((block-type (get-text-property point 'slack-code-block-type)))
    (eq 'inline block-type)))

(defun slack-mrkdwn-mark-code-block ()
  "Tag code-block and inline-code regions with `slack-code-block-type' properties."
  (goto-char (point-min))
  (while (re-search-forward slack-mrkdwn-regex-code-block (point-max) t)
    (slack-if-let* ((beg (match-beginning 2))
                    (end (match-end 2)))
        (unless (slack-mrkdwn-plain-text-p beg)
          (put-text-property beg end
                             'slack-code-block-type 'block))))

  (goto-char (point-min))
  (while (re-search-forward slack-mrkdwn-regex-code (point-max) t)
    (slack-if-let* ((beg (match-beginning 3))
                    (end (match-end 3)))
        (unless (or (slack-mrkdwn-plain-text-p beg)
                    (slack-mrkdwn-inside-code-p beg))
          (put-text-property beg end
                             'slack-code-block-type 'inline)))))

(defun slack-mrkdwn-add-bold-face ()
  "Apply `slack-mrkdwn-bold-face' to bold spans and hide their delimiters."
  (goto-char (point-min))
  (while (re-search-forward slack-mrkdwn-regex-bold (point-max) t)
    (slack-if-let* ((beg (match-beginning 3))
                    (end (match-end 3)))
        (unless (or (slack-mrkdwn-plain-text-p beg)
                    (slack-mrkdwn-inside-code-p beg))
          (put-text-property beg end 'face 'slack-mrkdwn-bold-face)
          (slack-if-let* ((markup-start-beg (match-beginning 2))
                          (markup-start-end (match-end 2)))
              (put-text-property markup-start-beg
                                 markup-start-end
                                 'invisible t))
          (slack-if-let* ((markup-end-beg (match-beginning 4))
                          (markup-end-end (match-end 4)))
              (put-text-property markup-end-beg
                                 markup-end-end
                                 'invisible t))))))

(defun slack-mrkdwn-add-italic-face ()
  "Apply `slack-mrkdwn-italic-face' to italic spans and hide their delimiters."
  (goto-char (point-min))
  (while (re-search-forward slack-mrkdwn-regex-italic (point-max) t)
    (slack-if-let* ((beg (match-beginning 3))
                    (end (match-end 3)))
        (unless (or (slack-mrkdwn-plain-text-p beg)
                    (slack-mrkdwn-inside-code-p beg))
          (put-text-property beg end 'face 'slack-mrkdwn-italic-face)
          (slack-if-let* ((markup-start-beg (match-beginning 2))
                          (markup-start-end (match-end 2)))
              (put-text-property markup-start-beg
                                 markup-start-end
                                 'invisible t))
          (slack-if-let* ((markup-end-beg (match-beginning 4))
                          (markup-end-end (match-end 4)))
              (put-text-property markup-end-beg
                                 markup-end-end
                                 'invisible t))))))

(defun slack-mrkdwn-add-strike-face ()
  "Apply `slack-mrkdwn-strike-face' to strikethrough spans and hide their delimiters."
  (goto-char (point-min))
  (while (re-search-forward slack-mrkdwn-regex-strike (point-max) t)
    (slack-if-let* ((beg (match-beginning 3))
                    (end (match-end 3)))
        (unless (or (slack-mrkdwn-plain-text-p beg)
                    (slack-mrkdwn-inside-code-p beg))
          (put-text-property beg end 'face 'slack-mrkdwn-strike-face)
          (slack-if-let* ((markup-start-beg (match-beginning 2))
                          (markup-start-end (match-end 2)))
              (put-text-property markup-start-beg
                                 markup-start-end
                                 'invisible t))
          (slack-if-let* ((markup-end-beg (match-beginning 4))
                          (markup-end-end (match-end 4)))
              (put-text-property markup-end-beg
                                 markup-end-end
                                 'invisible t))))))

(defun slack-mrkdwn-add-code-face ()
  "Apply `slack-mrkdwn-code-face' to inline code spans and hide their backticks."
  (goto-char (point-min))
  (while (re-search-forward slack-mrkdwn-regex-code (point-max) t)
    (slack-if-let* ((beg (match-beginning 3))
                    (end (match-end 3)))
        (unless (or (slack-mrkdwn-plain-text-p beg)
                    (slack-mrkdwn-inside-code-block-p beg))
          (put-text-property beg end 'face 'slack-mrkdwn-code-face)
          (slack-if-let* ((markup-start-beg (match-beginning 2))
                          (markup-start-end (match-end 2)))
              (put-text-property markup-start-beg
                                 markup-start-end
                                 'invisible t))
          (slack-if-let* ((markup-end-beg (match-beginning 4))
                          (markup-end-end (match-end 4)))
              (put-text-property markup-end-beg
                                 markup-end-end
                                 'invisible t))))))


(defun slack-mrkdwn-add-code-block-face ()
  "Apply `slack-mrkdwn-code-block-face' to fenced code blocks and hide fences."
  (goto-char (point-min))
  (while (re-search-forward slack-mrkdwn-regex-code-block (point-max) t)
    (slack-if-let* ((beg (match-beginning 2))
                    (end (match-end 2)))
        (unless (slack-mrkdwn-plain-text-p beg)
          (overlay-put (make-overlay beg end)
                       'face 'slack-mrkdwn-code-block-face)
          (put-text-property beg end 'face 'slack-mrkdwn-code-face)
          (slack-if-let* ((markup-start-beg (match-beginning 1))
                          (markup-start-end (match-end 1)))
              (put-text-property markup-start-beg
                                 markup-start-end
                                 'invisible t))
          (slack-if-let* ((markup-end-beg (match-beginning 4))
                          (markup-end-end (match-end 4)))
              (put-text-property markup-end-beg
                                 markup-end-end
                                 'invisible t))))))

(defun slack-mrkdwn-add-blockquote-face ()
  "Apply `slack-mrkdwn-blockquote-face' to blockquotes and replace the leading marker."
  (goto-char (point-min))
  (while (re-search-forward slack-mrkdwn-regex-blockquote (point-max) t)
    (slack-if-let* ((beg (match-beginning 3))
                    (end (match-end 3)))
        (unless (or (slack-mrkdwn-plain-text-p beg)
                    (slack-mrkdwn-inside-code-p beg))
          (put-text-property beg end
                             'face 'slack-mrkdwn-blockquote-face)
          (slack-if-let* ((markup-start-beg (match-beginning 1))
                          (markup-start-end (match-end 1)))
              (progn
                (put-text-property markup-start-beg
                                   markup-start-end
                                   'face
                                   'slack-mrkdwn-blockquote-face)
                (put-text-property markup-start-beg
                                   markup-start-end
                                   'display slack-mrkdwn-blockquote-sign)))))))

(defun slack-mrkdwn-add-list-face ()
  "Apply `slack-mrkdwn-list-face' to list markers, substituting a bullet glyph."
  (goto-char (point-min))
  (while (re-search-forward slack-mrkdwn-regex-list (point-max) t)
    (slack-if-let* ((beg (match-beginning 2))
                    (end (match-end 2)))
        (unless (or (slack-mrkdwn-plain-text-p beg)
                    (slack-mrkdwn-inside-code-p beg))
          (put-text-property beg end
                             'face 'slack-mrkdwn-list-face)
          (when (let ((list-sign (match-string 2)))
                  (or (string= "-" list-sign)
                      (string= "*" list-sign)))
            (put-text-property beg end
                               'display slack-mrkdwn-list-bullet))))))

(provide 'slack-mrkdwn)
;;; slack-mrkdwn.el ends here
