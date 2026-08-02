;;; slack-file-info-buffer.el ---                    -*- lexical-binding: t; -*-

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
(require 'slack-message)
(require 'slack-file)
(require 'slack-message-formatter)
(require 'slack-message-reaction)
(require 'slack-star)

(declare-function slack-file-request-info
                  "slack-file"
                  (file-id page team
                           &optional after-success on-error accept-result))

(defvar slack-file-link-keymap
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "RET") #'slack-file-display)
    (define-key map [mouse-1] #'slack-file-display)
    map))

(defun slack-file-display ()
  "Open the file info buffer for the file link at point."
  (interactive)
  (slack-if-let* ((id (get-text-property (point) 'file))
                  (buf slack-current-buffer))
      (slack-buffer-display-file buf id)))

(cl-defmethod slack-buffer-display-file ((this slack-buffer) file-id)
  "Display FILE-ID's stable info buffer next to THIS, then refresh it."
  (slack-file-info-buffer--present
   (slack-buffer-team this) file-id 1 t))

(define-derived-mode slack-file-info-buffer-mode slack-buffer-mode  "Slack File Info"
  (setq-local lui-max-buffer-size nil)
  (add-hook 'lui-post-output-hook 'slack-display-image t t))

(defclass slack-file-info-buffer (slack-buffer)
  ((file-id :initarg :file-id :type string)
   (file :initarg :file :initform nil :type (or null slack-file))))

(defun slack-file-info-buffer-page-key (file-id)
  "Return the durable file-detail page key for FILE-ID."
  (list 'file-info file-id))

(cl-defmethod slack-buffer-name ((this slack-file-info-buffer))
  "Return the display buffer name for THIS buffer."
  (format "*slack: %s File: %s*"
          (oref (slack-buffer-team this) name)
          (oref this file-id)))

(cl-defmethod slack-buffer-key
  ((_class (subclass slack-file-info-buffer)) file-id)
  "Return the class-level buffer key for FILE-ID's info buffer."
  file-id)

(cl-defmethod slack-buffer-key ((this slack-file-info-buffer))
  "Return the lookup key identifying the buffer for THIS buffer."
  (slack-buffer-key 'slack-file-info-buffer (oref this file-id)))

(cl-defmethod slack-team-buffer-key ((_class (subclass slack-file-info-buffer)))
  "Return the team-scoped class-level buffer key for the file info buffer."
  'slack-file-info-buffer)

(defun slack-create-file-info-buffer (team file-id &optional file)
  "Find or create FILE-ID's info buffer on TEAM, seeded with FILE."
  (let ((buffer
         (or (slack-buffer-find 'slack-file-info-buffer team file-id)
             (slack-file-info-buffer :team-id (oref team id)
                                     :file-id file-id
                                     :file file))))
    (when file
      (oset buffer file file))
    (slack-buffer-cache-team buffer team)
    buffer))

(cl-defmethod slack-buffer-init-buffer ((this slack-file-info-buffer))
  "Initialize and return the display buffer for THIS buffer."
  (let ((buf (cl-call-next-method)))
    (with-current-buffer buf
      (slack-file-info-buffer-mode)
      (slack-buffer-set-current-buffer this))
    buf))

(defun slack-file-info-buffer-render-page-state (buffer state)
  "Render exact file-info BUFFER from durable page STATE."
  (when (and (slot-boundp buffer 'buf)
             (buffer-live-p (oref buffer buf)))
    (when (slack-page-state-loaded-p state)
      (oset buffer file (slack-page-state-value state)))
    (with-current-buffer (oref buffer buf)
      (slack-buffer-widen
       (let ((inhibit-read-only t))
         (erase-buffer)
         (when (oref buffer file)
           (slack-buffer-insert buffer t))
         (goto-char (point-min))
         (slack-buffer-insert-page-status buffer state)
         (goto-char (point-min)))))))

(defun slack-file-info-buffer--page-loader (team file-id page state)
  "Return a file-info loader for FILE-ID comment PAGE on TEAM using STATE."
  (let* ((source-file (slack-file-find file-id team))
         (source-starred (and source-file (oref source-file is-starred))))
    (lambda (generation success error)
      (slack-file-request-info
       file-id page team
       (lambda (file &rest _ignored)
         (funcall success file nil nil))
       (lambda (&rest errors)
         (apply error errors))
       (lambda (file &rest _ignored)
         (when (and (= generation (slack-page-state-generation state))
                    (slack-page-state-in-flight-p state))
           (let ((current-file (slack-file-find file-id team)))
             (if (not (eq source-file current-file))
                 (progn
                   (funcall success current-file nil nil)
                   nil)
               (unless (eq source-starred
                           (and current-file
                                (oref current-file is-starred)))
                 (oset file is-starred (oref current-file is-starred)))
               t))))))))

(defun slack-file-info-buffer--present
    (team file-id page refresh &optional on-ready)
  "Present FILE-ID's comment PAGE on TEAM, refreshing when REFRESH is non-nil.
ON-READY receives the durable page state after a current result is rendered."
  (let* ((state (slack-team-page-state
                 team (slack-file-info-buffer-page-key file-id)))
         (cached (slack-file-find file-id team)))
    (when (and cached (not (slack-page-state-loaded-p state)))
      (slack-page-state-store state cached nil nil))
    (let ((buffer
           (slack-create-file-info-buffer
            team file-id (or (slack-page-state-value state) cached))))
      (slack-buffer-present-page
       buffer state
       (slack-file-info-buffer--page-loader team file-id page state)
       #'slack-file-info-buffer-render-page-state
       refresh on-ready)
      buffer)))

(cl-defmethod slack-buffer-download-file ((this slack-file-info-buffer) file-id)
  "Download the file at point in THIS buffer.
FILE-ID is the file-id argument."
  (slack-if-let* ((team (slack-buffer-team this))
                  (file (or (and (oref this file)
                                 (string= file-id (oref this file-id))
                                 (oref this file))
                            (slack-file-find file-id team))))
      (slack-file-download file team)))

(cl-defmethod slack-buffer-run-file-action ((this slack-file-info-buffer) file-id)
  "Run THIS buffer.
FILE-ID is the file-id argument."
  (slack-if-let* ((team (slack-buffer-team this))
                  (file (or (and (oref this file)
                                 (string= file-id (oref this file-id))
                                 (oref this file))
                            (slack-file-find file-id team))))
      (slack-file-run-action file this)))

(cl-defmethod slack-buffer-file-content-to-string ((this slack-file-info-buffer))
  "Return the HTML-with-CSS content of the file shown in buffer THIS as a string."
  (with-slots (file) this
    (slack-if-let* ((file file)
                    (content (oref file content))
                    (html (oref content content-highlight-html))
                    (css (oref content content-highlight-css)))
        (propertize (concat "<style>\n" css "</style>" "\n" html)
                    'slack-file-html-content t)
      "")))

(cl-defmethod slack-file-body-to-string ((file slack-file))
  "Render the body of the FILE as a string."
  (let* ((url (oref file url-private))
         (type (slack-file-type file))
         (size (slack-file-size file))
         (title (or (slack-file-title file) (oref file id))))
    (slack-format-message (propertize (format "<%s|%s>" url title)
                                      'face '(:weight bold))
                          (format "%s%s"
                                  (or (and size (format "%s " size)) "")
                                  type))))

(cl-defmethod slack-file-body-to-string ((this slack-file-email))
  "Render the body of THIS file email as a string."
  (let* ((label-face '(:foreground "#586e75" :weight bold))
         (from (format "%s %s"
                       (propertize "From:" 'face label-face)
                       (mapconcat #'(lambda (e) (oref e original))
                                  (oref this from)
                                  ", ")))
         (to (format "%s %s"
                     (propertize "To:" 'face label-face)
                     (mapconcat #'(lambda (e) (oref e original))
                                (oref this to)
                                ", ")))
         (cc (format "%s %s"
                     (propertize "CC:" 'face label-face)
                     (mapconcat #'(lambda (e) (oref e original))
                                (oref this cc)
                                ", ")))
         (subject (format "%s %s"
                          (propertize "Subject:" 'face label-face)
                          (propertize (oref this subject)
                                      'face '(:weight bold :height 1.1))))
         (body (propertize (format "\n%s" (oref this plain-text))
                           'slack-defer-face #'slack-put-email-body-overlay))
         (date (format "%s %s"
                       (propertize "Date:" 'face label-face)
                       (slack-format-ts (oref this created)))))
    (mapconcat #'identity
               (list from to cc subject date "" body)
               "\n")))

(cl-defmethod slack-message-to-string ((this slack-file-comment) team)
  "Render THIS file comment as a displayable string.
TEAM is the team argument."
  (with-slots (user comment) this
    (let ((name (or (slack-user-name user team) user))
          (status (slack-user-status user team)))
      (format "%s\n%s\n"
              (propertize (format "%s %s" name status)
                          'face 'slack-message-output-header)
              (slack-unescape comment team)))))

(cl-defmethod slack-buffer-file-to-string ((this slack-file-info-buffer))
  "Render a file attached to a message in THIS buffer as a string."
  (let* ((file (oref this file))
         (team (slack-buffer-team this))
         (user-name (or (slack-user-name (oref file user) team)
                        (oref file user)))
         (header (format "%s %s %s%s"
                         (propertize user-name
                                     'face '(:weight bold))
                         (if (oref file is-starred) ":star:" "")
                         (if (slack-file-downloadable-p file)
                             (format "%s "
                                     (slack-file-download-button file))
                           "")
                         (slack-file-action-button file)))
         (timestamp (and (oref file timestamp)
                         (format-time-string "%Y-%m-%d %H:%M:%S"
                                             (seconds-to-time
                                              (oref file timestamp)))))
         (body (slack-file-body-to-string file))
         (content (slack-buffer-file-content-to-string this))
         (thumb (or (and (slack-file-image-p file)
                         (slack-message-large-image-to-string file))
                    (slack-message-image-to-string file)))
         (comments (mapconcat #'(lambda (comment)
                                  (slack-message-to-string comment team))
                              (oref file comments)
                              "\n")))
    (propertize (slack-format-message header
                                      timestamp
                                      " "
                                      body
                                      " "
                                      content
                                      " "
                                      thumb
                                      " "
                                      comments)
                'file-id (oref file id))))

(cl-defmethod slack-buffer-insert ((this slack-file-info-buffer) &optional not-tracked-p)
  "Insert a rendered representation of THIS buffer into the current buffer.
NOT-TRACKED-P is the not-tracked-p argument."
  (when-let ((file (oref this file)))
    (let ((lui-time-stamp-position nil))
      (lui-insert-with-text-properties
       (slack-buffer-file-to-string this)
       ;; saved-text-properties not working??
       'file-id (oref file id)
       'ts (slack-ts file)
       'not-tracked-p not-tracked-p)))
  (slack-if-let* ((html-beg (cl-loop for i from (point-min) to lui-output-marker
                                     if (get-text-property i 'slack-file-html-content)
                                     return i))
                  (html-end (next-single-property-change html-beg
                                                         'slack-file-html-content))
                  (inhibit-read-only t))
      (shr-render-region html-beg html-end))
  (goto-char (point-min)))

(cl-defmethod slack-buffer-add-reaction-to-message ((this slack-file-info-buffer) reaction _ts)
  "Add a REACTION to the message selected in THIS buffer."
  (slack-file-add-reaction
   (oref this file-id) reaction (slack-buffer-team this)))

(cl-defmethod slack-buffer-add-star ((this slack-file-info-buffer) _ts)
  "Star the item at point in THIS buffer."
  (when-let ((file (oref this file)))
    (slack-star-api-request slack-message-stars-add-url
                            (slack-message-star-api-params file)
                            (slack-buffer-team this))))

(cl-defmethod slack-buffer-remove-star ((this slack-file-info-buffer) _ts)
  "Remove the star from THIS buffer."
  (when-let ((file (oref this file)))
    (slack-star-api-request slack-message-stars-remove-url
                            (slack-message-star-api-params file)
                            (slack-buffer-team this))))

(cl-defmethod slack-buffer--replace ((this slack-file-info-buffer) _ts)
  "Replace the rendered message identified by the argument in THIS buffer."
  (when (and (slot-boundp this 'buf)
             (buffer-live-p (oref this buf)))
    (slack-file-info-buffer-render-page-state
     this
     (slack-team-page-state
      (slack-buffer-team this)
      (slack-file-info-buffer-page-key (oref this file-id))))))

(cl-defmethod slack-buffer-update ((this slack-file-info-buffer))
  "Update THIS buffer after new data arrives."
  (when-let ((file (oref this file)))
    (let ((state
           (slack-team-page-state
            (slack-buffer-team this)
            (slack-file-info-buffer-page-key (oref this file-id)))))
      (unless (and (slack-page-state-in-flight-p state)
                   (eq file (slack-page-state-value state)))
        (slack-page-state-store state file nil nil))
      (slack-file-info-buffer-render-page-state this state))))

(defun slack-file-update ()
  "Refresh the file info buffer at point by re-requesting its contents."
  (interactive)
  (if (cl-typep slack-current-buffer 'slack-file-info-buffer)
      (let* ((buffer slack-current-buffer)
             (file (oref buffer file))
             (team (slack-buffer-team buffer))
             (file-id (oref buffer file-id))
             (page (if file (oref file page) 1)))
        (slack-file-info-buffer--present
         team file-id page t
         (lambda (state)
           (when-let* ((updated (slack-page-state-value state))
                       (file-list
                        (slack-buffer-find 'slack-file-list-buffer team)))
             (slack-buffer-replace file-list updated)))))
    (user-error "Current buffer is not a Slack file-info buffer")))

(cl-defmethod slack-file-run-action ((file slack-file) buf)
  "Prompt the user for an action on FILE shown in BUF and run it."
  (let* ((actions (list (and (not (slack-file-info-buffer-p buf))
                             (cons "View details"
                                   #'(lambda ()
                                       (slack-buffer-display-file
                                        buf
                                        (slack-file-id file)))))
                        (cons "Copy link to file"
                              #'(lambda ()
                                  (kill-new (oref file permalink))))
                        (if (oref file is-starred)
                            (cons "Remove file from saved"
                                  #'(lambda ()
                                      (slack-buffer-remove-star
                                       buf
                                       (slack-file-id file))))
                          (cons "Save file for later"
                                #'(lambda ()
                                    (slack-buffer-add-star
                                     buf
                                     (slack-file-id file)))))
                        (cons "Open original"
                              #'(lambda ()
                                  (browse-url (oref file url-private))))))
         (selected (completing-read "Action: "
                                    (cl-remove-if #'null actions)
                                    nil t))
         (action (cdr-safe (assoc-string selected actions))))
    (when action
      (funcall action))))

(provide 'slack-file-info-buffer)
;;; slack-file-info-buffer.el ends here
