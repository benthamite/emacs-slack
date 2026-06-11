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
  "Create and return a new search result buffer instance from PAYLOAD.
SEARCH-RESULT is the search-result argument.
TEAM is the team argument."
  (slack-if-let* ((buffer (slack-buffer-find 'slack-search-result-buffer team search-result)))
      buffer
    (make-instance 'slack-search-result-buffer
                   :team-id (oref team id)
                   :search-result search-result)))
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
  (with-slots (search-result) this
    (slack-search-has-next-page-p search-result)))

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

(cl-defmethod slack-buffer-request-history ((this slack-search-result-buffer) after-success &optional _on-error)
  "Request older history for THIS buffer from the Slack API.
AFTER-SUCCESS is the after-success argument."
  (with-slots (search-result) this
    (slack-search-request search-result after-success (slack-buffer-team this)
                          (slack-search-paging-next-page
                           (oref search-result pagination)))))

(cl-defmethod slack-buffer-init-buffer ((this slack-search-result-buffer))
  "Initialize and return the display buffer for THIS buffer."
  (let ((buffer (cl-call-next-method)))
    (with-current-buffer buffer
      (slack-search-result-buffer-mode)
      (slack-buffer-set-current-buffer this)
      (with-slots (search-result) this
        (let* ((messages (oref search-result matches)))
          (cl-loop for m in messages
                   do (slack-buffer-insert this m)))))
    buffer))

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

(defun slack-search-from-messages (query)
  "Run a Slack message search for QUERY and display the result buffer."
  (interactive
   (list (when (region-active-p)
           (substring-no-properties (funcall region-extract-function)))))
  (cl-destructuring-bind (team query sort sort-dir) (slack-search-query-params query)
    (let ((instance (make-instance 'slack-search-result
                                   :sort sort
                                   :sort-dir sort-dir
                                   :query query)))
      (cl-labels
          ((after-success ()
             (let ((buffer (slack-create-search-result-buffer instance team)))
               (slack-buffer-display buffer))))
        (slack-search-request instance #'after-success team)))))

(defun slack-search-from-files ()
  "Run a Slack file search prompted interactively and display the result buffer."
  (interactive)
  (cl-destructuring-bind (team query sort sort-dir) (slack-search-query-params)
    (let ((instance (make-instance 'slack-file-search-result
                                   :sort sort
                                   :sort-dir sort-dir
                                   :query query)))
      (cl-labels
          ((after-success ()
             (let ((buffer (slack-create-search-result-buffer instance team)))
               (slack-buffer-display buffer))))
        (slack-search-request instance #'after-success team)))))

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
