;;; slack-message-sender.el --- slack message concern message sending  -*- lexical-binding: t; -*-

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
(require 'json)
(require 'slack-util)
(require 'slack-room)
(require 'slack-im)
(require 'slack-group)
(require 'slack-message)
(require 'slack-channel)
(require 'slack-conversations)
(require 'slack-usergroup)
(require 'slack-mrkdwn)

(defvar slack-completing-read-function)
(defvar slack-buffer-function)

(declare-function slack-message-create "slack-create-message")
(declare-function slack-message-update-buffer "slack-message-buffer")
(defvar slack-current-buffer)

(defconst slack-channel-mention-regex "\\(<#\\([A-Za-z0-9]+\\)>\\)")
(defconst slack-user-mention-regex "\\(<@\\([A-Za-z0-9]+\\)>\\)")
(defconst slack-usergroup-mention-regex "\\(<!subteam^\\([A-Za-z0-9]+\\)>\\)")
(defconst slack-special-mention-regex "\\(<!\\(here\\|channel\\|everyone\\)>\\)")
(defconst slack-file-upload-complete-url "https://slack.com/api/files.completeUpload")

(defun slack-list-indent-level (leading-space)
  "Given a LEADING-SPACE we count how many indentation levels there are.
In this context an indentation level is a pair of spaces."
  (let* ((normalized (replace-regexp-in-string "\t" "  " leading-space))
         (space-count (length normalized)))
    (floor (/ space-count 2))))

(cl-defun slack-message-send-internal (message room team &key (on-success nil) (on-error nil) (payload nil) (files nil) (joined nil))
  "Send MESSAGE to ROOM on TEAM, joining the channel first if needed.
FILES is a list of file paths to upload.  JOINED prevents infinite
recursion when the join/open callback re-invokes this function.
ON-SUCCESS is the on-success argument.
ON-ERROR is the on-error argument."
  (when (slack-string-blankp message)
    (error "Empty message"))
  ;; Phase 1: ensure membership (mpim rooms can report is-member=false
  ;; even when the user is a member; conversations.open fixes this)
  (if (and (slack-channel-p room)
           (not (oref room is-member))
           (not joined))
      (if (or (oref room is-mpim) (oref room is-im))
          (slack-conversations-open
           team :room room
           :user-ids nil :on-success
           #'(lambda (_data)
               (slack-message-send-internal message
                                            room
                                            team
                                            :on-success on-success
                                            :on-error on-error
                                            :payload payload
                                            :files files
                                            :joined t)))
        (slack-conversations-join
         room team
         #'(lambda (_data) (slack-message-send-internal message
                                                        room
                                                        team
                                                        :on-success on-success
                                                        :on-error on-error
                                                        :payload payload
                                                        :files files
                                                        :joined t
                                                        ))))
    ;; Phase 2: send (upload files first if present, then post message)
    (if files
        (slack-message-upload-files team
                                    files
                                    :on-error on-error
                                    :on-success #'(lambda (files) (let ((message-payload (append (apply #'list
                                                                                                        (cons "channel" (oref room id))
                                                                                                        (with-temp-buffer
                                                                                                          (insert message)
                                                                                                          (slack-create-blocks-from-buffer)))
                                                                                                 payload)))
                                                                    (slack-files-upload-complete team
                                                                                                 files
                                                                                                 message-payload
                                                                                                 :on-success on-success
                                                                                                 :on-error on-error))))
      (let ((message-payload (append (apply #'list
                                            (cons "type" "message")
                                            (cons "channel" (oref room id))
                                            (with-temp-buffer
                                              (insert message)
                                              (slack-create-blocks-from-buffer)))
                                     payload)))
        (slack-chat-post-message team
                                 message-payload
                                 :on-success on-success
                                 :on-error on-error)))))

(cl-defun slack-chat-post-message (team message &key (on-success nil) (on-error nil))
  "Send MESSAGE via `chat.postMessage' on TEAM.
MESSAGE is a Slack API payload plist.  Invoke ON-SUCCESS once the
server acknowledges the post, or ON-ERROR when it returns an error
response."
  (cl-labels
      ((success (&key data &allow-other-keys)
         (if (eq t (plist-get data :ok))
             (progn
               (slack-chat-post-message--echo data team)
               (when on-success (funcall on-success)))
           (if on-error
               (funcall on-error data)
             (slack-log (format "Failed to post message. Error: %s, meta: %s"
                                (plist-get data :error)
                                (when (plist-get data :response_metadata)
                                  (mapconcat 'identity
                                             (plist-get (plist-get data :response_metadata)
                                                        :messages)
                                             "\n")))
                        team
                        :level 'error)))))
    (slack-request
     (slack-request-create
      "https://slack.com/api/chat.postMessage"
      team
      :type "POST"
      :data (json-encode message)
      :headers (list (cons "Content-Type"
                           "application/json;charset=utf-8"))
      :success #'success))))

(defun slack-chat-post-message--echo (data team)
  "Echo the sent message from chat.postMessage response DATA locally.
Creates the message from the API response and pushes it through
the standard buffer-update machinery so it appears immediately.
TEAM is the team argument."
  (let* ((channel (plist-get data :channel))
         (msg-data (plist-get data :message))
         (room (and channel (slack-room-find channel team))))
    (when (and room msg-data)
      (plist-put msg-data :channel channel)
      (let ((message (slack-message-create msg-data team room)))
        (when message
          (slack-room-push-message room message team)
          (slack-message-update-buffer message team))))))

(defun slack-message-room-list (team)
  "Return the combined list of group, IM, and channel names for TEAM."
  (append (slack-group-names team)
          (slack-im-names team)
          (slack-channel-names team)))

(defun slack-message-embed-channel ()
  "Prompt for a channel of the current team and insert a channel mention."
  (interactive)
  (slack-if-let* ((buf slack-current-buffer)
                  (team (slack-buffer-team buf)))
      (slack-select-from-list
          ((slack-channel-names team) "Select Channel: ")
          (slack-insert-channel-mention (oref selected id)
                                        (format "#%s" (slack-room-name selected team))))))

(defun slack-insert-channel-mention (channel-id display)
  "Insert a mention linking to CHANNEL-ID, shown to the user as DISPLAY."
  (insert (slack-propertize-mention-text 'slack-message-mention-face
                                         display
                                         (format "<#%s>" channel-id))))

(defun slack-insert-user-mention (user-id display)
  "Insert a mention linking to USER-ID, shown to the user as DISPLAY."
  (insert (slack-propertize-mention-text 'slack-message-mention-face
                                         display
                                         (format "<@%s>" user-id))))

(defun slack-insert-usergroup-mention (usergroup-id display)
  "Insert a mention linking to USERGROUP-ID, shown to the user as DISPLAY."
  (insert (slack-propertize-mention-text 'slack-message-mention-keyword-face
                                         display
                                         (format "<!subteam^%s>" usergroup-id))))

(defun slack-insert-keyword-mention (keyword display)
  "Insert a broadcast-style mention for KEYWORD, shown as DISPLAY."
  (insert (slack-propertize-mention-text 'slack-message-mention-keyword-face
                                         display
                                         (format "<!%s>" keyword))))

(defun slack-message-embed-mention ()
  "Prompt for a user, usergroup, or broadcast keyword and insert its mention."
  (interactive)
  (slack-if-let* ((buf slack-current-buffer)
                  (team (slack-buffer-team buf)))
      (let* ((keyworkds (list (list "here" :name "here" :type 'keyword)
                              (list "channel" :name "channel" :type 'keyword)
                              (list "everyone" :name "everyone" :type 'keyword)))
             (usergroups (mapcar #'(lambda (e) (list (oref e handle)
                                                     :id (oref e id)
                                                     :name (oref e handle)
                                                     :type 'usergroup))
                                 (cl-remove-if #'slack-usergroup-deleted-p
                                               (oref team usergroups))))
             (alist (append keyworkds
                            ;; show on selection if user is active or not
                            (slack-user-name-alist
                             team
                             :filter
                             #'(lambda (users)
                                 (cl-remove-if #'slack-user-hidden-p users)))
                            usergroups)))
        (slack-select-from-list
            (alist "Select User: ")
            (cl-case (plist-get selected :type)
              (keyword
               (slack-insert-keyword-mention (plist-get selected :name)
                                             (concat "@" (plist-get selected :name))))
              (usergroup
               (slack-insert-usergroup-mention (plist-get selected :id)
                                               (concat "@" (plist-get selected :name))))
              (t
               (slack-insert-user-mention (plist-get selected :id)
                                          (concat "@" (slack-user--name selected team)))))))))

(defvar slack-enable-wysiwyg)

(defun slack-enable-wysiwyg ()
  "Install the WYSIWYG refresh hook when `slack-enable-wysiwyg' is set."
  (when slack-enable-wysiwyg
    (add-hook 'after-change-functions
              'slack-wysiwyg-after-change nil t)))

(defun slack-wysiwyg-enabled-p ()
  "Return non-nil when WYSIWYG rendering is active in the current buffer."
  (and slack-enable-wysiwyg
       (or (eq 'slack-message-compose-buffer-mode
               major-mode)
           (eq 'slack-message-edit-buffer-mode
               major-mode))))

(defun slack-wysiwyg-after-change (_beg _end _length)
  "Re-apply WYSIWYG faces over the whole buffer after any edit."
  (when (slack-wysiwyg-enabled-p)
    (save-excursion
      (save-restriction
        (put-text-property (point-min) (point-max) 'face nil)
        (put-text-property (point-min) (point-max) 'invisible nil)
        (put-text-property (point-min) (point-max) 'slack-code-block-type nil)
        (put-text-property (point-min) (point-max) 'display nil)
        (remove-overlays (point-min) (point-max))
        (slack-mrkdwn-add-face)
        (mapc #'(lambda (regex)
                  (goto-char (point-min))
                  (while (re-search-forward regex (point-max) t)
                    (unless (slack-mrkdwn-inside-code-p (match-beginning 0))
                      (let* ((beg (match-beginning 0))
                             (end (match-end 0))
                             (props (get-text-property beg 'slack-mention-props)))
                        (when props
                          (let ((properties (append (plist-get props :props) nil)))
                            (while (< 0 (length properties))
                              (put-text-property beg end
                                                 (pop properties)
                                                 (pop properties)))))))))
              (list slack-user-mention-regex
                    slack-usergroup-mention-regex
                    slack-channel-mention-regex
                    slack-special-mention-regex))))))

(defun slack-put-block-props (beg end value)
  "Apply VALUE as the `slack-block-props' text property on BEG..END."
  (put-text-property beg end 'slack-block-props value))

(defun slack-put-section-block-props (beg end value)
  "Apply VALUE as the `slack-section-block-props' text property on BEG..END."
  (put-text-property beg end 'slack-section-block-props value))

(defun slack-mark-inline-format (regex type &optional beg-group)
  "Mark inline formatting matches for REGEX with TYPE.
BEG-GROUP is the match group for the beginning of the block
properties region (default 1)."
  (let ((beg-group (or beg-group 1)))
    (goto-char (point-min))
    (while (re-search-forward regex (point-max) t)
      (unless (slack-mark-inside-code-p (match-beginning 1))
        (slack-put-block-props (match-beginning beg-group)
                               (match-end 4)
                               (list :type type
                                     :text (match-string 3)))))))

(defun slack-mark-bold ()
  "Mark bold inline spans with block properties throughout the buffer."
  (slack-mark-inline-format slack-mrkdwn-regex-bold 'bold))

(defun slack-mark-italic ()
  "Mark italic inline spans with block properties throughout the buffer."
  (slack-mark-inline-format slack-mrkdwn-regex-italic 'italic))

(defun slack-mark-strike ()
  "Mark strike inline spans with block properties throughout the buffer."
  (slack-mark-inline-format slack-mrkdwn-regex-strike 'strike))

(defun slack-mark-code ()
  "Mark inline code spans with block properties throughout the buffer."
  ;; Group 2: the code regex captures the opening backtick in group 1
  ;; (boundary char) and the content starts at group 2.
  (slack-mark-inline-format slack-mrkdwn-regex-code 'code 2))

(defun slack-mark-code-block ()
  "Mark fenced code-block sections with section-block properties."
  (goto-char (point-min))
  (while (re-search-forward slack-mrkdwn-regex-code-block (point-max) t)
    (slack-put-section-block-props (match-beginning 0)
                                   (match-end 0)
                                   (list :section-type 'code-block
                                         :end (+ 3 (match-end 0)) ;; skip closing ``` (3 chars)
                                         :element-beg (match-beginning 2)
                                         :element-end (match-end 2)))))

(defun slack-mark-blockquote ()
  "Mark blockquote sections with section-block properties."
  (goto-char (point-min))
  (while (re-search-forward slack-mrkdwn-regex-blockquote (point-max) t)
    (unless (slack-mark-inside-code-p (match-beginning 0))
      (slack-put-section-block-props (match-beginning 0)
                                     (match-end 0)
                                     (list :section-type 'blockquote
                                           :element-beg (match-beginning 3)
                                           :element-end (match-end 3))))))

(defun slack-mark-list ()
  "Mark bullet and ordered list items with section-block properties."
  (goto-char (point-min))
  (while (re-search-forward slack-mrkdwn-regex-list (point-max) t)
    (unless (slack-mark-inside-code-p (match-beginning 0))
      (let* ((list-sign (match-string 2))
             (list-style (if (or (string= "-" list-sign)
                                 (string= "*" list-sign))
                             "bullet"
                           "ordered"))
             (list-indent (slack-list-indent-level (match-string 1))))
        (slack-put-section-block-props (match-beginning 0)
                                       (match-end 0)
                                       (list :section-type 'list
                                             :style list-style
                                             :indent list-indent
                                             :element-beg (match-beginning 4)
                                             :element-end (match-end 4)))))))

(defun slack-mark-mentions ()
  "Mark user, usergroup, channel, and broadcast mentions in the buffer."
  (goto-char (point-min))
  (while (re-search-forward slack-user-mention-regex (point-max) t)
    (unless (slack-mark-inhibit-mention-p (match-beginning 1))
      (slack-put-block-props (match-beginning 1)
                             (match-end 1)
                             (list :type 'user
                                   :user-id (match-string 2)))))
  (goto-char (point-min))
  (while (re-search-forward slack-usergroup-mention-regex (point-max) t)
    (unless (slack-mark-inhibit-mention-p (match-beginning 1))
      (slack-put-block-props (match-beginning 1)
                             (match-end 1)
                             (list :type 'usergroup
                                   :usergroup-id (match-string 2)))))
  (goto-char (point-min))
  (while (re-search-forward slack-channel-mention-regex (point-max) t)
    (unless (slack-mark-inhibit-mention-p (match-beginning 1))
      (slack-put-block-props (match-beginning 1)
                             (match-end 1)
                             (list :type 'channel
                                   :channel-id (match-string 2)))))
  (goto-char (point-min))
  (while (re-search-forward slack-special-mention-regex (point-max) t)
    (unless (slack-mark-inhibit-mention-p (match-beginning 1))
      (slack-put-block-props (match-beginning 1)
                             (match-end 1)
                             (list :type 'broadcast
                                   :range (match-string 2))))))

(defun slack-mark-emojis ()
  "Mark `:shortcode:' emoji occurrences in the buffer with block properties."
  (goto-char (point-min))
  (while (re-search-forward ":\\([a-z0-9_-]+\\):" (point-max) t)
    (unless (slack-mark-inside-code-p (match-beginning 0))
      (slack-put-block-props (match-beginning 0)
                             (match-end 0)
                             (list :type 'emoji
                                   :name (match-string 1))))))

(defun slack-mark-links ()
  "Add slack text property to markdown links."
  (goto-char (point-min))
  (let ((regex-url-scheme (regexp-opt thing-at-point-uri-schemes))
        (regex-markdown-url slack-mrkdwn-regex-link-inline))
    (save-excursion
      (while (re-search-forward (rx (or (regex regex-markdown-url) (regex regex-url-scheme))) (point-max) t)
        (unless (slack-mark-inside-code-p (match-beginning 0))
          (if (match-beginning 6)
              ;; markdown urls with text
              (let ((bounds (cons (match-beginning 0) (match-end 0))))
                (when bounds
                  (slack-put-block-props (car bounds)
                                         (cdr bounds)
                                         (list :type 'link
                                               :text (buffer-substring-no-properties (match-beginning 3)
                                                                                     (match-end 3))
                                               :url (buffer-substring-no-properties (match-beginning 6)
                                                                                    (match-end 6))))))
            ;; plain urls
            (let ((bounds (bounds-of-thing-at-point 'url)))
              (when bounds
                (slack-put-block-props (car bounds)
                                       (cdr bounds)
                                       (list :type 'link
                                             :url (buffer-substring-no-properties (car bounds)
                                                                                  (cdr bounds))))))))))))

(defun slack-mark-inhibit-mention-p (point)
  "Return non-nil when mention parsing should be skipped at POINT."
  (or (slack-mark-inside-code-p point)
      (slack-mark-inside-bold-p point)))

(defun slack-mark-inside-code-p (point)
  "Return non-nil when POINT lies inside an inline code or code-block span."
  (slack-if-let* ((props (or (get-text-property point 'slack-block-props)
                             (get-text-property point 'slack-section-block-props))))
      (or (eq 'code (plist-get props :type))
          (eq 'code-block (plist-get props :section-type)))))

(defun slack-mark-inside-bold-p (point)
  "Return non-nil when POINT lies inside a bold span."
  (slack-if-let* ((props (get-text-property point 'slack-block-props)))
      (eq 'bold (plist-get props :type))))

(defun slack-mark-rich-text-elements ()
  "Mark every kind of rich-text inline element in the buffer."
  (slack-mark-bold)
  (slack-mark-italic)
  (slack-mark-strike)
  (slack-mark-code)
  (slack-mark-mentions)
  (slack-mark-emojis)
  (slack-mark-links))

(defun slack-create-blocks-from-buffer ()
  "Create and return a new blocks from buffer instance from PAYLOAD."
  (interactive)
  (with-current-buffer (current-buffer)
    (slack-mark-rich-text-elements)
    (slack-mark-code-block)
    (slack-mark-blockquote)
    (slack-mark-list)
    (cl-labels ((with-ranges (ranges cb &optional before-mark)
                  (let ((str (mapconcat #'(lambda (range)
                                            (buffer-substring-no-properties
                                             (car range)
                                             (cdr range)))
                                        (reverse ranges)
                                        "\n")))
                    (with-temp-buffer
                      (insert str)
                      (when before-mark
                        (funcall before-mark))
                      (slack-mark-rich-text-elements)
                      (funcall cb))))
                (create-elements-from-ranges (ranges &optional before-mark)
                  (when (< 0 (length ranges))
                    (with-ranges ranges
                                 #'(lambda () (create-elements (point-min)
                                                               (point-max)))
                                 before-mark)))
                (create-section-elements-from-ranges (ranges)
                  (when (< 0 (length ranges))
                    (with-ranges ranges
                                 #'(lambda ()
                                     (create-section-elements (point-min)
                                                              (point-max))))))
                (create-section-elements (start end)
                  (let* ((cur-point start)
                         (elements nil)
                         (section-elements nil)
                         (preformatted-ranges nil)
                         (blockquote-ranges nil)
                         (list-style nil)
                         (list-indent nil)
                         (list-ranges nil))
                    (cl-labels ((commit-block (type block-elements &rest props)
                                  (when (< 0 (length block-elements))
                                    (let ((e (list (cons "type" type)
                                                   (cons "elements" block-elements))))
                                      (dolist (prop props)
                                        (push prop e))
                                      (push e elements))))
                                (commit-section-block ()
                                  (when (commit-block "rich_text_section"
                                                      (reverse section-elements))
                                    (setq section-elements nil)))
                                (commit-preformatted-block ()
                                  (when (commit-block "rich_text_preformatted"
                                                      (create-elements-from-ranges
                                                       preformatted-ranges
                                                       #'(lambda ()
                                                           (slack-put-section-block-props (point-min) (point-max)
                                                                                          (list :section-type 'code-block)))))
                                    (setq preformatted-ranges nil)))
                                (commit-blockquote-block ()

                                  (when (commit-block "rich_text_quote"
                                                      (create-elements-from-ranges
                                                       blockquote-ranges))
                                    (setq blockquote-ranges nil)))
                                (commit-list-block ()
                                  (when (commit-block "rich_text_list"
                                                      (cl-mapcan #'(lambda (range)
                                                                     (create-section-elements-from-ranges
                                                                      (list range)))
                                                                 (reverse list-ranges))
                                                      (cons "style" list-style)
                                                      (cons "indent" list-indent))
                                    (setq list-ranges nil)
                                    (setq list-style nil)
                                    (setq list-indent nil))))
                      (while (and cur-point (< cur-point end))
                        (let* ((block-props (get-text-property cur-point 'slack-section-block-props))
                               (section-type (and block-props (plist-get block-props :section-type)))
                               (end (or (next-single-property-change cur-point 'slack-section-block-props)
                                        end)))
                          (cl-case section-type
                            (code-block (progn
                                          (commit-section-block)
                                          (commit-blockquote-block)
                                          (commit-list-block)
                                          (push (cons (plist-get block-props :element-beg)
                                                      (plist-get block-props :element-end))
                                                preformatted-ranges)))
                            (blockquote (progn
                                          (commit-section-block)
                                          (commit-preformatted-block)
                                          (commit-list-block)
                                          (push (cons (plist-get block-props :element-beg)
                                                      (plist-get block-props :element-end))
                                                blockquote-ranges)
                                          ;; Skip newline
                                          (setq end (1+ end))
                                          ))
                            (list (progn
                                    ;; let's handle bulleted lists first
                                    (when (and list-ranges
                                               (or (not (string= list-style
                                                                 (plist-get block-props :style)))
                                                   (/= list-indent
                                                       (plist-get block-props :indent))))
                                      (commit-list-block))
                                    (commit-section-block)
                                    (commit-preformatted-block)
                                    (commit-blockquote-block)
                                    (push (cons (plist-get block-props :element-beg)
                                                (plist-get block-props :element-end))
                                          list-ranges)
                                    (setq list-style (plist-get block-props :style))
                                    (setq list-indent (plist-get block-props :indent)))
                                  ;; Skip newline
                                  (setq end (1+ end)))
                            (t (progn
                                 (commit-preformatted-block)
                                 (commit-blockquote-block)
                                 (commit-list-block)
                                 (dolist (e (create-elements cur-point end))
                                   (push e section-elements)))))
                          (setq cur-point end)))
                      (commit-section-block)
                      (commit-preformatted-block)
                      (commit-blockquote-block)
                      (commit-list-block))
                    (reverse elements)))
                (create-elements (start end)
                  (save-excursion
                    (save-restriction
                      (narrow-to-region start end)
                      (let* ((cur-point (point-min))
                             (elements nil))
                        (cl-labels ((create-text-element (text &optional style)
                                      (cl-remove-if #'null
                                                    (list (cons "type" "text")
                                                          (cons "text" text)
                                                          (when style
                                                            (cons "style" style))))))
                          (while (and cur-point (< cur-point (point-max)))
                            (let* ((block-props (get-text-property cur-point 'slack-block-props))
                                   (block-type (and block-props (plist-get block-props :type)))
                                   (block-text (and block-props (plist-get block-props :text)))
                                   (next-change-point (or (next-single-property-change cur-point 'slack-block-props)
                                                          (point-max)))
                                   (element (progn
                                              (cl-case block-type
                                                (bold (create-text-element block-text (list (cons "bold" t))))
                                                (italic (create-text-element block-text (list (cons "italic" t))))
                                                (strike (create-text-element block-text (list (cons "strike" t))))
                                                (code (create-text-element block-text (list (cons "code" t))))
                                                (text (create-text-element block-text))
                                                (user (list (cons "type" "user")
                                                            (cons "user_id" (plist-get block-props :user-id))))
                                                (usergroup (list (cons "type" "usergroup")
                                                                 (cons "usergroup_id" (plist-get block-props :usergroup-id))))
                                                (channel (list (cons "type" "channel")
                                                               (cons "channel_id" (plist-get block-props :channel-id))))
                                                (broadcast (list (cons "type" "broadcast")
                                                                 (cons "range" (plist-get block-props :range))))
                                                (emoji (list (cons "type" "emoji")
                                                             (cons "name" (plist-get block-props :name))))
                                                (link (append
                                                       (list (cons "type" "link")
                                                             (cons "url" (plist-get block-props :url)))
                                                       ;; if we have text, let's hide the url
                                                       (when (plist-get block-props :text)
                                                         (list (cons "text" (plist-get block-props :text))))))
                                                (t (create-text-element
                                                    (buffer-substring-no-properties cur-point
                                                                                    next-change-point)))))))
                              ;; (message "props: %s, element: %s" block-props element)
                              (when element
                                (push element elements))
                              (let* ((n (min (or next-change-point end))))
                                ;; (message "cur: %s, end: %s, %s" cur-point n (buffer-substring-no-properties cur-point n))
                                (setq cur-point n)))))

                        (reverse elements))))))
      (let ((elements (create-section-elements (point-min) (point-max))))

        (let ((blocks (list (cons "blocks" (list (list (cons "type" "rich_text")
                                                       (cons "elements" elements)))))))
          ;; (message "elements: %s, blocks: %s" elements blocks)
          ;; (let ((buf (get-buffer-create "emacs-slack blocks")))
          ;;   (with-current-buffer buf
          ;;     (delete-region (point-min) (point-max))
          ;;     (insert (json-encode-list blocks))
          ;;     (json-mode)
          ;;     (json-pretty-print-buffer))
          ;;   (switch-to-buffer-other-window buf))
          blocks)))))

(cl-defun slack-message-upload-files (team files &key on-success on-error)
  "Upload FILES to TEAM in parallel and invoke ON-SUCCESS or ON-ERROR on completion."
  (let ((files-count (length files))
        (result nil)
        (timer nil)
        (failed-p nil))
    (cl-labels
        ((on-upload (success-p &optional file-id)
                    (if success-p
                        (push file-id result)
                      (setq failed-p t))))
      (dolist (file files)
        (slack-upload-file file team #'on-upload))
      (setq timer (run-at-time t 1 #'(lambda ()
                                       (slack-log (format "Uploading files... (%s/%s)" (length result) files-count)
                                                  team)
                                       (when failed-p
                                         (funcall on-error)
                                         (cancel-timer timer))
                                       (when (<= files-count (length result))
                                         (funcall on-success files)
                                         (cancel-timer timer))))))))

(cl-defun slack-files-upload-complete (team files message-payload &key (on-success nil) (on-error nil))
  "Finalize the upload of FILES with MESSAGE-PAYLOAD on TEAM.
Call ON-SUCCESS when the API accepts the completion, or ON-ERROR on
failure."
  (cl-labels ((on-complete (&key data &allow-other-keys)
                (slack-request-handle-error
                 (data "slack-files-upload-complete"
                       #'(lambda (err)
                           (slack-log (format "Failed to files upload complete. FILES: %s, ERROR: %s"
                                              (mapcar #'(lambda (file) (oref file filename))
                                                      files)
                                              err)
                                      team)
                           (when (functionp on-error)
                             (funcall on-error))))
                 (when (functionp on-success)
                   (funcall on-success)))))
    (slack-request
     (slack-request-create
      slack-file-upload-complete-url
      team
      :type "POST"
      :data (json-encode (append (list (cons "files" (mapcar #'(lambda (file)
                                                                 (list (cons "id" (oref file id))
                                                                       (cons "title" (oref file filename))))
                                                             files)))
                                 message-payload))
      :headers (list (cons "Content-Type" "application/json;charset=utf-8"))
      :success #'on-complete))))

(provide 'slack-message-sender)
;;; slack-message-sender.el ends here
