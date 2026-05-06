;;; test-buffer-rendering.el --- tests for buffer rendering pipeline  -*- lexical-binding: t; -*-

;; Tests for the buffer rendering system: lui buffer creation, timestamp
;; alignment, text property propagation, message header sizing, and
;; stars-buffer insert/replace operations.

;;; Code:

(require 'ert)
(require 'cl-lib)
(require 'eieio)
(require 'slack-team)
(require 'slack-channel)
(require 'slack-usergroup)
(require 'slack-message)
(require 'slack-user-message)
(require 'slack-message-formatter)
(require 'slack-message-faces)
(require 'slack-star)
(require 'slack-stars-buffer)
(require 'slack-buffer)
(require 'slack-room)
(require 'slack-user)

(defvar slack-channel-button-keymap nil)
(setq slack-render-image-p nil)

(defmacro slack-test-setup (&rest body)
  "Set up mock Slack objects and evaluate BODY."
  (declare (indent 0) (debug t))
  `(let* ((channel-id "C11111")
          (channel-name "TestChannel")
          (channel (make-instance 'slack-channel
                                  :id channel-id
                                  :name channel-name))
          (user-id "U11111")
          (user-name "TestUser")
          (real-name "RealName")
          (display-name "Display name")
          (user (list :name user-name :id user-id
                      :profile (list :display_name_normalized display-name
                                     :real_name_normalized real-name)))
          (usergroup-id "S88888")
          (usergroup-handle "TestUsergroup")
          (usergroup (make-instance 'slack-usergroup
                                    :id usergroup-id
                                    :handle usergroup-handle))
          (team (make-instance 'slack-team
                               :id "T99999"
                               :token "xoxb-test-token"
                               :self-id "U38383838"
                               :channels (let ((h (make-hash-table :test 'equal)))
                                           (puthash channel-id channel h)
                                           h)
                               :users (let ((h (make-hash-table :test 'equal)))
                                        (puthash (plist-get user :id) user h)
                                        h)
                               :usergroups (list usergroup))))
     ,@body))

(defun slack-test--register-team (team)
  "Register TEAM in global lookup tables for test duration."
  (puthash (oref team id) (oref team token) slack-tokens-by-id)
  (puthash (oref team token) team slack-teams-by-token))

(defun slack-test--unregister-team (team)
  "Remove TEAM from global lookup tables."
  (remhash (oref team id) slack-tokens-by-id)
  (remhash (oref team token) slack-teams-by-token))

;;; ---- Helper functions ----

(defun slack-test--make-message (ts &optional text user-id channel thread-ts)
  "Create a `slack-user-message' with timestamp TS.
TEXT is the message body, USER-ID the sender, CHANNEL the room
id, and THREAD-TS the parent thread timestamp.  All optional
arguments default to sensible test values."
  (make-instance 'slack-user-message
                 :type "message"
                 :ts ts
                 :text (or text "hello")
                 :user (or user-id "U11111")
                 :channel (or channel "C11111")
                 :thread_ts thread-ts
                 :reactions nil))

(defmacro slack-test--with-lui-buffer (&rest body)
  "Execute BODY inside a temporary lui-mode buffer with slack settings.
The buffer uses `lui-mode' with `lui-fill-type' set to nil (as
slack buffers do).  The buffer is killed after BODY completes."
  (declare (indent 0) (debug t))
  `(let ((buf (generate-new-buffer " *slack-test-lui*")))
     (unwind-protect
         (with-current-buffer buf
           (lui-mode)
           (setq-local lui-fill-type nil)
           (setq-local lui-time-stamp-only-when-changed-p nil)
           (setq-local lui-time-stamp-position 'right)
           ,@body)
       (when (buffer-live-p buf)
         (kill-buffer buf)))))

(defmacro slack-test--with-slack-buffer-mode (&rest body)
  "Execute BODY inside a temporary `slack-buffer-mode' buffer.
The buffer is killed after BODY."
  (declare (indent 0) (debug t))
  `(let ((buf (generate-new-buffer " *slack-test-buf*")))
     (unwind-protect
         (with-current-buffer buf
           (slack-buffer-mode)
           (setq-local lui-time-stamp-only-when-changed-p nil)
           ,@body)
       (when (buffer-live-p buf)
         (tracking-remove-buffer buf)
         (kill-buffer buf)))))

(defun slack-test--timestamp-column (line-number)
  "Return the column where the `lui-time-stamp' region starts on LINE-NUMBER.
The region includes padding spaces before the visible timestamp
text.  Returns nil if no timestamp is found on that line."
  (save-excursion
    (goto-char (point-min))
    (forward-line (1- line-number))
    (let ((eol (line-end-position))
          (pos (line-beginning-position)))
      (while (and (< pos eol)
                  (not (get-text-property pos 'lui-time-stamp)))
        (setq pos (1+ pos)))
      (when (get-text-property pos 'lui-time-stamp)
        (goto-char pos)
        (current-column)))))

(defun slack-test--find-ts-position (ts)
  "Return the buffer position where text property `ts' equals TS.
Returns nil if TS is not found."
  (let ((pos (point-min))
        (found nil))
    (while (and (not found) (< pos (point-max)))
      (if (equal (get-text-property pos 'ts) ts)
          (setq found pos)
        (setq pos (next-single-property-change pos 'ts nil (point-max)))))
    found))

(defun slack-test--text-properties-at-ts (ts)
  "Return the text properties at the position where `ts' equals TS.
Returns nil if TS is not found in the buffer."
  (when-let ((pos (slack-test--find-ts-position ts)))
    (text-properties-at pos)))

(defun slack-test--has-margin-timestamp ()
  "Return non-nil if the buffer has a right-margin timestamp.
Searches for a character with the `lui-time-stamp' property whose
`display' property indicates a margin placement.  The display
spec has the form ((margin right-margin) TS-STRING)."
  (let ((pos (point-min))
        (found nil))
    (while (and (not found) (< pos (point-max)))
      (if (get-text-property pos 'lui-time-stamp)
          (let ((disp (get-text-property pos 'display)))
            (if (and (consp disp)
                     (consp (car disp))
                     (eq (caar disp) 'margin))
                (setq found pos)
              (setq pos (next-single-property-change
                         pos 'lui-time-stamp nil (point-max)))))
        (setq pos (next-single-property-change
                   pos 'lui-time-stamp nil (point-max)))))
    found))

;;; ---- 1. Timestamp alignment (right position) ----

(ert-deftest slack-test-timestamp-short-line-target-column ()
  "Short first line: timestamp region starts at text end, padding reaches target.
The lui-time-stamp property covers padding + timestamp text.
For a short line, the region starts at the text end column and
padding fills to fill-column + 2."
  (slack-test--with-lui-buffer
    (let ((lui-time-stamp-position 'right)
          (lui-fill-column 70)
          (lui-time-stamp-format "[00:00]")
          (lui-time-stamp-time (encode-time 0 0 0 1 1 2025)))
      (lui-insert-with-text-properties "short")
      (let ((col (slack-test--timestamp-column 1)))
        (should col)
        (should (= col (length "short")))))))

(ert-deftest slack-test-timestamp-target-is-fill-column-plus-two ()
  "For `right' position, the visible timestamp starts at fill-column + 2.
The `lui-formatted-time-stamp' property records the timestamp
text.  The first line should end with padding + that text,
totaling to fill-column + 2 + timestamp-length."
  (slack-test--with-lui-buffer
    (let ((lui-time-stamp-position 'right)
          (lui-fill-column 50)
          (lui-time-stamp-format "[00:00]")
          (lui-time-stamp-time (encode-time 0 0 0 1 1 2025)))
      (lui-insert-with-text-properties "x")
      (goto-char (point-min))
      (end-of-line)
      (should (= (current-column) (+ 52 (length "[00:00]")))))))

(ert-deftest slack-test-timestamp-numeric-position ()
  "A numeric `lui-time-stamp-position' determines the target column.
The first line should end at position + timestamp-length."
  (slack-test--with-lui-buffer
    (let ((lui-time-stamp-position 40)
          (lui-time-stamp-format "[00:00]")
          (lui-time-stamp-time (encode-time 0 0 0 1 1 2025)))
      (lui-insert-with-text-properties "hi")
      (goto-char (point-min))
      (end-of-line)
      (should (= (current-column) (+ 40 (length "[00:00]")))))))

(ert-deftest slack-test-timestamp-long-line-fallback ()
  "Long first line exceeding target gets indent of 1.
This documents the lui.el behavior where indent falls back to 1
when the first line exceeds the target column."
  (slack-test--with-lui-buffer
    (let* ((lui-time-stamp-position 'right)
           (lui-fill-column 20)
           (lui-time-stamp-format "[00:00]")
           (lui-time-stamp-time (encode-time 0 0 0 1 1 2025))
           (long-text (make-string 30 ?x)))
      (lui-insert-with-text-properties long-text)
      (let ((col (slack-test--timestamp-column 1)))
        (should col)
        (should (= col (length long-text)))))))

(ert-deftest slack-test-timestamp-exact-boundary ()
  "First line at exactly the target column gets indent of 1."
  (slack-test--with-lui-buffer
    (let* ((lui-time-stamp-position 'right)
           (lui-fill-column 20)
           (target-col (+ 2 20))
           (lui-time-stamp-format "[00:00]")
           (lui-time-stamp-time (encode-time 0 0 0 1 1 2025))
           (text (make-string target-col ?x)))
      (lui-insert-with-text-properties text)
      (let ((col (slack-test--timestamp-column 1)))
        (should col)
        (should (= col (length text)))))))

(ert-deftest slack-test-timestamp-property-present ()
  "Inserted text has the `lui-time-stamp' text property."
  (slack-test--with-lui-buffer
    (let ((lui-time-stamp-position 'right)
          (lui-fill-column 70)
          (lui-time-stamp-format "[test]")
          (lui-time-stamp-time (encode-time 0 0 0 1 1 2025)))
      (lui-insert-with-text-properties "hello")
      (let ((pos (point-min))
            (found nil))
        (while (and (not found) (< pos (point-max)))
          (when (get-text-property pos 'lui-time-stamp)
            (setq found t))
          (setq pos (1+ pos)))
        (should found)))))

;;; ---- 1b. Timestamp alignment for long first lines ----

;;; ---- 2. Text property propagation in stars buffer ----

(ert-deftest slack-test-stars-buffer-insert-sets-ts ()
  "Stars buffer insert sets the `ts' text property."
  (slack-test-setup
    (slack-test--register-team team)
    (unwind-protect
        (slack-test--with-slack-buffer-mode
          (let* ((msg (slack-test--make-message "1710000000.000100")))
            (slack-buffer-insert
             (make-instance 'slack-stars-buffer :team-id (oref team id))
             msg)
            (should (slack-test--find-ts-position "1710000000.000100"))))
      (slack-test--unregister-team team))))

(ert-deftest slack-test-stars-buffer-insert-sets-team-id ()
  "Stars buffer insert sets the `team-id' text property."
  (slack-test-setup
    (slack-test--register-team team)
    (unwind-protect
        (slack-test--with-slack-buffer-mode
          (let* ((msg (slack-test--make-message "1710000000.000100")))
            (slack-buffer-insert
             (make-instance 'slack-stars-buffer :team-id (oref team id))
             msg)
            (let ((props (slack-test--text-properties-at-ts "1710000000.000100")))
              (should (plist-get props 'team-id))
              (should (string= (plist-get props 'team-id) "T99999")))))
      (slack-test--unregister-team team))))

(ert-deftest slack-test-stars-buffer-insert-sets-room-id ()
  "Stars buffer insert sets the `room-id' text property."
  (slack-test-setup
    (slack-test--register-team team)
    (unwind-protect
        (slack-test--with-slack-buffer-mode
          (let* ((msg (slack-test--make-message "1710000000.000100"
                                                "hello" "U11111" "C11111")))
            (slack-buffer-insert
             (make-instance 'slack-stars-buffer :team-id (oref team id))
             msg)
            (let ((props (slack-test--text-properties-at-ts "1710000000.000100")))
              (should (plist-get props 'room-id))
              (should (string= (plist-get props 'room-id) "C11111")))))
      (slack-test--unregister-team team))))

(ert-deftest slack-test-stars-buffer-insert-sets-thread-ts ()
  "Stars buffer insert sets the `thread-ts' text property."
  (slack-test-setup
    (slack-test--register-team team)
    (unwind-protect
        (slack-test--with-slack-buffer-mode
          (let* ((msg (slack-test--make-message "1710000000.000200"
                                                "reply" "U11111" "C11111"
                                                "1710000000.000100")))
            (slack-buffer-insert
             (make-instance 'slack-stars-buffer :team-id (oref team id))
             msg)
            (let ((props (slack-test--text-properties-at-ts "1710000000.000200")))
              (should (plist-get props 'thread-ts))
              (should (string= (plist-get props 'thread-ts)
                               "1710000000.000100")))))
      (slack-test--unregister-team team))))

(ert-deftest slack-test-stars-buffer-insert-thread-ts-nil-for-parent ()
  "Stars buffer insert sets `thread-ts' to nil for non-threaded messages."
  (slack-test-setup
    (slack-test--register-team team)
    (unwind-protect
        (slack-test--with-slack-buffer-mode
          (let* ((msg (slack-test--make-message "1710000000.000100"
                                                "parent msg" "U11111" "C11111")))
            (slack-buffer-insert
             (make-instance 'slack-stars-buffer :team-id (oref team id))
             msg)
            (let ((props (slack-test--text-properties-at-ts "1710000000.000100")))
              (should-not (plist-get props 'thread-ts)))))
      (slack-test--unregister-team team))))

(ert-deftest slack-test-stars-buffer-insert-all-properties-present ()
  "Stars buffer insert sets all four required text properties."
  (slack-test-setup
    (slack-test--register-team team)
    (unwind-protect
        (slack-test--with-slack-buffer-mode
          (let* ((msg (slack-test--make-message "1710000000.000200"
                                                "reply" "U11111" "C11111"
                                                "1710000000.000100")))
            (slack-buffer-insert
             (make-instance 'slack-stars-buffer :team-id (oref team id))
             msg)
            (let ((props (slack-test--text-properties-at-ts "1710000000.000200")))
              (should (plist-member props 'ts))
              (should (plist-member props 'team-id))
              (should (plist-member props 'room-id))
              (should (plist-member props 'thread-ts)))))
      (slack-test--unregister-team team))))

;;; ---- 3. Message header length ----

(ert-deftest slack-test-header-basic-shorter-than-fill-column ()
  "A basic header with just a username is shorter than fill-column."
  (slack-test-setup
    (let ((msg (slack-test--make-message "1710000000.000100"))
          (slack-render-image-p nil))
      (let ((header (slack-message-header msg team)))
        (should (< (length header) fill-column))))))

(ert-deftest slack-test-header-has-header-face ()
  "The header string has the `slack-message-output-header' face."
  (slack-test-setup
    (let ((msg (slack-test--make-message "1710000000.000100"))
          (slack-render-image-p nil))
      (let ((header (slack-message-header msg team)))
        (should (equal (get-text-property 0 'face header)
                       'slack-message-output-header))))))

(ert-deftest slack-test-header-with-star-adds-length ()
  "A starred message header includes \" :star:\" suffix."
  (slack-test-setup
    (let ((msg (slack-test--make-message "1710000000.000100"))
          (slack-render-image-p nil))
      (oset msg is-starred t)
      (let ((header (slack-message-header msg team)))
        (should (string-match-p ":star:" header))))))

(ert-deftest slack-test-header-with-edit-longer-than-basic ()
  "An edited message header is longer than a basic one."
  (slack-test-setup
    (let* ((msg-plain (slack-test--make-message "1710000000.000100"))
           (msg-edited (slack-test--make-message "1710000000.000200"))
           (slack-render-image-p nil))
      (oset msg-edited edited (make-instance 'slack-message-edited
                                             :user "U11111"
                                             :ts "1710000001"))
      (let ((plain-header (slack-message-header msg-plain team))
            (edited-header (slack-message-header msg-edited team)))
        (should (> (length edited-header) (length plain-header)))))))

(ert-deftest slack-test-header-length-predictable ()
  "Header length for a known username can be measured exactly."
  (slack-test-setup
    (let ((msg (slack-test--make-message "1710000000.000100"))
          (slack-render-image-p nil))
      (let ((header (slack-message-header msg team)))
        (should (= (length header) (length "Display name")))))))

;;; ---- 4. Stars buffer insert/replace ----

(ert-deftest slack-test-stars-buffer-insert-message-text ()
  "Stars buffer insert puts the message body text into the buffer."
  (slack-test-setup
    (slack-test--register-team team)
    (unwind-protect
        (slack-test--with-slack-buffer-mode
          (let* ((msg (slack-test--make-message "1710000000.000100"
                                                "the body text")))
            (slack-buffer-insert
             (make-instance 'slack-stars-buffer :team-id (oref team id))
             msg)
            (goto-char (point-min))
            (should (search-forward "the body text" nil t))))
      (slack-test--unregister-team team))))

(ert-deftest slack-test-stars-buffer-insert-separator-follows ()
  "Stars buffer insert appends a separator after each message.
The separator is inserted by `(lui-insert \"\" t)' which
produces a newline with `not-tracked-p'."
  (slack-test-setup
    (slack-test--register-team team)
    (unwind-protect
        (slack-test--with-slack-buffer-mode
          (let* ((msg (slack-test--make-message "1710000000.000100")))
            (slack-buffer-insert
             (make-instance 'slack-stars-buffer :team-id (oref team id))
             msg)
            (goto-char (point-min))
            (let ((ts-end (next-single-property-change (point) 'ts)))
              (when ts-end
                (let ((after-props (text-properties-at ts-end)))
                  (should (plist-get after-props 'not-tracked-p)))))))
      (slack-test--unregister-team team))))

(ert-deftest slack-test-stars-buffer-insert-sets-time-stamp-format ()
  "Stars buffer insert uses the slack date-time timestamp format."
  (slack-test-setup
    (slack-test--register-team team)
    (unwind-protect
        (slack-test--with-slack-buffer-mode
          (let* ((msg (slack-test--make-message "1710000000.000100")))
            (slack-buffer-insert
             (make-instance 'slack-stars-buffer :team-id (oref team id))
             msg)
            (let ((pos (point-min))
                  (found-ts nil))
              (while (and (not found-ts) (< pos (point-max)))
                (when (get-text-property pos 'lui-time-stamp)
                  (let ((formatted (get-text-property
                                    pos 'lui-formatted-time-stamp)))
                    (when formatted
                      (setq found-ts formatted))))
                (setq pos (1+ pos)))
              (should found-ts)
              (should (string-match-p "^\\[" found-ts)))))
      (slack-test--unregister-team team))))

(ert-deftest slack-test-stars-buffer-insert-two-messages-ordered ()
  "Inserting two messages preserves insertion order."
  (slack-test-setup
    (slack-test--register-team team)
    (unwind-protect
        (slack-test--with-slack-buffer-mode
          (let* ((msg1 (slack-test--make-message "1710000000.000100" "first"))
                 (msg2 (slack-test--make-message "1710000000.000200" "second"))
                 (stars-buf (make-instance 'slack-stars-buffer
                                           :team-id (oref team id))))
            (slack-buffer-insert stars-buf msg1)
            (slack-buffer-insert stars-buf msg2)
            (goto-char (point-min))
            (let ((pos1 (search-forward "first" nil t))
                  (pos2 (search-forward "second" nil t)))
              (should pos1)
              (should pos2)
              (should (< pos1 pos2)))))
      (slack-test--unregister-team team))))

(ert-deftest slack-test-create-stars-buffer-caches-team ()
  "Stars buffer keeps its team when global team lookup cannot resolve it."
  (slack-test-setup
    (oset team name "TestTeam")
    (oset team id nil)
    (let ((stars-buf (slack-create-stars-buffer team)))
      (should (eq (slack-buffer-team stars-buf) team))
      (should (string= (slack-buffer-name stars-buf)
                       "*slack: TestTeam : Saved items*")))))

;;; ---- 5. Lui-insert basic behavior ----

(ert-deftest slack-test-lui-insert-adds-text ()
  "Lui-insert places the given text into the buffer."
  (slack-test--with-lui-buffer
    (let ((lui-time-stamp-position nil))
      (lui-insert "hello")
      (goto-char (point-min))
      (should (search-forward "hello" nil t)))))

(ert-deftest slack-test-lui-insert-with-timestamp-contains-text ()
  "Lui-insert with right timestamp still contains the original text."
  (slack-test--with-lui-buffer
    (let ((lui-time-stamp-position 'right)
          (lui-fill-column 70)
          (lui-time-stamp-format "[00:00]")
          (lui-time-stamp-time (encode-time 0 0 0 1 1 2025)))
      (lui-insert "hello")
      (goto-char (point-min))
      (should (search-forward "hello" nil t)))))

(ert-deftest slack-test-lui-insert-text-properties-propagated ()
  "Custom text properties passed to `lui-insert-with-text-properties' survive."
  (slack-test--with-lui-buffer
    (let ((lui-time-stamp-position nil))
      (lui-insert-with-text-properties "test" 'custom-prop 42)
      (goto-char (point-min))
      (should (equal 42 (get-text-property (point) 'custom-prop))))))

(ert-deftest slack-test-lui-insert-not-tracked-property ()
  "Passing `not-tracked-p' to `lui-insert' sets the property."
  (slack-test--with-lui-buffer
    (let ((lui-time-stamp-position nil))
      (lui-insert "test" t)
      (goto-char (point-min))
      (should (get-text-property (point) 'not-tracked-p)))))

(ert-deftest slack-test-lui-insert-raw-text-property ()
  "Lui sets `lui-raw-text' to the original inserted string."
  (slack-test--with-lui-buffer
    (let ((lui-time-stamp-position nil))
      (lui-insert-with-text-properties "raw content")
      (goto-char (point-min))
      (should (equal "raw content"
                     (get-text-property (point) 'lui-raw-text))))))

(ert-deftest slack-test-lui-insert-message-id-increments ()
  "Each `lui-insert' call gets an incrementing `lui-message-id'."
  (slack-test--with-lui-buffer
    (let ((lui-time-stamp-position nil))
      (lui-insert "first")
      (lui-insert "second")
      (goto-char (point-min))
      (let ((id1 (get-text-property (point) 'lui-message-id)))
        (goto-char (next-single-property-change (point) 'lui-message-id))
        (let ((id2 (get-text-property (point) 'lui-message-id)))
          (should id1)
          (should id2)
          (should (< id1 id2)))))))

;;; ---- 6. Slack-mode timestamp configuration ----

(ert-deftest slack-test-slack-mode-disables-fill ()
  "Slack mode sets `lui-fill-type' to nil."
  (let ((buf (generate-new-buffer " *slack-test-mode*")))
    (unwind-protect
        (with-current-buffer buf
          (slack-mode)
          (should (null lui-fill-type)))
      (when (buffer-live-p buf)
        (kill-buffer buf)))))

(ert-deftest slack-test-slack-buffer-mode-timestamp-position ()
  "Slack buffer mode inherits the default `lui-time-stamp-position'."
  (let ((buf (generate-new-buffer " *slack-test-mode2*")))
    (unwind-protect
        (with-current-buffer buf
          (slack-buffer-mode)
          (should (eq lui-time-stamp-position 'right)))
      (when (buffer-live-p buf)
        (kill-buffer buf)))))

;;; ---- 7. Message formatting integration ----

(ert-deftest slack-test-message-to-string-contains-header ()
  "The formatted message string contains the header."
  (slack-test-setup
    (let ((msg (slack-test--make-message "1710000000.000100" "body text"))
          (slack-render-image-p nil))
      (let ((str (slack-message-to-string msg team)))
        (should (string-match-p "Display name" str))
        (should (string-match-p "body text" str))))))

(ert-deftest slack-test-message-to-string-has-header-property ()
  "The formatted string marks the header with `slack-message-header'."
  (slack-test-setup
    (let ((msg (slack-test--make-message "1710000000.000100"))
          (slack-render-image-p nil))
      (let ((str (slack-message-to-string msg team)))
        (should (text-property-any
                 0 (length str) 'slack-message-header t str))))))

(ert-deftest slack-test-message-to-string-has-permalink ()
  "The formatted string has a `permalink' text property."
  (slack-test-setup
    (let ((msg (slack-test--make-message "1710000000.000100"))
          (slack-render-image-p nil))
      (oset msg permalink
            "https://example.slack.com/archives/C11111/p1710000000000100")
      (let ((str (slack-message-to-string msg team)))
        (should (get-text-property 0 'permalink str))))))

(ert-deftest slack-test-format-message-filters-blank ()
  "`slack-format-message' drops nil and empty strings."
  (should (equal "a\nb" (slack-format-message "a" nil "" "b")))
  (should (equal "only" (slack-format-message "only" nil "" nil))))

;;; ---- 8. Timestamp alignment stress tests ----

(ert-deftest slack-test-timestamp-multiline-only-first-line ()
  "Only the first line of a multi-line message gets the timestamp."
  (slack-test--with-lui-buffer
    (let ((lui-time-stamp-position 'right)
          (lui-fill-column 70)
          (lui-time-stamp-format "[00:00]")
          (lui-time-stamp-time (encode-time 0 0 0 1 1 2025)))
      (lui-insert-with-text-properties "line one\nline two\nline three")
      (should (slack-test--timestamp-column 1))
      (should-not (slack-test--timestamp-column 2))
      (should-not (slack-test--timestamp-column 3)))))

(ert-deftest slack-test-timestamp-empty-string-gets-timestamp ()
  "Even an empty string insertion gets a timestamp."
  (slack-test--with-lui-buffer
    (let ((lui-time-stamp-position 'right)
          (lui-fill-column 70)
          (lui-time-stamp-format "[00:00]")
          (lui-time-stamp-time (encode-time 0 0 0 1 1 2025)))
      (lui-insert-with-text-properties "")
      (should (slack-test--timestamp-column 1)))))

(ert-deftest slack-test-timestamp-nil-position-no-timestamp ()
  "With `lui-time-stamp-position' nil, no timestamp is inserted."
  (slack-test--with-lui-buffer
    (let ((lui-time-stamp-position nil))
      (lui-insert-with-text-properties "no stamp")
      (let ((pos (point-min))
            (found nil))
        (while (and (not found) (< pos (point-max)))
          (when (get-text-property pos 'lui-time-stamp)
            (setq found t))
          (setq pos (1+ pos)))
        (should-not found)))))

(ert-deftest slack-test-timestamp-left-position-prepends ()
  "With `left' position, timestamp is prepended to the first line."
  (slack-test--with-lui-buffer
    (let ((lui-time-stamp-position 'left)
          (lui-time-stamp-format "[00:00]")
          (lui-time-stamp-time (encode-time 0 0 0 1 1 2025)))
      (lui-insert-with-text-properties "test left")
      (goto-char (point-min))
      (should (get-text-property (point) 'lui-time-stamp)))))

;;; ---- 9. Stars buffer replace ----

(ert-deftest slack-test-stars-buffer-replace-updates-text ()
  "Replacing a message in stars buffer changes its body text."
  (slack-test-setup
    (slack-test--register-team team)
    (unwind-protect
        (slack-test--with-slack-buffer-mode
          (let* ((msg (slack-test--make-message "1710000000.000100"
                                                "original text"))
                 (stars-buf (make-instance 'slack-stars-buffer
                                           :team-id (oref team id)))
                 (star-item (make-instance 'slack-star-item
                                           :item-id "C11111"
                                           :item-type "message"
                                           :date-created 0
                                           :date-due 0
                                           :date-completed 0
                                           :date-updated 0
                                           :is-archived nil
                                           :date-snoozed-until 0
                                           :ts "1710000000.000100"
                                           :state "active"))
                 (star (make-instance 'slack-star
                                      :items (list star-item))))
            (oset team star star)
            (slack-room-set-messages channel (list msg) team)
            (slack-buffer-insert stars-buf msg)
            (goto-char (point-min))
            (should (search-forward "original text" nil t))
            (oset msg text "updated text")
            (let ((inhibit-read-only t))
              (slack-buffer--replace stars-buf "1710000000.000100"))
            (goto-char (point-min))
            (should (search-forward "updated text" nil t))
            (should-not (search-forward "original text" nil t))))
      (slack-test--unregister-team team))))

(ert-deftest slack-test-stars-buffer-replace-preserves-ts-property ()
  "After replacing a message, the `ts' text property is preserved."
  (slack-test-setup
    (slack-test--register-team team)
    (unwind-protect
        (slack-test--with-slack-buffer-mode
          (let* ((msg (slack-test--make-message "1710000000.000100"
                                                "original"))
                 (stars-buf (make-instance 'slack-stars-buffer
                                           :team-id (oref team id)))
                 (star-item (make-instance 'slack-star-item
                                           :item-id "C11111"
                                           :item-type "message"
                                           :date-created 0
                                           :date-due 0
                                           :date-completed 0
                                           :date-updated 0
                                           :is-archived nil
                                           :date-snoozed-until 0
                                           :ts "1710000000.000100"
                                           :state "active"))
                 (star (make-instance 'slack-star
                                      :items (list star-item))))
            (oset team star star)
            (slack-room-set-messages channel (list msg) team)
            (slack-buffer-insert stars-buf msg)
            (oset msg text "replaced")
            (let ((inhibit-read-only t))
              (slack-buffer--replace stars-buf "1710000000.000100"))
            (should (slack-test--find-ts-position "1710000000.000100"))))
      (slack-test--unregister-team team))))

;;; ---- 10. Emoji rendering ----

(require 'slack-emoji)
(require 'slack-message-buffer)
(require 'slack-all-threads-buffer)

(ert-deftest slack-test-render-native-emoji-sets-display ()
  "Render shortcodes as Unicode glyphs via display property."
  (let ((slack-emoji-master (make-hash-table :test 'equal)))
    (puthash ":smile:" "\U0001F604" slack-emoji-master)
    (puthash ":heart:" "\u2764" slack-emoji-master)
    (with-temp-buffer
      (insert "hello :smile: and :heart: world")
      (slack-buffer--render-native-emoji (point-min) (point-max))
      (goto-char (point-min))
      (search-forward ":smile:")
      (should (equal "\U0001F604"
                     (get-text-property (match-beginning 0) 'display)))
      (search-forward ":heart:")
      (should (equal "\u2764"
                     (get-text-property (match-beginning 0) 'display))))))

(ert-deftest slack-test-render-native-emoji-skips-unknown ()
  "Unknown shortcodes are left untouched."
  (let ((slack-emoji-master (make-hash-table :test 'equal)))
    (puthash ":smile:" "\U0001F604" slack-emoji-master)
    (with-temp-buffer
      (insert ":unknown_emoji: text")
      (slack-buffer--render-native-emoji (point-min) (point-max))
      (goto-char (point-min))
      (should-not (get-text-property (point) 'display)))))

(ert-deftest slack-test-render-native-emoji-empty-master ()
  "No-op when `slack-emoji-master' is empty."
  (let ((slack-emoji-master (make-hash-table :test 'equal)))
    (with-temp-buffer
      (insert ":smile: text")
      (slack-buffer--render-native-emoji (point-min) (point-max))
      (goto-char (point-min))
      (should-not (get-text-property (point) 'display)))))

(defmacro slack-test-with-mode (mode &rest body)
  "Activate MODE in a temp buffer and evaluate BODY."
  (declare (indent 1) (debug t))
  `(let ((buf (generate-new-buffer " *slack-test-emoji*"))
         (slack-buffer-emojify t))
     (unwind-protect
         (with-current-buffer buf ,mode ,@body)
       (when (buffer-live-p buf)
         (let ((kill-buffer-query-functions nil))
           (kill-buffer buf))))))

(ert-deftest slack-test-buffer-mode-has-emoji-hook ()
  "`slack-buffer-mode' installs the emoji pre-output hook."
  (slack-test-with-mode (slack-buffer-mode)
    (should (memq #'slack-buffer--pre-output-render-emoji
                  lui-pre-output-hook))))

(ert-deftest slack-test-message-buffer-mode-has-emoji-hook ()
  "`slack-message-buffer-mode' inherits the emoji hook."
  (slack-test-with-mode (slack-message-buffer-mode)
    (should (memq #'slack-buffer--pre-output-render-emoji
                  lui-pre-output-hook))))

(ert-deftest slack-test-all-threads-mode-has-emoji-hook ()
  "`slack-all-threads-buffer-mode' inherits the emoji hook."
  (slack-test-with-mode (slack-all-threads-buffer-mode)
    (should (memq #'slack-buffer--pre-output-render-emoji
                  lui-pre-output-hook))))

(ert-deftest slack-test-message-buffer-mode-derives-from-slack-buffer-mode ()
  "`slack-message-buffer-mode' derives from `slack-buffer-mode'."
  (slack-test-with-mode (slack-message-buffer-mode)
    (should (derived-mode-p 'slack-buffer-mode))))

(ert-deftest slack-test-message-buffer-mode-has-input-function ()
  "`slack-message-buffer-mode' sets the lui input function."
  (slack-test-with-mode (slack-message-buffer-mode)
    (should (eq lui-input-function 'slack-message--send))))

(ert-deftest slack-test-rerender-covers-slack-mode-buffers ()
  "Async rerender reaches buffers in `slack-mode'."
  (let ((slack-emoji-master (make-hash-table :test 'equal))
        (buf (generate-new-buffer " *slack-test-rerender*")))
    (unwind-protect
        (progn
          (with-current-buffer buf
            (slack-mode)
            (let ((inhibit-read-only t))
              (insert ":smile: text")))
          (puthash ":smile:" "\U0001F604" slack-emoji-master)
          (slack-emoji--rerender-all-buffers)
          (with-current-buffer buf
            (goto-char (point-min))
            (should (equal "\U0001F604"
                           (get-text-property (point) 'display)))))
      (when (buffer-live-p buf)
        (let ((kill-buffer-query-functions nil))
          (kill-buffer buf))))))

(ert-deftest slack-test-rerender-covers-slack-buffer-mode-buffers ()
  "Async rerender reaches buffers in `slack-buffer-mode'."
  (let ((slack-emoji-master (make-hash-table :test 'equal))
        (buf (generate-new-buffer " *slack-test-rerender2*")))
    (unwind-protect
        (progn
          (with-current-buffer buf
            (slack-buffer-mode)
            (let ((inhibit-read-only t))
              (insert ":heart: text")))
          (puthash ":heart:" "\u2764" slack-emoji-master)
          (slack-emoji--rerender-all-buffers)
          (with-current-buffer buf
            (goto-char (point-min))
            (should (equal "\u2764"
                           (get-text-property (point) 'display)))))
      (when (buffer-live-p buf)
        (let ((kill-buffer-query-functions nil))
          (kill-buffer buf))))))

;;; ---- 11. Reaction emoji rendering ----

(require 'slack-reaction)
(require 'slack-activity-feed-buffer)

(ert-deftest slack-test-emoji-resolve-returns-unicode ()
  "`slack-emoji-resolve' returns Unicode when master table is populated."
  (let ((slack-emoji-master (make-hash-table :test 'equal)))
    (puthash ":+1:" "\U0001F44D" slack-emoji-master)
    (should (equal "\U0001F44D" (slack-emoji-resolve "+1")))))

(ert-deftest slack-test-emoji-resolve-falls-back-to-shortcode ()
  "`slack-emoji-resolve' returns :name: when master table is empty."
  (let ((slack-emoji-master (make-hash-table :test 'equal))
        (slack-emoji--fetch-attempted t))
    (should (equal ":wave:" (slack-emoji-resolve "wave")))))

(ert-deftest slack-test-emoji-resolve-falls-back-for-unknown ()
  "`slack-emoji-resolve' returns :name: for unknown shortcodes."
  (let ((slack-emoji-master (make-hash-table :test 'equal)))
    (puthash ":+1:" "\U0001F44D" slack-emoji-master)
    (should (equal ":custom_emoji:" (slack-emoji-resolve "custom_emoji")))))

(ert-deftest slack-test-emoji-resolve-skips-non-string-values ()
  "`slack-emoji-resolve' returns shortcode when value is t (custom image emoji)."
  (let ((slack-emoji-master (make-hash-table :test 'equal))
        (slack-emoji--fetch-attempted t))
    (puthash ":company_logo:" t slack-emoji-master)
    (should (equal ":company_logo:" (slack-emoji-resolve "company_logo")))))

(ert-deftest slack-test-reaction-to-string-has-unicode ()
  "`slack-reaction-to-string' renders the glyph, not the shortcode."
  (let ((slack-emoji-master (make-hash-table :test 'equal)))
    (puthash ":+1:" "\U0001F44D" slack-emoji-master)
    (slack-test-setup
      (let* ((r (make-instance 'slack-reaction
                               :name "+1" :count 3 :users nil))
             (str (slack-reaction-to-string r team)))
        (should (string-match-p "\U0001F44D" str))
        (should-not (string-match-p (regexp-quote ":+1:") str))
        (should (string-match-p "3" str))))))

(ert-deftest slack-test-reaction-to-string-fallback-when-empty ()
  "`slack-reaction-to-string' uses shortcode when master is empty."
  (let ((slack-emoji-master (make-hash-table :test 'equal))
        (slack-emoji--fetch-attempted t))
    (slack-test-setup
      (let* ((r (make-instance 'slack-reaction
                               :name "+1" :count 1 :users nil))
             (str (slack-reaction-to-string r team)))
        (should (string-match-p (regexp-quote ":+1:") str))))))

(ert-deftest slack-test-reaction-to-string-has-reaction-property ()
  "`slack-reaction-to-string' sets the `reaction' text property."
  (let ((slack-emoji-master (make-hash-table :test 'equal)))
    (puthash ":+1:" "\U0001F44D" slack-emoji-master)
    (slack-test-setup
      (let* ((r (make-instance 'slack-reaction
                               :name "+1" :count 1 :users nil))
             (str (slack-reaction-to-string r team)))
        (should (eq r (get-text-property 0 'reaction str)))))))

(ert-deftest slack-test-activity-reaction-has-unicode ()
  "`slack-activity-reaction-to-string' renders the glyph."
  (let ((slack-emoji-master (make-hash-table :test 'equal)))
    (puthash ":raised_hands:" "\U0001F64C" slack-emoji-master)
    (slack-test-setup
      (let* ((r (make-instance 'activity-reaction
                               :user user-id :name "raised_hands"))
             (str (slack-activity-reaction-to-string r team)))
        (should (string-match-p "\U0001F64C" str))
        (should-not (string-match-p ":raised_hands:" str))))))

;;; ---- 12. Integration: reactions through full lui-insert pipeline ----

(ert-deftest slack-test-reaction-unicode-survives-lui-insert ()
  "Reaction Unicode glyph survives the full lui-insert pipeline."
  (let ((slack-emoji-master (make-hash-table :test 'equal)))
    (puthash ":+1:" "\U0001F44D" slack-emoji-master)
    (slack-test-setup
      (slack-test--register-team team)
      (unwind-protect
          (slack-test--with-slack-buffer-mode
            (let* ((msg (slack-test--make-message "1710000000.000100"
                                                  "test body"))
                   (r (make-instance 'slack-reaction
                                     :name "+1" :count 1 :users nil)))
              (oset msg reactions (list r))
              (slack-buffer-insert
               (make-instance 'slack-stars-buffer :team-id (oref team id))
               msg)
              (goto-char (point-min))
              (should (search-forward "\U0001F44D" nil t))))
        (slack-test--unregister-team team)))))

(ert-deftest slack-test-reaction-unicode-survives-deferred-hooks ()
  "Reaction Unicode glyph survives `slack-buffer-with-deferred-hooks'."
  (let ((slack-emoji-master (make-hash-table :test 'equal)))
    (puthash ":heart:" "\u2764" slack-emoji-master)
    (slack-test-setup
      (slack-test--register-team team)
      (unwind-protect
          (slack-test--with-slack-buffer-mode
            (let* ((msg (slack-test--make-message "1710000000.000100"
                                                  "test body"))
                   (r (make-instance 'slack-reaction
                                     :name "heart" :count 2 :users nil)))
              (oset msg reactions (list r))
              (slack-buffer-with-deferred-hooks
                (slack-buffer-insert
                 (make-instance 'slack-stars-buffer :team-id (oref team id))
                 msg))
              (goto-char (point-min))
              (should (search-forward "\u2764" nil t))))
        (slack-test--unregister-team team)))))

(ert-deftest slack-test-reaction-shortcode-absent-when-resolved ()
  "No raw shortcode remains in buffer when emoji master is populated."
  (let ((slack-emoji-master (make-hash-table :test 'equal)))
    (puthash ":smile:" "\U0001F604" slack-emoji-master)
    (slack-test-setup
      (slack-test--register-team team)
      (unwind-protect
          (slack-test--with-slack-buffer-mode
            (let* ((msg (slack-test--make-message "1710000000.000100"
                                                  "test body"))
                   (r (make-instance 'slack-reaction
                                     :name "smile" :count 1 :users nil)))
              (oset msg reactions (list r))
              (slack-buffer-insert
               (make-instance 'slack-stars-buffer :team-id (oref team id))
               msg)
              (goto-char (point-min))
              (should-not (search-forward ":smile:" nil t))))
        (slack-test--unregister-team team)))))

(ert-deftest slack-test-multiple-reactions-all-render ()
  "Multiple reactions all render as Unicode glyphs."
  (let ((slack-emoji-master (make-hash-table :test 'equal)))
    (puthash ":+1:" "\U0001F44D" slack-emoji-master)
    (puthash ":heart:" "\u2764" slack-emoji-master)
    (slack-test-setup
      (slack-test--register-team team)
      (unwind-protect
          (slack-test--with-slack-buffer-mode
            (let* ((msg (slack-test--make-message "1710000000.000100"
                                                  "test body"))
                   (r1 (make-instance 'slack-reaction
                                      :name "+1" :count 1 :users nil))
                   (r2 (make-instance 'slack-reaction
                                      :name "heart" :count 3 :users nil)))
              (oset msg reactions (list r1 r2))
              (slack-buffer-insert
               (make-instance 'slack-stars-buffer :team-id (oref team id))
               msg)
              (goto-char (point-min))
              (should (search-forward "\U0001F44D" nil t))
              (goto-char (point-min))
              (should (search-forward "\u2764" nil t))))
        (slack-test--unregister-team team)))))

;;; ---- Runner ----

(provide 'test-buffer-rendering)
;;; test-buffer-rendering.el ends here
