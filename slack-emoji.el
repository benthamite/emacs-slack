;;; slack-emoji.el ---                               -*- lexical-binding: t; -*-

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
(require 'slack-request)
(require 'slack-image)
(require 'dash)

(declare-function emojify-get-emoji "emojify")
(declare-function emojify-image-dir "emojify")
(declare-function emojify-create-emojify-emojis "emojify")
(declare-function emojify-completing-read "emojify")
(declare-function emojify-download-emoji-maybe "emojify")
(declare-function slack-buffer--render-native-emoji "slack-buffer")
(defvar emojify-user-emojis)
(defvar emojify-emojis)
(defvar slack-current-buffer)
(defvar slack-teams-by-token)

(defconst slack-emoji-list "https://slack.com/api/emoji.list")

;; [How can I get the FULL list of slack emoji through API? - Stack Overflow](https://stackoverflow.com/a/39654939)
(defconst slack-emoji-master-data-url
  "https://raw.githubusercontent.com/iamcal/emoji-data/master/emoji.json")
;; this is to get each emoji image
(defconst slack-emoji-master-image-url
  "https://github.com/iamcal/emoji-data/raw/refs/heads/master/img-google-64/")

(defvar slack-emoji-master (make-hash-table :test 'equal :size 1600))

(defvar slack-emoji-jobs-to-run nil "List of lambdas to run asynchronously to download and process emojis.")
(defvar slack-emoji-paths nil "Paths to add consume on successful download of emojis.")
(defvar slack-emoji-job-runner nil "Reference to the job runner.")
(defvar slack-emoji-job-batch-size 200 "How many emojis to process at the time.")
(defvar slack-emoji-job-interval 10 "How many seconds have to pass in between batch processing.")
(defvar slack-emoji-all nil "List of all emojis found.")

(defun slack-emoji-run-job ()
  "Run first job of `slack-emoji-jobs-to-run'."
  (if-let ((job-to-run (-first-item slack-emoji-jobs-to-run)))
      (progn (setq slack-emoji-jobs-to-run (cdr slack-emoji-jobs-to-run))
             (funcall job-to-run)
             )
    (cancel-function-timers #'slack-emoji-run-job)
    (setq slack-emoji-job-runner nil)
    (setq slack-emoji-all nil)))

(defun slack-download-emoji (team after-success)
  "Download TEAM emojis and run AFTER-SUCCESS on the downloaded paths.
This runs asynchronously, splitting the emojis in batches of
`slack-emoji-job-batch-size,' every `slack-emoji-job-interval'
seconds."
  (when (and (require 'emojify nil t) (eq slack-emoji-jobs-to-run nil))
    ;; create slack image file directory if it doesn't exist, otherwise curl complains
    (ignore-errors (mkdir slack-image-file-directory 'parent-if-needed))
    (cl-labels
        ((handle-alias (name &optional depth)
           (let* ((depth (or depth 0))
                  (raw-url (plist-get slack-emoji-all name))
                  (alias (if (and raw-url (string-prefix-p "alias:" raw-url))
                             (intern (format ":%s" (cadr (split-string raw-url ":")))))))
             (if (> depth 10) nil
               (or
                (and (not raw-url) (handle-alias (intern ":slack") (1+ depth)))
                (and raw-url (string-prefix-p "alias:" raw-url)
                     (handle-alias (intern (replace-regexp-in-string "alias" "" raw-url)) (1+ depth)))
                (and alias (or (plist-get slack-emoji-all alias)
                               (let ((emoji (emojify-get-emoji (format "%s:" alias))))
                                 (if emoji
                                     (concat (emojify-image-dir) "/" (gethash "image" emoji))))))
                raw-url))))
         (push-new-emoji (emoji)
           (puthash (car emoji) t (oref team emoji-master))
           (cl-pushnew emoji emojify-user-emojis
                       :test #'string=
                       :key #'car))
         (on-success
           (&key data &allow-other-keys)
           (slack-request-handle-error
            (data "slack-download-emoji")
            (emojify-create-emojify-emojis)
            (let* ((default-emojis nil)
                   (_ (--> (slack-emoji-fetch-default-emojis-data team)
                           (oref it response)
                           (request-response-data it)
                           (--map (list
                                   (intern (concat ":" (plist-get it :short_name)))
                                   (concat
                                    slack-emoji-master-image-url
                                    (plist-get it :image)))
                                  it)
                           (-flatten it)
                           (setq default-emojis it)))
                   (emojis (setq slack-emoji-all (append (plist-get data :emoji) default-emojis))))
              (--> emojis
                   (-partition-all slack-emoji-job-batch-size it)
                   (--map
                    (let ((emojis it))
                      (lambda ()
                        (cl-loop for (name _) on emojis by #'cddr
                                 do (let* ((url (handle-alias name))
                                           (path (if (file-exists-p url) url
                                                   (slack-image-path url)))
                                           (emoji (cons (format "%s:" name)
                                                        (list (cons "name" (substring (symbol-name name) 1))
                                                              (cons "image" path)
                                                              (cons "style" "github")))))
                                      (if (file-exists-p path)
                                          (push-new-emoji emoji)
                                        (slack-url-copy-file
                                         url
                                         path
                                         team
                                         :success (lambda ()
                                                    (push-new-emoji emoji)))
                                        )
                                      (add-to-list 'slack-emoji-paths path)))))
                    it)
                   (append
                    it
                    (list
                     `(lambda ()
                        (when (functionp ',after-success) (funcall ',after-success slack-emoji-paths))
                        (setq slack-emoji-paths nil)
                        )))
                   (setq slack-emoji-jobs-to-run it)))
            (setq slack-emoji-job-runner (run-with-timer 0 slack-emoji-job-interval #'slack-emoji-run-job))
            )))
      (slack-request
       (slack-request-create
        slack-emoji-list
        team
        :success #'on-success)))))

(defun slack-select-emoji--affixation (candidates)
  "Add Unicode emoji glyphs as prefixes for CANDIDATES.
Pads each glyph to a uniform column width so names align."
  (mapcar (lambda (name)
            (let* ((glyph (gethash name slack-emoji-master))
                   (display (if (stringp glyph) glyph " "))
                   (width (string-width display))
                   (padding (make-string (max 1 (- 3 width)) ?\s)))
              (list name (concat display padding) "")))
          candidates))

(defun slack-select-emoji--candidates (team)
  "Build the emoji candidate list for TEAM.
Merges standard emoji shortcodes from `slack-emoji-master' with
TEAM-specific custom emoji shortcodes.  Candidates are bare
shortcode names (no colons) so the return value needs no
post-processing."
  (let ((candidates (hash-table-keys slack-emoji-master)))
    (maphash (lambda (k _v) (cl-pushnew k candidates :test #'equal))
             (oref team emoji-master))
    (sort candidates #'string<)))

(defun slack-select-emoji (team)
  "Select emoji for TEAM.
On Emacs 29+, use `completing-read' with glyph-prefixed display.
On older versions, use `emojify-completing-read' when available."
  (unless (< 0 (hash-table-count slack-emoji-master))
    (slack-emoji-fetch-master-data
     (car (hash-table-values slack-teams-by-token))))
  (if (or (slack-native-emoji-p)
          (not (fboundp 'emojify-completing-read))
          (not (fboundp 'emojify-download-emoji-maybe)))
      (let ((completion-extra-properties
             '(:affixation-function
               slack-select-emoji--affixation)))
        (completing-read "Emoji: "
                         (slack-select-emoji--candidates team)))
    (emojify-download-emoji-maybe)
    (cl-labels
        ((select ()
           (emojify-completing-read
            "Select Emoji: "
            (lambda (data &rest args)
              (unless (null args)
                (slack-log
                 (format "Invalid completing arguments: %s, %s"
                         data args)
                 team :level 'debug))
              (let ((emoji (car (split-string data " "))))
                (or (gethash emoji slack-emoji-master nil)
                    (gethash emoji (oref team emoji-master) nil)))))))
      (select))))

(defun slack-fetch-team-emojis (team)
  "Fetch custom emoji shortcodes for TEAM without downloading images.
Populates TEAM's `emoji-master' hash table for use with the
emoji picker on Emacs 29+ where native rendering is available."
  (cl-labels
      ((on-success (&key data &allow-other-keys)
         (slack-request-handle-error
          (data "slack-fetch-team-emojis")
          (cl-loop for (name _url) on (plist-get data :emoji)
                   by #'cddr
                   do (puthash (format "%s:" name) t
                               (oref team emoji-master))))))
    (slack-request
     (slack-request-create
      slack-emoji-list
      team
      :success #'on-success))))

(defun slack-emoji--unified-to-string (unified)
  "Convert UNIFIED hex codepoints to a Unicode string.
UNIFIED is a dash-separated string like \"1F600\" or
\"1F1FA-1F1F8\"."
  (condition-case nil
      (apply #'string
             (mapcar (lambda (hex) (string-to-number hex 16))
                     (split-string unified "-")))
    (error nil)))

(defun slack-emoji-fetch-default-emojis-data (team)
  "Synchronously fetch the default Unicode emoji master dataset for TEAM."
  (slack-request
   (slack-request-create
    slack-emoji-master-data-url
    team
    :type "GET"
    :without-auth t
    :sync t
    )))

(defun slack-emoji--store-master-data (data)
  "Populate `slack-emoji-master' from DATA.
DATA is the parsed JSON list from the iamcal/emoji-data
repository.  Each entry maps a shortcode like \":smile:\" to its
Unicode string, or to t when the character cannot be decoded."
  (cl-loop
   for emoji in data
   do (let ((short-names (plist-get emoji :short_names))
            (char (slack-emoji--unified-to-string
                   (plist-get emoji :unified))))
        (when short-names
          (cl-loop
           for name in short-names
           do (puthash (format ":%s:" name)
                       (or char t)
                       slack-emoji-master))))))

(defun slack-emoji--rerender-all-buffers ()
  "Retroactively render emoji in all existing slack buffers.
Called after `slack-emoji-master' is populated asynchronously so
that buffers created before the data arrived get their
`:shortcode:' text replaced with Unicode glyphs."
  (when (fboundp 'slack-buffer--render-native-emoji)
    (dolist (buf (buffer-list))
      (when (buffer-live-p buf)
        (with-current-buffer buf
          (when (derived-mode-p 'slack-mode 'slack-buffer-mode)
            (slack-buffer--render-native-emoji
             (point-min) (point-max))))))))

(defun slack-emoji-fetch-master-data (team)
  "Fetch the master emoji list synchronously and populate `slack-emoji-master'.
TEAM is used for the HTTP request context.  Blocks until the data
is downloaded; use `slack-emoji-fetch-master-data-async' during
startup to avoid freezing Emacs."
  (cl-labels
      ((success (&key data &allow-other-keys)
         (slack-request-handle-error
          (data "slack-emoji-fetch-master-data")
          (slack-emoji--store-master-data data))))
    (slack-request
     (slack-request-create
      slack-emoji-master-data-url
      team
      :type "GET"
      :success #'success
      :without-auth t
      :sync t))))

(defun slack-emoji-fetch-master-data-async (team)
  "Fetch the master emoji list asynchronously.
Like `slack-emoji-fetch-master-data' but does not block.  TEAM is
used for the HTTP request context."
  (cl-labels
      ((success (&key data &allow-other-keys)
         (slack-request-handle-error
          (data "slack-emoji-fetch-master-data-async")
          (slack-emoji--store-master-data data)
          (slack-emoji--rerender-all-buffers))))
    (slack-request
     (slack-request-create
      slack-emoji-master-data-url
      team
      :type "GET"
      :success #'success
      :without-auth t))))

(defvar slack-emoji--fetch-attempted nil
  "Non-nil once a lazy fetch of emoji master data has been tried.")

(defun slack-emoji--ensure-master-data ()
  "Populate `slack-emoji-master' if empty, fetching synchronously.
Uses `url-retrieve-synchronously' to avoid depending on the
`slack-request' worker (whose sync path never fires callbacks).
No-ops after the first attempt to avoid repeated failures."
  (when (and (not slack-emoji--fetch-attempted)
             (= 0 (hash-table-count slack-emoji-master)))
    (setq slack-emoji--fetch-attempted t)
    (condition-case err
        (slack-emoji--fetch-and-store-master-data)
      (error
       (message "slack-emoji: fetch failed: %s" (error-message-string err))))))

(defun slack-emoji--fetch-and-store-master-data ()
  "Download and parse the iamcal emoji-data JSON into `slack-emoji-master'."
  (let ((buf (url-retrieve-synchronously
              slack-emoji-master-data-url t nil 30)))
    (unwind-protect
        (with-current-buffer buf
          (goto-char (point-min))
          (re-search-forward "\n\n")
          (let ((data (json-parse-buffer
                       :object-type 'plist
                       :array-type 'list
                       :false-object :json-false
                       :null-object nil)))
            (slack-emoji--store-master-data data)
            (slack-emoji--rerender-all-buffers)))
      (kill-buffer buf))))

(defun slack-emoji-resolve (name)
  "Return the Unicode string for emoji NAME, or the :NAME: shortcode.
NAME is a bare emoji name without colons (e.g. \"+1\",
\"raised_hands\").  When `slack-emoji-master' is populated and
contains a Unicode mapping, return the glyph.  Otherwise return
the shortcode string.  Triggers a lazy fetch on first use."
  (slack-emoji--ensure-master-data)
  (let* ((shortcode (format ":%s:" name))
         (char (when (< 0 (hash-table-count slack-emoji-master))
                 (gethash shortcode slack-emoji-master))))
    (if (stringp char) char shortcode)))

(defun slack-emoji-known-name-p (name)
  "Return non-nil when NAME is a known emoji shortcode.
NAME is a bare name without colons (e.g. \"+1\", \"smile\").  Check
the standard emoji table, every registered team's custom emoji, and
emojify's database when it is loaded."
  (let ((shortcode (format ":%s:" name)))
    (or (gethash shortcode slack-emoji-master)
        (slack-emoji--team-emoji-p shortcode)
        (and (boundp 'emojify-emojis)
             (hash-table-p emojify-emojis)
             (gethash shortcode emojify-emojis)))))

(defun slack-emoji--team-emoji-p (shortcode)
  "Return non-nil when SHORTCODE is a custom emoji of a registered team."
  (catch 'found
    (maphash (lambda (_token team)
               (when (gethash shortcode (oref team emoji-master))
                 (throw 'found t)))
             slack-teams-by-token)))

(defun slack-insert-emoji ()
  "Insert emoji in slack buffer."
  (interactive)
  (slack-if-let* ((buffer slack-current-buffer)
                  (team (slack-buffer-team buffer)))
      (insert (slack-select-emoji team))))

(provide 'slack-emoji)
;;; slack-emoji.el ends here
