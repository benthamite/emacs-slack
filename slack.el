;;; slack.el --- slack client              -*- lexical-binding: t; -*-

;; Copyright (C) 2015  yuya.minami

;; URL: https://github.com/emacs-slack/emacs-slack
;; Author: yuya.minami <yuya.minami@yuyaminami-no-MacBook-Pro.local>
;; Maintainers:
;; - Name: Andrea
;;   Email: andrea-dev@hotmail.com
;; Keywords: tools
;; Version: 0.0.3
;; Package-Requires: ((websocket "1.12") (request "0.3.2") (circe "2.11") (alert "1.2") (emojify "1.2.1") (emacs "25.1") (dash "2.19.1") (s "1.13.1"))
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

;; Slack client in Emacs.

;;

;;; Code:
(defun slack--live-websocket-processes ()
  "Return websocket processes currently referenced by a team's `ws.conn'."
  (let (procs)
    (when (boundp 'slack-teams-by-token)
      (maphash
       (lambda (_k team)
         (when (slot-boundp team 'ws)
           (let* ((ws (oref team ws))
                  (conn (and ws (oref ws conn))))
             (when (and conn (fboundp 'websocket-conn))
               (let ((proc (ignore-errors (websocket-conn conn))))
                 (when proc (push proc procs)))))))
       slack-teams-by-token))
    procs))

(defun slack--cleanup-stale-websockets ()
  "Delete orphaned Slack websocket processes, preserving live ones.
Called on reload of `slack' to avoid the connection storm that
otherwise accumulates because each reload orphans the previous
websocket instead of closing it.  Any slack websocket process not
currently referenced by some team's `ws.conn' slot is considered
a zombie from a prior load and is deleted.  The live websocket
(if any) is preserved so that reloading while connected does not
force a reconnect."
  (let ((alive (slack--live-websocket-processes)))
    (dolist (p (process-list))
      (when (and (not (memq p alive))
                 (memq (process-status p) '(open run connect))
                 (string-match-p "wss-primary\\.slack\\.com" (process-name p)))
        (ignore-errors (delete-process p))))))

(when (featurep 'slack)
  (slack--cleanup-stale-websockets))

(require 'cl-lib)
(require 'subr-x)
(require 'color)
(require 'dash)
(require 's)

(declare-function browse-url-default-browser "browse-url")

(require 'slack-util)
(require 'slack-team)
(require 'slack-channel)
(require 'slack-im)
(require 'slack-file)
(require 'slack-message-faces)
(require 'slack-message-notification)
(require 'slack-message-sender)
(require 'slack-message-editor)
(require 'slack-message-reaction)
(require 'slack-user)
(require 'slack-user-message)
(require 'slack-bot-message)
(require 'slack-search)
(require 'slack-reminder)
(require 'slack-thread)
(require 'slack-attachment)
(require 'slack-emoji)
(require 'slack-star)

(require 'slack-buffer)
(require 'slack-room-info-buffer)
(require 'slack-message-buffer)
(require 'slack-message-edit-buffer)
(require 'slack-message-share-buffer)
(require 'slack-thread-message-buffer)
(require 'slack-all-threads-buffer)
(require 'slack-room-message-compose-buffer)
(require 'slack-pinned-items-buffer)
(require 'slack-user-profile-buffer)
(require 'slack-file-list-buffer)
(require 'slack-file-info-buffer)
(require 'slack-thread-message-compose-buffer)
(require 'slack-search-result-buffer)
(require 'slack-activity-feed-buffer)
(require 'slack-scheduled-messages-buffer)
(require 'slack-stars-buffer)
(require 'slack-dialog-buffer)
(require 'slack-dialog-edit-element-buffer)

(require 'slack-websocket)
(require 'slack-request)
(require 'slack-usergroup)
(require 'slack-modeline)
(require 'slack-create-message)

(require 'slack-company)
(require 'slack-menu)

(defgroup slack nil
  "Emacs Slack Client"
  :prefix "slack-"
  :group 'tools)

(defcustom slack-buffer-function #'switch-to-buffer-other-window
  "Function to print buffer."
  :type 'function
  :group 'slack)

(defvar slack-use-register-team-string
  "use `slack-register-team' instead.")

(defcustom slack-client-id nil
  "Client ID provided by Slack."
  :type 'string
  :group 'slack)
(make-obsolete-variable
 'slack-client-id slack-use-register-team-string
 "0.0.2")

(defcustom slack-client-secret nil
  "Client Secret Provided by Slack."
  :type 'string
  :group 'slack)
(make-obsolete-variable
 'slack-client-secret slack-use-register-team-string
 "0.0.2")

(defcustom slack-token nil
  "Slack token provided by Slack.
set this to save request to Slack if already have."
  :type 'string
  :group 'slack)
(make-obsolete-variable
 'slack-token slack-use-register-team-string
 "0.0.2")

(defcustom slack-room-subscription '()
  "Group or Channel list to subscribe notification."
  :type '(repeat string)
  :group 'slack)
(make-obsolete-variable
 'slack-room-subscription slack-use-register-team-string
 "0.0.2")

(defcustom slack-typing-visibility 'frame
  "When to display typing indicator.
When `frame', typing slack buffer is in the current frame.
When `buffer', typing slack buffer is the current buffer.
When `never', never display typing indicator."
  :type '(choice (const frame)
                 (const buffer)
                 (const never))
  :group 'slack)

(defcustom slack-display-team-name t
  "If nil, only display channel, im, group name."
  :type 'boolean
  :group 'slack)

(defcustom slack-completing-read-function #'completing-read
  "Require same argument with `completing-read'."
  :type 'function
  :group 'slack)

(defcustom slack-enable-wysiwyg nil
  "If t, enable live markup in message compose or edit buffer."
  :type 'boolean
  :group 'slack)

(defcustom slack-before-quit-hook nil
  "Hooks to run before quitting slack."
  :type '(repeat function)
  :group 'slack)

(defcustom slack-refresh-token-instructions "
Using Chrome, open and sign into the slack customization page, e.g. https://my.slack.com/customize
Right click anywhere on the page and choose \"inspect\" from the context menu. This will open the Chrome developer tools.
Find the console (it's one of the tabs in the developer tools window)
At the prompt (\"> \") type the following: window.prompt(\"your api token is: \", TS.boot_data.api_token)
Copy the displayed token elsewhere.
If your token starts with xoxc then keep following the other steps below, otherwise you are done and can close the window.
You can also set the enterprise token by running: window.prompt(\"your enterprise api token is: \", TS.boot_data.enterprise_api_token)
--- YOU ARE HERE ---
Now switch to the Applications tab in the Chrome developer tools (or Storage tab in Firefox developer tools).
Expand Cookies in the left-hand sidebar.
Click the cookie entry named d and copy its value. Note, use the default encoded version, so don't click the Show URL decoded checkbox.
ALSO take the d-s and lc cookie and store as 'xoxd-xxxxxxxx; d-s=xxxxxx; lc=xxxxx'.
Now you're done and can close the window.

For further explanation, see the documentation for the emojme project: (github.com/jackellenberger/emojme)

Note that it is only possible to obtain the cookie manually, not through client-side javascript, due to it being set as HttpOnly and Secure. See OWASP HttpOnly.

Evaluate these to update your current team:

(oset slack-current-team token \"\")
(oset slack-current-team cookie \"\")
;; optionally if you are using enterprise Slack
(oset slack-current-team enterprise-token \"\")
(setq slack-request-curl-options (append request-curl-options (list \"--user-agent\" \"\")) ;; check the request tab, enterprise policies check you use the same user agent as login

Then use `slack-start' to make the changes effective.
"

  "Instruction to refresh slack tokens."
  :type 'string
  :group 'slack)

(defcustom slack-edit-refresh-token-instructions #'identity
  "A function to edit `slack-refresh-token-instructions' before display.
You can add it to append custom instructions that depend on context."
  :type 'function
  :group 'slack)

;;;###autoload
(defun slack-start (&optional team)
  (interactive
   (list
    slack-current-team))
  (cl-labels ((start
                (team)
                (when (file-exists-p (request--curl-cookie-jar))
                  (delete-file (request--curl-cookie-jar)))
                (slack-url-cookie-store team)
                (slack-team-kill-buffers team)
                (slack-if-let* ((ws (and (slot-boundp team 'ws)
                                         (oref team ws))))
                    (progn
                      (when (oref ws conn)
                        (slack-ws--close ws team))
                      (oset ws inhibit-reconnection nil)))
                (slack-authorize team)))
    (if team
        (start team)
      (if (hash-table-empty-p slack-teams-by-token)
          (slack-start (call-interactively #'slack-register-team))
        (cl-loop for team in (hash-table-values slack-teams-by-token)
                 do (start team))))
    (slack-enable-modeline)))

;;;###autoload
(defun slack-stop (force)
  "Quit all slack teams."
  (interactive "P")
  (run-hooks 'slack-before-quit-hook)
  (slack-ws-close)
  (when force
    ;; needed as fallback when refreshing credentials to cleanup old ones from local state
    (message "Deleted all teams from cache: you need to redefine them." )
    (setq slack-current-team nil
          slack-teams-by-token (make-hash-table :test 'equal)
          slack-tokens-by-id (make-hash-table :test 'equal)))
  (message "Slack stopped"))

;;;###autoload
(defun slack-register-team (&rest plist)
  "PLIST must contain :name and :token.
Available options (property name, type, default value)
:subscribed-channels [ list symbol ] nil
  notified when new message arrived in these channels.
:default [boolean] nil
  set this team as `slack-current-team' at registration time,
  so interactive commands use it without prompting.
:full-and-display-names [boolean] nil
  if t, use full name to display user name.
:mark-as-read-immediately [boolean] these
  if t, mark messages as read when open channel.
  if nil, mark messages as read when cursor hovered.
:modeline-enabled [boolean] nil
  if t, display mention count and has unread in modeline.
:modeline-name [or nil string] nil
  use this value in modeline.
  if nil, use team name.
:visible-threads [boolean] nil
  if t, thread replies are also displayed in channel buffer.
:websocket-event-log-enabled [boolean] nil
  if t, websocket event is logged.
  use `slack-log-open-event-buffer' to open the buffer.
:animate-image [boolean] nil
  if t, animate gif images."
  (interactive
   (let* ((name (read-from-minibuffer "Team Name: "))
          (token (read-from-minibuffer "Token: "))
          (cookie (when (slack-need-cookie-p token)
                    (read-from-minibuffer "Cookie: "))))
     (list :name name :token token :cookie cookie)))
  (cl-labels ((has-token-p (plist)
                (let ((token (plist-get plist :token)))
                  (and token (< 0 (length token)))))
              (register (team)
                (let ((same-team (slack-team-find-by-token (oref team token))))
                  (unless (and same-team (slack-team-connectedp same-team))
                    (when same-team
                      (slack-team-disconnect same-team)
                      (slack-team-connect team))
                    (puthash (oref team token) team slack-teams-by-token)
                    (if (plist-get plist :default)
                        (setq slack-current-team team))
                    (slack-user-prefs-update team)))))
    (if (has-token-p plist)
        (let ((team (slack-create-team plist)))
          (register team))
      (error ":token is required"))))

(cl-defmethod slack-team-connect ((team slack-team))
  (unless (slack-team-connectedp team)
    (slack-start team)))

(defun slack-change-current-team ()
  (interactive)
  (let* ((alist (mapcar #'(lambda (team) (cons (slack-team-name team)
                                               (oref team token)))
                        (hash-table-values slack-teams-by-token)))
         (selected (funcall slack-completing-read-function "Select Team: " alist))
         (team (slack-team-find-by-token
                (cdr-safe (cl-assoc selected alist :test #'string=)))))
    (setq slack-current-team team)
    (message "Deleting %s to clear old Slack cookies" (request--curl-cookie-jar))
    (delete-file (request--curl-cookie-jar))
    (message "Set slack-current-team to %s" (or (and team (oref team name))
                                                "nil"))
    (if team
        (slack-team-connect team))))

(defun slack-kill-all-buffers ()
  "Kill all slack buffers."
  (interactive)
  (-each
      (--filter (s-starts-with-p "*slack" (buffer-name it)) (buffer-list))
    'kill-buffer))

(defun slack-refresh-token ()
  "Extract Slack credentials interactively via browser DevTools.
Opens the Slack customize page, then prompts for the token and
(when needed) cookies.  Displays the collected credentials in a
results buffer so they can be stored externally (e.g. in `pass').

For xoxc tokens, cookies must be copied from the browser
Application/Storage tab because they are HttpOnly."
  (interactive)
  (when (file-exists-p (request--curl-cookie-jar))
    (delete-file (request--curl-cookie-jar))
    (message "Cleared old cookie jar"))
  (slack-refresh-token--open-browser)
  (let* ((token (slack-refresh-token--read-token))
         (needs-cookie (slack-need-cookie-p token))
         (enterprise-token (when needs-cookie
                             (slack-refresh-token--read-enterprise-token)))
         (cookie (when needs-cookie
                   (slack-refresh-token--read-cookie))))
    (slack-refresh-token--display-results
     token enterprise-token cookie)))

(defun slack-refresh-token--open-browser ()
  "Open the Slack customize page and copy the token JS to clipboard."
  (kill-new "window.prompt(\"your api token is: \", TS.boot_data.api_token)")
  (browse-url "https://my.slack.com/customize")
  (message "Opened Slack customize page.  JS snippet copied to clipboard."))

(defun slack-refresh-token--read-token ()
  "Prompt user for the Slack API token."
  (let ((token (read-string
                "Paste token (from Console: Ctrl-V the copied JS snippet, then copy the result): ")))
    (when (string-empty-p token)
      (user-error "Token is required"))
    (string-trim token)))

(defun slack-refresh-token--read-enterprise-token ()
  "Prompt user for the enterprise token.
Copies a JS snippet to extract it.  Returns nil if skipped."
  (kill-new "window.prompt(\"enterprise token: \", TS.boot_data.enterprise_api_token)")
  (let ((token (read-string
                "Paste enterprise token (JS copied to clipboard; RET to skip): ")))
    (unless (string-empty-p token)
      (string-trim token))))

(defun slack-refresh-token--read-cookie ()
  "Prompt user for the three cookie values needed by xoxc tokens.
Returns the combined cookie string in the format expected by
`slack-team-d-cookie' and friends."
  (message "Switch to Application/Storage tab in DevTools to copy cookies.")
  (let ((d (read-string "Paste cookie \"d\" value (starts with xoxd-): "))
        (d-s (read-string "Paste cookie \"d-s\" value: "))
        (lc (read-string "Paste cookie \"lc\" value (RET to skip): ")))
    (when (string-empty-p d)
      (user-error "Cookie \"d\" is required for xoxc tokens"))
    (when (string-empty-p d-s)
      (user-error "Cookie \"d-s\" is required for xoxc tokens"))
    (slack-refresh-token--format-cookie d d-s lc)))

(defun slack-refresh-token--format-cookie (d d-s lc)
  "Format cookie values D, D-S, and LC into the combined string.
The format is \"xoxd-...; d-s=VALUE; lc=VALUE\"."
  (let ((parts (list (string-trim d))))
    (push (format "d-s=%s" (string-trim d-s)) parts)
    (unless (string-empty-p lc)
      (push (format "lc=%s" (string-trim lc)) parts))
    (string-join (nreverse parts) "; ")))

(defun slack-refresh-token--display-results (token enterprise-token cookie)
  "Display TOKEN, ENTERPRISE-TOKEN, and COOKIE in a results buffer.
The buffer contains the values for external storage plus
ready-to-evaluate Elisp to apply them to the current team."
  (let ((buf (get-buffer-create "*slack-credentials*")))
    (with-current-buffer buf
      (let ((inhibit-read-only t))
        (erase-buffer)
        (insert "Slack credentials collected successfully.\n")
        (insert "Store these values securely (e.g. `pass').\n\n")
        (insert (format "token: %s\n" token))
        (when enterprise-token
          (insert (format "enterprise-token: %s\n" enterprise-token)))
        (when cookie
          (insert (format "cookie: %s\n" cookie)))
        (insert "\n;; Evaluate to apply to the current team:\n")
        (insert (format "(oset slack-current-team token \"%s\")\n" token))
        (when enterprise-token
          (insert (format "(oset slack-current-team enterprise-token \"%s\")\n"
                          enterprise-token)))
        (when cookie
          (insert (format "(oset slack-current-team cookie \"%s\")\n" cookie)))
        (insert "(slack-start)\n"))
      (goto-char (point-min))
      (special-mode))
    (switch-to-buffer-other-window buf)))

(defun slack-show-channel-info ()
  "Show an org mode buffer with channel information."
  (interactive)
  (let* ((channel (car (slack-current-room-and-team)))
         (b (format "*slack %s channel information*" (oref channel id))))
    (with-help-window b
      (with-current-buffer b
        (insert "* Information\n\n")
        (org-mode)
        (slack-override-keybiding-in-buffer
         (kbd "q")
         'bury-buffer)
        )
      (insert "- Name :: " (or (ignore-errors (oref channel name)) "Not available") "\n")
      (insert "- Topic :: " (format "%s" (oref channel topic)) "\n")
      (insert "- Purpose :: " (format "%s" (or (ignore-errors (oref channel purpose)) "Not available")) "\n")
      (insert "- Archived :: " (format "%s" (or (ignore-errors (oref channel is-archived)) "Not available")) "\n")
      (insert "- Creator :: " (or (ignore-errors (oref channel creator)) "Not available") "\n")
      ;; TODO add other slack-group properties AND format properly
      )))

(defun slack-show-channel-bookmarks (channel-id team)
  "Show an org mode buffer with the bookmarks of CHANNEL-ID for TEAM."
  (interactive (let ((room-and-team (slack-current-room-and-team)))
                 (list
                  (ignore-errors (oref (nth 0 room-and-team) id))
                  (nth 1 room-and-team))))
  (if (and channel-id team)
      (slack-bookmarks-request
       channel-id team
       (lambda (data)
         (-some--> data
           (plist-get it :bookmarks)
           (let ((b "*slack bookmarks for channel*"))
             (with-help-window b
               (with-current-buffer b
                 (insert "* Bookmarks\n\n")
                 (org-mode)
                 (slack-override-keybiding-in-buffer
                  (kbd "q")
                  'bury-buffer))
               (--each it
                 (with-current-buffer b
                   (let ((link (or (plist-get it :link) ""))
                         (title (or (plist-get it :title) "untitled")))
                     (insert (format "- [[%s][%s]]\n"
                                     (replace-regexp-in-string "\\]\\]" "]​]" link)
                                     (replace-regexp-in-string "\\]\\]" "]​]" title)))))))))))
    (user-error "Not in a Slack channel buffer")))

;;; Slack URL interception

(defcustom slack-intercept-urls t
  "If non-nil, intercept Slack workspace URLs and open them in emacs-slack.
When enabled, URLs matching *.slack.com/archives/* are handled by
`slack-browse-url' instead of the default browser."
  :type 'boolean
  :group 'slack)

(defconst slack-url-regexp
  "\\`https://[^/]+\\.slack\\.com/archives/[A-Z0-9]+/p[0-9]+"
  "Regexp matching Slack message permalink URLs.")

(defun slack-browse-url (url &rest _args)
  "Open a Slack message permalink URL in emacs-slack.
Parses the channel ID and timestamp from URL and navigates to the
message.  Falls back to the default browser if the team is not
connected or parsing fails."
  (condition-case err
      (let* ((info (slack-permalink-to-info url))
             (team-domain (plist-get info :team-domain))
             (room-id (plist-get info :room-id))
             (ts (plist-get info :ts))
             (thread-ts (plist-get info :thread-ts))
             (team (or (slack-team-find-by-domain team-domain)
                       ;; Try matching by team name as fallback
                       (--find (equal team-domain (oref it name))
                               (hash-table-values slack-teams-by-token)))))
        (if (and team room-id ts)
            (let ((room (slack-room-find room-id team)))
              (if room
                  (slack-open-message team room ts thread-ts ts t)
                (message "slack: room %s not found, opening in browser" room-id)
                (browse-url-default-browser url)))
          (message "slack: could not parse URL, opening in browser")
          (browse-url-default-browser url)))
    (error
     (message "slack-browse-url error: %S, opening in browser" err)
     (browse-url-default-browser url))))

(defun slack-register-url-handler ()
  "Register `slack-browse-url' with `browse-url-handlers'."
  (when (and slack-intercept-urls
             (boundp 'browse-url-handlers))
    (add-to-list 'browse-url-handlers
                 (cons slack-url-regexp #'slack-browse-url))))

(slack-register-url-handler)

;;;###autoload
(defun slack-start-and-select ()
  "Start Slack if not connected, then select a channel.
If all registered teams are already connected, skip straight to
channel selection."
  (interactive)
  (let ((disconnected (slack-disconnected-teams)))
    (if disconnected
        (progn
          (slack-start)
          (message "Connecting to Slack... use `slack-channel-select' once ready."))
      (call-interactively #'slack-channel-select))))

(defun slack-disconnected-teams ()
  "Return a list of registered teams that have no active WebSocket."
  (let (teams)
    (maphash (lambda (_token team)
               (unless (oref team ws)
                 (push team teams)))
             slack-teams-by-token)
    teams))

;;;###autoload
(defun slack-disconnect-all ()
  "Disconnect all connected Slack teams."
  (interactive)
  (maphash (lambda (_token team)
             (when (oref team ws)
               (slack-team-disconnect team)))
           slack-teams-by-token)
  (message "Disconnected all Slack teams."))

(provide 'slack)
;;; slack.el ends here
