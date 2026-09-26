;;; slack-modeline.el ---                            -*- lexical-binding: t; -*-

;; Copyright (C) 2019  南優也

;; Author: 南優也 <yuya373@yuya373noMacBook-Pro.local>
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
(require 'slack-team)
(require 'slack-counts)
(require 'slack-room)

(defvar slack-extra-subscribed-channels)

(declare-function slack-activity-feed-refresh-unread-summary
                  "slack-activity-feed-buffer")

(defvar slack-modeline nil)

(defvar slack-has-unreads nil
  "Non-nil when any connected Slack team has unread messages.")

(defvar slack-unread-count 0
  "Total mention count across all connected Slack teams.")

(defcustom slack-enable-global-mode-string nil
  "If true, add `slack-modeline' to `global-mode-string'."
  :type 'boolean
  :group 'slack)

(defcustom slack-modeline-formatter #'slack-default-modeline-formatter
  "Format modeline with Arg `((team-name . (has-unreads . cnt)))'."
  :type 'function
  :group 'slack)

(defcustom slack-counts-refresh-interval 60
  "Seconds between periodic count and Activity feed refreshes.
Set to nil to disable.  The default is 60 seconds."
  :type '(choice (const :tag "Disabled" nil)
                 (integer :tag "Seconds"))
  :group 'slack)

(defcustom slack-activity-refresh-debounce 30
  "Minimum seconds between Activity feed API calls.
Prevents excessive calls when many WebSocket events arrive in
quick succession.  The periodic timer bypasses this limit."
  :type 'integer
  :group 'slack)

(defvar slack-counts-refresh-timer nil
  "Timer for periodic counts refresh.")

(defvar slack-activity-last-refresh-time 0
  "Time of the last Activity feed refresh as `float-time'.")

(defface slack-modeline-has-unreads-face
  '((t (:weight bold :foreground "#d33682")))
  "Face used to team has unreads message in modeline."
  :group 'slack)

(defface slack-modeline-thread-has-unreads-face
  '((t (:weight bold :foreground "#d33682")))
  "Face used to thread has unreads message in modeline."
  :group 'slack)

(defface slack-modeline-channel-has-unreads-face
  '((t (:weight bold :foreground "#d33682")))
  "Face used to channel has unreads message in modeline."
  :group 'slack)

(defun slack-default-modeline-formatter (alist)
  "Format ALIST where each element is a team summary.
Each entry is (team-name . ((thread . (unreads . count))
\(channel . (unreads . count))))."
  (mapconcat #'(lambda (e)
                 (let* ((team-name (car e))
                        (summary (cdr e))
                        (thread (cdr (cl-assoc 'thread summary)))
                        (channel (cdr (cl-assoc 'channel summary)))
                        (thread-has-unreads (car thread))
                        (channel-has-unreads (car channel))
                        (has-unreads (or thread-has-unreads
                                         channel-has-unreads))
                        (thread-mention-count (cdr thread))
                        (channel-mention-count (cdr channel)))
                   (format "[ %s: %s, %s ]"
                           (if has-unreads
                               (propertize team-name
                                           'face 'slack-modeline-has-unreads-face)
                             team-name)
                           (if (or channel-has-unreads (< 0 channel-mention-count))
                               (propertize (number-to-string channel-mention-count)
                                           'face 'slack-modeline-channel-has-unreads-face)
                             channel-mention-count)
                           (if (or thread-has-unreads (< 0 thread-mention-count))
                               (propertize (number-to-string thread-mention-count)
                                           'face 'slack-modeline-thread-has-unreads-face)
                             thread-mention-count))))
             alist " "))

(defun slack-enable-modeline ()
  "Enable the Slack mode-line indicator and start periodic refresh."
  (when slack-enable-global-mode-string
    (add-to-list 'global-mode-string '(:eval slack-modeline) t))
  (slack-counts-start-refresh-timer))

(defun slack-update-modeline ()
  "Recompute unread summary variables and refresh the mode line."
  (interactive)
  (slack-update-unread-summary)
  (let ((teams (cl-remove-if-not #'slack-team-modeline-enabledp
                                 (hash-table-values slack-teams-by-token))))
    (when (< 0 (length teams))
      (setq slack-modeline
            (funcall slack-modeline-formatter
                     (mapcar #'(lambda (e)
                                 (cons (or (oref e modeline-name)
                                           (slack-team-name e))
                                       (slack-team-counts-summary e)))
                             teams)))))
  (force-mode-line-update))

(defun slack-update-unread-summary ()
  "Trigger a debounced Activity feed refresh.
Updates `slack-has-unreads' and `slack-unread-count' from the
`activity.feed' API, which matches Slack's Activity section
exactly.  Skips the call if less than
`slack-activity-refresh-debounce' seconds have elapsed."
  (let ((now (float-time)))
    (when (< slack-activity-refresh-debounce
             (- now slack-activity-last-refresh-time))
      (setq slack-activity-last-refresh-time now)
      (slack-activity-feed-refresh-unread-summary))))

(defun slack-team-counts-summary (team)
  "Return an alist summarizing unread state for TEAM.
The result has the form `((thread . (HAS-UNREADS . COUNT))
\(channel . (HAS-UNREADS . COUNT)))', aggregating the mention
counts tracked by `slack-counts'."
  (with-slots (counts) team
    (if counts
        (with-slots (threads channels mpims ims) counts
          (let ((thread (cons (oref threads has-unreads)
                              (oref threads mention-count)))
                (channel (slack-modeline--conversation-summary
                          team channels mpims ims)))
            (list (cons 'thread thread)
                  (cons 'channel channel))))
      (list (cons 'thread (cons nil 0))
            (cons 'channel (cons nil 0))))))

(defun slack-modeline--conversation-summary (team channels mpims ims)
  "Return (has-unreads . mention-count) for TEAM's conversation counts.
CHANNELS, MPIMS and IMS are the per-conversation count lists of a
`slack-counts' object.  Direct messages always count.  Channels and
groups count when `slack-modeline--channel-counted-p' says so."
  (let (unreads
        (total 0))
    (dolist (cc (append ims (cl-remove-if-not
                             (lambda (cc)
                               (slack-modeline--channel-counted-p
                                (slack-room-find (oref cc id) team) team))
                             (append channels mpims))))
      (cl-incf total (oref cc mention-count))
      (when (and (oref cc has-unreads) (null unreads))
        (setq unreads t)))
    (cons unreads total)))

(defun slack-modeline--channel-counted-p (room team)
  "Return non-nil when channel or group ROOM of TEAM counts in the mode line.
When `slack-modeline-count-only-subscribed-channel' is non-nil and TEAM
has subscribed channels configured, only rooms that satisfy
`slack-room-subscribedp' count, and a room that is not loaded yet is
skipped because its subscription cannot be decided.  Otherwise every
room counts."
  (or (not slack-modeline-count-only-subscribed-channel)
      (not (or (oref team subscribed-channels)
               slack-extra-subscribed-channels))
      (and room (slack-room-subscribedp room team))))

(cl-defmethod slack-counts-update ((team slack-team))
  "Update counts for TEAM."
  (slack-client-counts team
                       #'(lambda (counts)
                           (oset team counts counts)
                           (slack-update-modeline))))

(defun slack-counts-refresh-all ()
  "Refresh counts and Activity state for all connected teams."
  (maphash (lambda (_token team)
             (when (slack-team-connectedp team)
               (slack-counts-update team)))
           slack-teams-by-token)
  (setq slack-activity-last-refresh-time (float-time))
  (slack-activity-feed-refresh-unread-summary))

(defun slack-counts-start-refresh-timer ()
  "Start the periodic counts refresh timer."
  (slack-counts-stop-refresh-timer)
  (when slack-counts-refresh-interval
    (setq slack-counts-refresh-timer
          (run-with-timer slack-counts-refresh-interval
                          slack-counts-refresh-interval
                          #'slack-counts-refresh-all))))

(defun slack-counts-stop-refresh-timer ()
  "Stop the periodic counts refresh timer."
  (when (timerp slack-counts-refresh-timer)
    (cancel-timer slack-counts-refresh-timer)
    (setq slack-counts-refresh-timer nil)))

(provide 'slack-modeline)
;;; slack-modeline.el ends here
