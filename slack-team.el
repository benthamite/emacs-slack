;;; slack-team.el ---  team class                    -*- lexical-binding: t; -*-

;; Copyright (C) 2016  南優也

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
(require 'eieio)
(require 'slack-util)
(require 'slack-team-ws)
(require 'dash)
(require 's)

(declare-function emojify-create-emojify-emojis "emojify")
(declare-function slack-start "slack")
(defvar emojify--user-emojis-regexp)

(defvar slack-current-team nil)
(defvar slack-completing-read-function)

(defcustom slack-modeline-count-only-subscribed-channel t
  "Count unread only subscribed channel."
  :type 'boolean
  :group 'slack)

(defclass slack-team-threads ()
  ((initializedp :initform nil)
   (has-more :initform t)
   (total-unread-replies :initform 0 :type number)
   (new-threads-count :initform 0 :type number)))

(defclass slack-team ()
  ((id :initarg :id :initform nil)
   (token :initarg :token :initform nil)
   (enterprise-token :initarg :enterprise-token :initform nil)
   (cookie :initarg :cookie :initform nil)
   (name :initarg :name :initform nil)
   (domain :initarg :domain :initform nil)
   (self :initarg :self :initform nil)
   (self-id :initarg :self-id :initform nil)
   (self-name :initarg :self-name :initform nil)
   (channels :initarg :channels :initform (make-hash-table :test 'equal))
   (groups :initarg :groups :initform (make-hash-table :test 'equal))
   (ims :initarg :ims :initform (make-hash-table :test 'equal))
   (file-room :initform nil)
   (search-results :initform nil)
   (users :initarg :users :initform (make-hash-table :test 'equal))
   (bots :initarg :bots :initform (make-hash-table :test 'equal))
   (sent-message :initform (make-hash-table :test 'equal))
   (message-id :initform 0)
   (subscribed-channels :initarg :subscribed-channels
                        :type list :initform nil)
   (typing :initform nil)
   (typing-timer :initform nil)
   (reminders :initform nil :type list)
   (threads :type slack-team-threads :initform (make-instance 'slack-team-threads))
   (modeline-enabled :initarg :modeline-enabled :initform nil)
   (modeline-name :initarg :modeline-name :initform nil)
   (websocket-event-log-enabled :initarg :websocket-event-log-enabled :initform nil)
   (waiting-requests :initform nil)
   (authorize-request :initform nil)
   (emoji-download-watch-timer :initform nil)
   (star :initform nil)
   (slack-message-buffer :initform nil :type (or null hash-table))
   (slack-file-info-buffer :initform nil :type (or null hash-table))
   (slack-file-list-buffer :initform nil :type (or null hash-table))
   (slack-message-edit-buffer :initform nil :type (or null hash-table))
   (slack-pinned-items-buffer :initform nil :type (or null hash-table))
   (slack-user-profile-buffer :initform nil :type (or null hash-table))
   (slack-thread-message-buffer :initform nil :type (or null hash-table))
   (slack-message-share-buffer :initform nil :type (or null hash-table))
   (slack-room-message-compose-buffer :initform nil :type (or null hash-table))
   (slack-thread-message-compose-buffer :initform nil :type (or null hash-table))
   (slack-stars-buffer :initform nil :type (or null hash-table))
   (slack-search-result-buffer :initform nil :type (or null hash-table))
   (slack-activity-feed-buffer :initform nil :type (or null hash-table))
   (slack-scheduled-messages-buffer :initform nil :type (or null hash-table))
   (slack-dialog-buffer :initform nil :type (or null hash-table))
   (slack-dialog-edit-element-buffer :initform nil :type (or null hash-table))
   (slack-room-info-buffer :initform nil :type (or null hash-table))
   (slack-all-threads-buffer :initform nil :type (or null hash-table))
   (slack-message-attachment-preview-buffer :initform nil :type (or null hash-table))
   (full-and-display-names :initarg :full-and-display-names :initform nil)
   (mark-as-read-immediately :initarg :mark-as-read-immediately :initform t)
   (commands :initform '() :type list)
   (usergroups :initarg :usergroups :initform '() :type list)
   (ws :type slack-team-ws)
   (files :initarg :files :initform (make-hash-table :test 'equal))
   (file-ids :initarg file-ids :initform '())
   (counts :initform nil)
   (emoji-master :initform (make-hash-table :test 'equal))
   (visible-threads :initarg :visible-threads :initform nil :type boolean)
   (animate-image :initarg :animate-image :initform nil :type boolean)
   (dnd-status :initform (make-hash-table :test 'equal))
   (presence :initform (make-hash-table :test 'equal))
   (disable-block-format :initform nil :initarg :disable-block-format :type boolean)
   (user-prefs :initform nil)
   ))

(defun slack-create-team (plist)
  "Create and return a new team instance from PAYLOAD.
PLIST is the plist argument."
  (let ((ws (apply #'make-instance 'slack-team-ws
                   (slack-collect-slots 'slack-team-ws plist)))
        (team (apply #'make-instance 'slack-team
                     (slack-collect-slots 'slack-team plist))))
    (oset team ws ws)
    team))

(cl-defmethod slack-equalp ((this slack-team) other)
  "Return non-nil when the team equals the OTHER argument.
THIS is the slack-team instance."
  (and (oref this id)
       (oref other id)
       (string= (oref this id) (oref other id))))

(cl-defmethod slack-team-set-ws-url ((this slack-team) url)
  "Store URL as the websocket endpoint for THIS team."
  (with-slots (ws) this
    (oset ws url url)))

(cl-defmethod slack-team-kill-buffers ((this slack-team) &key (except nil))
  "Kill all Slack buffers belonging to THIS team.
Slot symbols in EXCEPT are excluded from the kill."
  (let* ((l (list 'slack-message-buffer
                  'slack-file-info-buffer
                  'slack-file-list-buffer
                  'slack-message-edit-buffer
                  'slack-pinned-items-buffer
                  'slack-user-profile-buffer
                  'slack-thread-message-buffer
                  'slack-message-share-buffer
                  'slack-room-message-compose-buffer
                  'slack-thread-message-compose-buffer
                  'slack-search-result-buffer
                  'slack-stars-buffer))
         (slots (cl-remove-if #'(lambda (e) (cl-find e except)) l)))
    (cl-loop for slot in slots
             do (let ((ht (slot-value this slot)))
                  (when (hash-table-p ht)
                    (cl-loop for buffer in (hash-table-values ht)
                             do (when buffer
                                  (kill-buffer (slack-buffer-buffer buffer)))))))))

(defvar slack-tokens-by-id (make-hash-table :test 'equal))
(defvar slack-teams-by-token (make-hash-table :test 'equal))
(defun slack-team-find-by-token (token)
  "Return the registered team keyed by TOKEN, or nil."
  (gethash token slack-teams-by-token))

(defun slack-team-find (id)
  "Return the registered team whose team ID is ID, or nil."
  (let ((token (gethash id slack-tokens-by-id)))
    (when token
      (slack-team-find-by-token token))))

(defun slack-team-find-by-domain (team-domain)
  "Go from TEAM-DOMAIN to team."
  (--find
   (equal team-domain (oref it domain))
   (hash-table-values slack-teams-by-token)))

(cl-defmethod slack-team--delete ((this slack-team))
  "Remove THIS team from the global token/id registries."
  (remhash (oref this id) slack-tokens-by-id)
  (remhash (oref this token) slack-teams-by-token))

(cl-defmethod slack-team-equalp ((team slack-team) other)
  "Return non-nil when TEAM and OTHER share the same token."
  (with-slots (token) team
    (string= token (oref other token))))

(cl-defmethod slack-team-name ((team slack-team))
  "Return the display name of TEAM."
  (oref team name))

(defun slack-team-canonical (team)
  "Return the canonical object for TEAM from `slack-teams-by-token'.
Falls back to TEAM itself when the token is not found."
  (or (gethash (oref team token) slack-teams-by-token)
      team))

(cl-defun slack-team-select (&optional no-default)
  "Prompt the user to select a Slack team and return it.
When `slack-current-team' is already set and NO-DEFAULT is nil,
return it without prompting.  Otherwise prompt from all
registered teams (including disconnected ones) and remember the
selection in `slack-current-team'."
  (if (and slack-current-team (not no-default))
      (slack-team-canonical slack-current-team)
    (let* ((teams (hash-table-values slack-teams-by-token))
           (alist (mapcar (lambda (team)
                            (cons (slack-team-name team) (oref team token)))
                          teams))
           (selected (funcall slack-completing-read-function
                              "Select Team: " alist))
           (team (slack-team-find-by-token
                  (cdr (cl-assoc selected alist :test #'string=)))))
      (setq slack-current-team team)
      team)))

(cl-defmethod slack-team-connectedp ((team slack-team))
  "Return non-nil when TEAM's websocket is connected."
  (oref (oref team ws) connected))

(defun slack-team-modeline-enabledp (team)
  "Return non-nil when TEAM should contribute to the modeline."
  (oref team modeline-enabled))

(cl-defmethod slack-team-event-log-enabledp ((team slack-team))
  "Return non-nil when websocket event logging is enabled for TEAM."
  (oref team websocket-event-log-enabled))

(cl-defmethod slack-team-mark-as-read-immediatelyp ((team slack-team))
  "Return non-nil when TEAM marks messages as read immediately."
  (oref team mark-as-read-immediately))

(defvar slack-team-random-numbers-for-client-token
  (let ((result nil))
    (dotimes (_ 10)
      (push (random 10) result))
    (mapconcat #'number-to-string result "")))

(cl-defmethod slack-team-client-token ((team slack-team))
  "Return a per-session client token string identifying TEAM."
  (format "EmacsSlack-%s-%s"
          (oref team id)
          slack-team-random-numbers-for-client-token))

(cl-defmethod slack-team-inc-message-id ((team slack-team))
  "Increment and return TEAM's outgoing websocket message id counter."
  (with-slots (message-id) team
    (if (eq message-id (1- most-positive-fixnum))
        (setq message-id 1)
      (cl-incf message-id))))

(defun slack-team-watch-emoji-download-complete (team paths)
  "Finalize emoji download for TEAM once every file in PATHS exists."
  (if (eq (length (cl-remove-if #'identity (mapcar #'file-exists-p paths)))
          0)
      (when (timerp (oref team emoji-download-watch-timer))
        (cancel-timer (oref team emoji-download-watch-timer))
        (oset team emoji-download-watch-timer nil)
        (emojify-create-emojify-emojis t)
        ;; https://github.com/iqbalansari/emacs-emojify/issues/103
        ;; when the size of the user defined emojis is too large,
        ;; emojify creates a regex larger than emacs can handle
        ;; but it works fine with its simple (github) style regex
        (when (> (length paths) 1500) (setq emojify--user-emojis-regexp nil))
        )))

(cl-defmethod slack-team-token ((this slack-team))
  "Return the authentication token for THIS team."
  (oref this token))

(cl-defmethod slack-team-enterprise-token ((this slack-team))
  "Return the enterprise authentication token for THIS team, if any."
  (oref this enterprise-token))

(cl-defmethod slack-team-cookie ((this slack-team))
  "Return the raw cookie string for THIS team."
  (oref this cookie))

(cl-defmethod slack-team-d-cookie ((this slack-team))
  "Like `slack-team-cookie' but it only returns the value of the cookie for THIS.
This seems necessary for allowing api call to still carry d-s.
TODO I should experiment to see if api calls require cookies."
  (nth 0 (s-split ";" (oref this cookie))))

(cl-defmethod slack-team-d-s-cookie ((this slack-team))
  "Get d-s cookie useful to authenticate to websocket.
THIS is the slack-team instance."
  (ignore-errors (s-trim (s-replace ";" "" (nth 0 (s-split "lc=" (nth 1 (s-split "d-s=" (oref this cookie)))))))))

(cl-defmethod slack-team-lc-cookie ((this slack-team))
  "Get lc cookie useful to authenticate to websocket.
THIS is the slack-team instance."
  (or
   (ignore-errors
     (s-trim (s-replace ";" "" (nth 1 (s-split "lc=" (nth 1 (s-split "d-s=" (oref this cookie))))))))
   ;; assuming we can default to d-s if not present because I am not sure if it is always needed
   (slack-team-d-s-cookie this)))

(cl-defmethod slack-team-missing-user-ids ((this slack-team) user-ids)
  "Return the subset of USER-IDS not yet cached in THIS team's users table."
  (let ((exists-user-ids (hash-table-keys (oref this users))))
    (cl-remove-if #'(lambda (e) (cl-find e exists-user-ids :test #'string=))
                  (cl-remove-duplicates user-ids :test #'string=))))

(cl-defmethod slack-team-visible-threads-p ((this slack-team))
  "Return non-nil when THIS team shows threads inline in channel buffers."
  (oref this visible-threads))

(cl-defmethod slack-team-animate-image-p ((this slack-team))
  "Return non-nil when THIS team renders animated images."
  (oref this animate-image))

(cl-defmethod slack-team-channels ((this slack-team))
  "Return the list of cached channel objects for THIS team."
  (hash-table-values (oref this channels)))

(cl-defmethod slack-team-groups ((this slack-team))
  "Return the list of cached private-channel objects for THIS team."
  (hash-table-values (oref this groups)))

(cl-defmethod slack-team-ims ((this slack-team))
  "Return the list of cached direct-message objects for THIS team."
  (hash-table-values (oref this ims)))

(defvar slack-team--conversations-loaded (make-hash-table :test 'equal)
  "Hash table mapping team ID to non-nil when conversations are loaded.")

(defun slack-team-conversations-loaded-p (team)
  "Return non-nil if TEAM's conversation list has been fetched."
  (gethash (oref team id) slack-team--conversations-loaded))

(defun slack-team-set-conversations-loaded (team)
  "Mark TEAM's conversation list as loaded."
  (puthash (oref team id) t slack-team--conversations-loaded))

(defun slack-team-ensure-conversations-loaded (team)
  "Signal an error if TEAM's conversation list hasn't loaded yet.
If TEAM was never authorized (ID is nil), auto-start it.
Returns non-nil on success so it can be used in `if-let*'
bindings."
  (unless (slack-team-conversations-loaded-p team)
    (if (slack-team-never-authorized-p team)
        (progn
          (slack-start team)
          (user-error "Slack was not connected for \"%s\"; starting now — please retry in a moment"
                      (oref team name)))
      (user-error "Slack is still loading conversations for \"%s\"; please wait for \">> Slack is ready!\""
                  (oref team name))))
  t)

(defun slack-team-never-authorized-p (team)
  "Return non-nil if TEAM has never completed authorization.
A nil ID means the rtm.connect handshake never succeeded."
  (null (oref team id)))

(cl-defmethod slack-team-users ((this slack-team))
  "Return the list of cached user plists for THIS team."
  (hash-table-values (oref this users)))

(cl-defmethod slack-team-set-users ((this slack-team) users)
  "Add or replace USERS in THIS team's user cache, keyed by user id."
  (cl-loop for user in users
           do (puthash (plist-get user :id)
                       user
                       (oref this users))))

(cl-defmethod slack-team-set-bots ((this slack-team) bots)
  "Add or replace BOTS in THIS team's bot cache, keyed by bot id."
  (cl-loop for bot in bots
           do (puthash (plist-get bot :id)
                       bot
                       (oref this bots))))

(cl-defmethod slack-team-bots ((this slack-team))
  "Return the list of cached bot plists for THIS team."
  (hash-table-values (oref this bots)))

(cl-defmethod slack-team-files ((this slack-team))
  "Return the list of cached file objects for THIS team in insertion order."
  (let ((ret))
    (cl-loop for id in (oref this file-ids)
             do (push (gethash id (oref this files))
                      ret))
    ret))

(cl-defmethod slack-team-id ((this slack-team))
  "Return the Slack-assigned team id for THIS team."
  (oref this id))

(provide 'slack-team)
;;; slack-team.el ends here
