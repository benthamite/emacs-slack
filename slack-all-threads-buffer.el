;;; slack-all-threads-buffer.el ---                  -*- lexical-binding: t; -*-

;; Copyright (C) 2018

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
(require 'eieio)
(require 'slack-util)
(require 'slack-buffer)
(require 'slack-create-message)
(require 'slack-message-buffer)

(defconst slack-subscriptions-thread-get-view-url "https://slack.com/api/subscriptions.thread.getView")
(defconst slack-subscriptions-thread-clear-all-url "https://slack.com/api/subscriptions.thread.clearAll")

(define-derived-mode slack-all-threads-buffer-mode
  slack-buffer-mode
  "Slack All Threads"
  (add-hook 'lui-pre-output-hook 'slack-mrkdwn-add-face nil t)
  (add-hook 'lui-pre-output-hook 'slack-display-inline-action t t)
  (add-hook 'post-command-hook #'slack-buffer--maybe-load-more-at-end nil t)
  (cursor-sensor-mode))

(defface slack-all-thread-buffer-thread-header-face
  '((t (:weight bold :height 1.2)))
  "Face used to All Threads buffer's each threads header."
  :group 'slack)

(defclass slack-all-threads-buffer (slack-buffer)
  ((current-ts :initarg :current-ts :type string)
   (has-more :initarg :has-more :type boolean)
   (total-unread-replies :initarg :total-unread-replies :type integer)
   (new-threads-count :initarg :new-threads-count :type integer)
   (threads :initarg :threads :type list '())))

(cl-defmethod slack-buffer-name ((this slack-all-threads-buffer))
  (format "*slack: %s : All Threads"
          (slack-team-name (slack-buffer-team this))))

(cl-defmethod slack-buffer-key ((_class (subclass slack-all-threads-buffer)))
  'slack-all-threads-buffer)

(cl-defmethod slack-buffer-key ((_this slack-all-threads-buffer))
  (slack-buffer-key 'slack-all-threads-buffer))

(cl-defmethod slack-team-buffer-key ((_class (subclass slack-all-threads-buffer)))
  'slack-all-threads-buffer)

(defclass slack-thread-view ()
  ((root-msg :initarg :root_msg :type slack-message)
   (latest-replies :initarg :latest_replies :type list :initform '())
   (unread-replies :initarg :unread_replies :type list :initform '())))

(cl-defmethod slack-message-user-ids ((this slack-thread-view))
  (let ((ret (slack-message-user-ids (oref this root-msg))))

    (cl-loop for m in (oref this latest-replies)
             do (cl-loop for id in (slack-message-user-ids m)
                         do (cl-pushnew id ret :test #'string=)))

    (cl-loop for m in (oref this unread-replies)
             do (cl-loop for id in (slack-message-user-ids m)
                         do (cl-pushnew id ret :test #'string=)))
    ret))

(cl-defmethod slack-buffer-find-message ((this slack-all-threads-buffer) ts)
  (cl-block outer
    (cl-loop for thread in (reverse (oref this threads))
             do (cl-loop for message in (append (list (oref thread root-msg))
                                                (oref thread latest-replies)
                                                (oref thread unread-replies))
                         when (string= ts (slack-ts message))
                         do (cl-return-from outer message)))))

(cl-defmethod slack-buffer--replace ((this slack-all-threads-buffer) ts)
  (let ((message (slack-buffer-find-message this ts)))
    (when message
      (slack-buffer-replace this message))))

(defun slack-create-thread-view (payload team)
  (let ((room (slack-room-find (plist-get (plist-get payload :root_msg)
                                          :channel)
                               team)))
    (make-instance 'slack-thread-view
                   :root_msg (slack-message-create (plist-get payload :root_msg) team room)
                   :latest_replies (mapcar #'(lambda (e) (slack-message-create e team room))
                                           (plist-get payload :latest_replies))
                   :unread_replies (mapcar #'(lambda (e) (slack-message-create e team room))
                                           (plist-get payload :unread_replies)))))

(defun slack-create-all-threads-buffer (team total-unread-replies new-threads-count threads has-more)
  (slack-if-let* ((buf (slack-buffer-find 'slack-all-threads-buffer team)))
      buf
    (slack-all-threads-buffer :team-id (oref team id)
                              :total-unread-replies total-unread-replies
                              :new-threads-count new-threads-count
                              :threads threads
                              :has-more has-more)))

(cl-defmethod slack-buffer-init-buffer ((this slack-all-threads-buffer))
  (let* ((buf (cl-call-next-method)))
    (with-current-buffer buf
      (let ((inhibit-read-only t))
        (delete-region (point-min) (point-max)))
      (slack-all-threads-buffer-mode)
      (slack-buffer-set-current-buffer this)
      (goto-char lui-input-marker)
      (let ((lui-time-stamp-position nil))
        (lui-insert (propertize "All Threads\n"
                                'face '(:weight bold))
                    t))
      (with-slots (threads current-ts) this
        (cl-loop for thread in threads
                 do (slack-buffer-insert-thread this thread)))
      (goto-char (point-min)))
    (slack-subscriptions-thread-clear-all (slack-buffer-team this))
    buf))

(cl-defmethod slack-buffer-insert ((this slack-all-threads-buffer) message
                                   &optional not-tracked-p)
  "Insert MESSAGE into the all-threads buffer THIS.
Adds `room-id' property so `slack-feed-open-at-point' can find the channel."
  (let ((lui-time-stamp-format "[%Y-%m-%d %H:%M] ")
        (lui-time-stamp-time (slack-message-time-stamp message))
        (team (slack-buffer-team this)))
    (lui-insert-with-text-properties
     (slack-message-to-string message team)
     'not-tracked-p not-tracked-p
     'ts (slack-ts message)
     'room-id (oref message channel)
     'slack-last-ts lui-time-stamp-last
     'cursor-sensor-functions '(slack-buffer-subscribe-cursor-event))
    (lui-insert "" t)))

(cl-defmethod slack-buffer-insert-thread ((this slack-all-threads-buffer) thread)
  (with-slots (root-msg latest-replies unread-replies) thread
    (oset this current-ts (oref root-msg last-read))
    (let* ((lui-time-stamp-position nil)
           (team (slack-buffer-team this))
           (channel (oref root-msg channel))
           (room (slack-room-find channel team))
           (prefix (or (and (slack-im-p room) "@") "#")))
      (lui-insert (slack-buffer-separator) t)
      (lui-insert (propertize (format "%s%s"
                                      prefix
                                      (slack-room-name room team))
                              'face
                              'slack-all-thread-buffer-thread-header-face)
                  t))

    (slack-buffer-insert this root-msg t)
    (cl-loop for reply in latest-replies
             do (slack-buffer-insert this reply t))
    (cl-loop for reply in unread-replies
             do (slack-buffer-insert this reply t))))

(cl-defmethod slack-buffer-has-next-page-p ((this slack-all-threads-buffer))
  (oref this has-more))

(cl-defmethod slack-buffer-delete-load-more-string ((_this slack-all-threads-buffer)))

(cl-defmethod slack-buffer-prepare-marker-for-history ((_this slack-all-threads-buffer)))

(cl-defmethod slack-buffer-insert-history ((this slack-all-threads-buffer))
  (with-slots (threads current-ts) this
    (cl-loop for thread in (cl-remove-if #'(lambda (e)
                                             (not (string< (oref (oref e root-msg)
                                                                 last-read)
                                                           current-ts)))
                                         threads)
             do (slack-buffer-insert-thread this thread))))

(cl-defmethod slack-buffer-insert--history ((this slack-all-threads-buffer))
  (slack-buffer-insert-history this))

(cl-defmethod slack-buffer-request-history ((this slack-all-threads-buffer) after-success)
  (let ((cur-point (point)))
    (with-slots (current-ts) this
      (cl-labels
          ((success (total-unread-replies new-threads-count threads has-more)
                    (oset this total-unread-replies total-unread-replies)
                    (oset this new-threads-count new-threads-count)
                    (oset this threads (append (oref this threads) threads))
                    (oset this has-more has-more)
                    (funcall after-success)
                    (when (and (< (point-min) cur-point)
                               (< cur-point (point-max)))
                      (goto-char cur-point))))
        (slack-subscriptions-thread-get-view (slack-buffer-team this) current-ts #'success)))))


(cl-defmethod slack-buffer-display-thread ((this slack-all-threads-buffer) ts)
  (with-slots (threads) this
    (slack-if-let* ((thread (cl-find-if #'(lambda (e) (string= ts (slack-ts (oref e root-msg))))
                                        threads))
                    (root (oref thread root-msg))
                    (room-id (oref root channel))
                    (team (slack-buffer-team this))
                    (room (slack-room-find room-id team)))
        (cl-labels
            ((display-thread (message)
                             (slack-thread-show-messages message
                                                         room
                                                         team)))
          (slack-if-let* ((ts (slack-ts root))
                          (message (slack-room-find-message room ts)))
              (display-thread message)
            (cl-labels
                ((success (messages _next-cursor)
                          (slack-room-set-messages room messages team)
                          (let ((message (slack-room-find-message room (slack-ts root))))
                            (display-thread message))))
              (slack-conversations-history room team
                                           :oldest (slack-ts root)
                                           :inclusive "true"
                                           :limit "1"
                                           :after-success #'success)))))))

(defun slack-all-threads ()
  (interactive)
  (let ((team (slack-team-select)))
    (cl-labels
        ((open-buffer (total-unread-replies new-threads-count threads has-more)
                      (slack-buffer-display
                       (slack-create-all-threads-buffer team
                                                        total-unread-replies
                                                        new-threads-count
                                                        threads
                                                        has-more)))
         (success (&rest args)
                  (slack-if-let* ((buf (slack-buffer-find 'slack-all-threads-buffer team)))
                      (progn
                        (kill-buffer (slack-buffer-buffer buf))
                        (apply #'open-buffer args))
                    (apply #'open-buffer args))))
      (slack-subscriptions-thread-get-view team nil #'success))))

(defun slack-subscriptions-thread-clear-all (team)
  (let ((current-ts (substring
                     (number-to-string (time-to-seconds (current-time)))
                     0 15)))
    (cl-labels
        ((success (&key data &allow-other-keys)
                  (slack-request-handle-error
                   (data "slack-subscriptions-thread-clear-all"))))
      (slack-request
       (slack-request-create
        slack-subscriptions-thread-clear-all-url
        team
        :type "POST"
        :params (list (cons "current_ts" current-ts))
        :success #'success)))))

(defun slack-subscriptions-thread-get-view (team &optional current-ts after-success)
  (let ((current-ts (or current-ts
                        (substring
                         (number-to-string (time-to-seconds (current-time)))
                         0 15))))
    (cl-labels
        ((callback (total-unread-replies
                    new-threads-count
                    threads
                    has-more)
                   (when (functionp after-success)
                     (funcall after-success
                              total-unread-replies
                              new-threads-count
                              threads
                              has-more)))
         (success (&key data &allow-other-keys)
                  (slack-request-handle-error
                   (data "slack-subscriptions-thread-get-view")
                   (let* ((total-unread-replies (plist-get data :total_unread_replies))
                          (new-threads-count (plist-get data :new_threads_count))
                          (threads (mapcar #'(lambda (e) (slack-create-thread-view e team))
                                           (plist-get data :threads)))
                          (has-more (eq (plist-get data :has_more) t))
                          (user-ids (slack-team-missing-user-ids
                                     team (cl-loop for thread in threads
                                                   nconc (slack-message-user-ids thread)))))
                     (if (< 0 (length user-ids))
                         (slack-users-info-request
                          user-ids team :after-success
                          #'(lambda () (callback total-unread-replies
                                                 new-threads-count
                                                 threads
                                                 has-more)))
                       (callback total-unread-replies
                                 new-threads-count
                                 threads
                                 has-more))))))
      (slack-request
       (slack-request-create
        slack-subscriptions-thread-get-view-url
        team
        :type "POST"
        :params (list (cons "current_ts" current-ts))
        :success #'success)))))

(cl-defmethod slack-all-threads-buffer-find-thread ((buf slack-all-threads-buffer) ts)
  "Find the `slack-thread-view' containing TS in BUF.
TS may belong to a root message or any reply within a thread."
  (cl-find-if
   (lambda (thr)
     (or (string= ts (slack-ts (oref thr root-msg)))
         (cl-find ts (oref thr latest-replies)
                  :key #'slack-ts :test #'string=)
         (cl-find ts (oref thr unread-replies)
                  :key #'slack-ts :test #'string=)))
   (oref buf threads)))

(cl-defmethod slack-feed--open ((buf slack-all-threads-buffer) ts)
  "Open the thread containing TS in all-threads buffer BUF.
TS may belong to a root message or any reply within a thread."
  (if-let* ((thread (slack-all-threads-buffer-find-thread buf ts)))
      (slack-buffer-display-thread buf (slack-ts (oref thread root-msg)))
    (message "Thread not found for ts %s" ts)))

(defun slack-all-threads-unfollow-at-point ()
  "Unfollow the thread at point.
Works from both `slack-all-threads-buffer' and
`slack-thread-message-buffer'."
  (interactive)
  (let ((buf slack-current-buffer))
    (cond
     ((and buf (object-of-class-p buf 'slack-all-threads-buffer))
      (slack-all-threads--unfollow-from-feed buf))
     ((and buf (object-of-class-p buf 'slack-thread-message-buffer))
      (slack-all-threads--unfollow-from-thread buf))
     (t (message "Not in a thread buffer")))))

(defun slack-all-threads--unfollow-from-feed (buf)
  "Unfollow thread at point in all-threads BUF."
  (if-let* ((ts (get-text-property (point) 'ts))
            (thread (slack-all-threads-buffer-find-thread buf ts))
            (root (oref thread root-msg))
            (team (slack-buffer-team buf))
            (room (slack-room-find (oref root channel) team)))
      (slack-all-threads-unfollow-thread room (slack-ts root) team)
    (message "No thread at point")))

(defun slack-all-threads--unfollow-from-thread (buf)
  "Unfollow the thread displayed in thread-message BUF."
  (slack-all-threads-unfollow-thread
   (slack-buffer-room buf) (oref buf thread-ts) (slack-buffer-team buf)))

(defun slack-all-threads-unfollow-thread (room ts team)
  "Unfollow thread TS in ROOM on TEAM."
  (cl-labels
      ((after-success ()
                      (slack-log "Unfollowed thread" team :level 'info)
                      (message "Unfollowed thread")))
    (slack-subscriptions-thread-remove room ts team
                                       #'after-success)))

(define-key slack-all-threads-buffer-mode-map (kbd "RET") 'slack-feed-open-at-point)
(define-key slack-all-threads-buffer-mode-map (kbd "n") 'slack-feed-goto-next)
(define-key slack-all-threads-buffer-mode-map (kbd "p") 'slack-feed-goto-prev)
(define-key slack-all-threads-buffer-mode-map (kbd "g") 'slack-all-threads)
(define-key slack-all-threads-buffer-mode-map (kbd "U") 'slack-all-threads-unfollow-at-point)

(provide 'slack-all-threads-buffer)
;;; slack-all-threads-buffer.el ends here
