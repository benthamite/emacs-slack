;;; slack-stars-buffer.el ---                        -*- lexical-binding: t; -*-

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

(require 'eieio)
(require 'slack-util)
(require 'slack-team)

(declare-function slack-star-item-file "slack-star")
(require 'slack-buffer)
(require 'slack-message-buffer)
(require 'slack-star)
(require 'slack-message)

(define-derived-mode slack-stars-buffer-mode slack-buffer-mode "Slack Saved Items")

(defclass slack-stars-buffer (slack-buffer)
  ())

(defun slack-stars--prefetch-messages (star-items team callback)
  "Prefetch uncached messages for STAR-ITEMS in parallel, then call CALLBACK.
Fires async API requests for all messages not already in cache,
and invokes CALLBACK (with no arguments) once every request has
completed (or immediately if all messages are cached)."
  (let ((pending (list 0)))
    (dolist (item star-items)
      (let* ((ts (oref item ts))
             (room-id (oref item item-id))
             (room (slack-room-find room-id team)))
        (when (and room (not (slack-room-find-message room ts)))
          (cl-incf (car pending)))))
    (if (= 0 (car pending))
        (funcall callback)
      (dolist (item star-items)
        (slack-stars--prefetch-single item team pending callback)))))

(defun slack-stars--prefetch-single (item team pending callback)
  "Prefetch a single ITEM for TEAM, decrementing PENDING and calling CALLBACK."
  (let* ((ts (oref item ts))
         (thread-ts (oref item thread-ts))
         (room-id (oref item item-id))
         (room (slack-room-find room-id team))
         (is-reply (and thread-ts (not (string-equal ts thread-ts)))))
    (when (and room (not (slack-room-find-message room ts)))
      (condition-case err
          (let ((done (lambda (messages &rest _)
                        (when messages
                          (slack-room-set-messages room messages team))
                        (when (= 0 (cl-decf (car pending)))
                          (funcall callback))))
                (fail (lambda (&rest _)
                        (when (= 0 (cl-decf (car pending)))
                          (funcall callback)))))
            (if is-reply
                (slack-conversations-replies
                 room thread-ts team
                 :oldest ts
                 :inclusive "true"
                 :limit "1"
                 :after-success done
                 :on-error fail)
              (slack-conversations-history
               room team
               :latest ts
               :inclusive "true"
               :limit "1"
               :after-success done
               :on-error fail)))
        (error
         (message "slack-stars: prefetch error for %s: %S" ts err)
         (when (= 0 (cl-decf (car pending)))
           (funcall callback)))))))

(cl-defmethod slack-buffer-name ((this slack-stars-buffer))
  (let ((team (slack-buffer-team this)))
    (format "*slack: %s : Saved items*" (oref team name))))

(cl-defmethod slack-buffer-key ((_class (subclass slack-stars-buffer)) &rest _args)
  'slack-stars-buffer)

(cl-defmethod slack-buffer-key ((_this slack-stars-buffer))
  (slack-buffer-key 'slack-stars-buffer))

(cl-defmethod slack-team-buffer-key ((_class (subclass slack-stars-buffer)))
  'slack-stars-buffer)

(cl-defmethod slack-buffer-toggle-email-expand ((this slack-stars-buffer) file-id)
  (slack-if-let* ((team (slack-buffer-team this))
                  (ts (get-text-property (point) 'ts))
                  (items (slack-star-items (oref team star)))
                  (item (cl-find-if #'(lambda (e) (string= ts (slack-ts e)))
                                    items))
                  (file (slack-star-item-file item file-id)))
      (progn
        (oset file is-expanded (not (oref file is-expanded)))
        (slack-buffer--replace this ts))))

(cl-defmethod slack-buffer-insert ((this slack-stars-buffer) message &optional not-tracked-p)
  (let ((lui-time-stamp-format "[%Y-%m-%d %H:%M] ")
        (lui-time-stamp-time (seconds-to-time
                              (string-to-number
                               (slack-ts
                                message)))))
    (lui-insert-with-text-properties
     (slack-message-to-string message (slack-buffer-team this))
     'ts (slack-ts message)
     'team-id (oref (slack-buffer-team this) id)
     'room-id (oref message channel)
     'not-tracked-p not-tracked-p)
    (lui-insert "" t)))

(cl-defmethod slack-buffer-has-next-page-p ((this slack-stars-buffer))
  (let ((team (slack-buffer-team this)))
    (slack-star-has-next-page-p (oref team star))))

(cl-defmethod slack-buffer-loading-message-end-point ((_this slack-stars-buffer))
  (previous-single-property-change (point-max)
                                   'loading-message))

(cl-defmethod slack-buffer-delete-load-more-string ((this slack-stars-buffer))
  (let* ((inhibit-read-only t)
         (loading-message-end
          (slack-buffer-loading-message-end-point this))
         (loading-message-start
          (previous-single-property-change loading-message-end
                                           'loading-message)))
    (delete-region loading-message-start
                   loading-message-end)))

(cl-defmethod slack-stars--insert-items ((this slack-stars-buffer) star-items)
  "Insert messages for STAR-ITEMS into THIS buffer."
  (let ((team (slack-buffer-team this)))
    (cl-loop for i in star-items
             for room = (slack-room-find (oref i item-id) team)
             for m = (and room (slack-room-find-message room (oref i ts)))
             when m do (slack-buffer-insert this m))))

(cl-defmethod slack-stars--insert-tail ((this slack-stars-buffer))
  "Insert load-more or end-of-list marker at the bottom of THIS buffer."
  (let ((lui-time-stamp-position nil))
    (if (slack-buffer-has-next-page-p this)
        (slack-buffer-insert-load-more this)
      (lui-insert "(no more items)\n" t))))

(cl-defmethod slack-buffer-load-more ((this slack-stars-buffer))
  "Load the next page of saved items and append at the bottom."
  (if (slack-buffer-has-next-page-p this)
      (let* ((team (slack-buffer-team this))
             (star (oref team star))
             (old-count (length (slack-star-items star))))
        (slack-stars-list-request
         team (oref star cursor)
         (lambda ()
           (let ((new-items (nthcdr old-count (slack-star-items star))))
             (slack-stars--prefetch-messages
              new-items team
              (lambda ()
                (with-current-buffer (slack-buffer-buffer this)
                  (let ((inhibit-read-only t))
                    (slack-buffer-delete-load-more-string this)
                    (slack-stars--insert-items this new-items)
                    (slack-stars--insert-tail this)))))))))
    (message "No more items.")))

(cl-defmethod slack-buffer-init-buffer ((this slack-stars-buffer))
  (let* ((buf (cl-call-next-method))
         (team (slack-buffer-team this))
         (star (oref team star))
         (items (slack-star-items star)))
    (with-current-buffer buf
      (slack-stars-buffer-mode)
      (slack-buffer-set-current-buffer this))
    (slack-stars--prefetch-messages
     items team
     (lambda ()
       (with-current-buffer buf
         (let ((inhibit-read-only t))
           (slack-stars--insert-items this items)
           (slack-stars--insert-tail this))
         (goto-char (point-min)))))
    buf))

(defun slack-create-stars-buffer (team)
  (slack-if-let* ((buf (slack-buffer-find 'slack-stars-buffer team)))
      buf
    (make-instance 'slack-stars-buffer :team-id (oref team id))))

(cl-defmethod slack-buffer-remove-star ((this slack-stars-buffer) ts)
  "Remove THIS star at TS."
  (let ((team (slack-buffer-team this)))
    (with-slots (star) team
      (slack-star-remove-star star ts team))))

(cl-defmethod slack-buffer-message-delete ((this slack-stars-buffer) ts)
  (let ((buffer (slack-buffer-buffer this))
        (inhibit-read-only t))
    (with-current-buffer buffer
      (slack-if-let* ((beg (slack-buffer-ts-eq (point-min) (point-max) ts))
                      (end (next-single-property-change beg 'ts)))
          (delete-region beg end)))))

(cl-defmethod slack-buffer--replace ((this slack-stars-buffer) ts)
  (let ((team (slack-buffer-team this)))
    (with-slots (star) team
      (let* ((star-items (slack-star-items star))
             (star-item (cl-find-if (lambda (i) (string= (oref i ts) ts))
                                    star-items))
             (room (and star-item
                        (slack-room-find (oref star-item item-id) team)))
             (item (and room (slack-room-find-message room ts))))
        (when item
          (lui-replace (slack-message-to-string item team)
                       #'(lambda ()
                           (string= (get-text-property (point) 'ts)
                                    ts))))))))

;;;###autoload
(defun slack-saved-items ()
  "Show the saved items buffer."
  (interactive)
  (let* ((team (slack-team-select))
         (buf (slack-buffer-find 'slack-stars-buffer team)))
    (if buf (slack-buffer-display buf)
      (slack-stars-list-request
       team nil
       #'(lambda () (slack-buffer-display (slack-create-stars-buffer team)))))))

;;;###autoload
(defalias 'slack-stars-list 'slack-saved-items)

(defun slack-saved-items-open-message ()
  "Open the message at point in its channel or thread buffer."
  (interactive)
  (if-let* ((ts (get-text-property (point) 'ts))
            (team-id (get-text-property (point) 'team-id))
            (room-id (get-text-property (point) 'room-id))
            (thread-ts (or (get-text-property (point) 'thread-ts) ts))
            (team (slack-team-find team-id)))
      (slack-open-message
       team
       (slack-room-find room-id team)
       (--find (s-matches-p "[0-9]" it) (list ts))
       (--find (s-matches-p "[0-9]" it) (list thread-ts)))
    (error "Not possible to jump to message")))

(defalias 'slack-stars-open-message 'slack-saved-items-open-message)
(define-key slack-stars-buffer-mode-map (kbd "RET") 'slack-saved-items-open-message)

(defun slack-message-remove-from-saved ()
  "Remove the saved item at point."
  (interactive)
  (slack-if-let* ((buffer slack-current-buffer))
      (slack-buffer-remove-star buffer (slack-get-ts))))

(defalias 'slack-message-remove-star 'slack-message-remove-from-saved)
(define-key slack-stars-buffer-mode-map (kbd "K") 'slack-message-remove-from-saved)

(defun slack-saved-items-refresh-buffer ()
  "Close and reopen saved items buffer to refresh contents."
  (interactive)
  (kill-buffer)
  (slack-saved-items))

(defalias 'slack-stars-refresh-buffer 'slack-saved-items-refresh-buffer)
(define-key slack-stars-buffer-mode-map (kbd "G") 'slack-saved-items-refresh-buffer)

(provide 'slack-stars-buffer)
;;; slack-stars-buffer.el ends here
