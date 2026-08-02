;;; slack-page-state.el --- remote page lifecycle  -*- lexical-binding: t; -*-

;; Copyright (C) 2026  Pablo Stafforini

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

;; Track the generation and lifecycle of remote pages independently of buffers.

;;; Code:

(require 'cl-lib)

(cl-defstruct (slack-page-state (:constructor slack-page-state-create))
  (status 'unloaded)
  (generation 0)
  loaded-p
  value
  continuation
  has-more
  error
  updated-at
  (committed-generation 0)
  (ready-generation 0)
  commit-waiters
  ready-waiters
  error-waiters)

(defun slack-page-state-in-flight-p (state)
  "Return non-nil when STATE has a page request in flight."
  (memq (slack-page-state-status state) '(loading refreshing)))

(defun slack-page-state-begin (state &optional refresh)
  "Begin loading STATE and return its generation, or nil when coalesced.
When REFRESH is non-nil, refresh loaded data but still coalesce in-flight work."
  (cond
   ((slack-page-state-in-flight-p state) nil)
   ((and (slack-page-state-loaded-p state) (not refresh)) nil)
   (t
    (cl-incf (slack-page-state-generation state))
    (setf (slack-page-state-status state)
          (if (slack-page-state-loaded-p state) 'refreshing 'loading)
          (slack-page-state-error state) nil)
    (slack-page-state-generation state))))

(defun slack-page-state-restart (state)
  "Supersede STATE's current generation and return the replacement generation."
  (slack-page-state--discard-waiters
   state (slack-page-state-generation state))
  (setf (slack-page-state-status state)
        (if (slack-page-state-loaded-p state) 'refreshing 'loading)
        (slack-page-state-error state) nil)
  (cl-incf (slack-page-state-generation state)))

(defun slack-page-state-store (state value continuation has-more)
  "Synchronously store VALUE and pagination in STATE as ready data."
  (let ((generation (slack-page-state-restart state)))
    (slack-page-state-commit
     state generation value continuation has-more)))

(defun slack-page-state-commit
    (state generation value continuation has-more &optional defer-ready)
  "Commit VALUE and pagination to STATE for GENERATION.
CONTINUATION is opaque and HAS-MORE records whether another page exists.
When DEFER-READY is non-nil, publish the primary page but remain in flight."
  (when (and (= generation (slack-page-state-generation state))
             (slack-page-state-in-flight-p state)
             (/= generation
                 (slack-page-state-committed-generation state)))
    (setf (slack-page-state-loaded-p state) t
          (slack-page-state-value state) value
          (slack-page-state-continuation state) continuation
          (slack-page-state-has-more state) has-more
          (slack-page-state-error state) nil
          (slack-page-state-updated-at state) (current-time)
          (slack-page-state-committed-generation state) generation)
    (unless defer-ready
      (setf (slack-page-state-status state) 'ready))
    (unwind-protect
        (slack-page-state--notify-commit state)
      (unless defer-ready
        (slack-page-state-ready state generation)))
    t))

(defun slack-page-state-ready (state generation)
  "Finish STATE GENERATION after supplemental hydration."
  (when (and (> generation 0)
             (= generation (slack-page-state-generation state)))
    (cond
     ((= generation (slack-page-state-ready-generation state)) t)
     ((and (or (slack-page-state-in-flight-p state)
               (eq 'ready (slack-page-state-status state)))
           (= generation (slack-page-state-committed-generation state)))
      (setf (slack-page-state-status state) 'ready
            (slack-page-state-ready-generation state) generation)
      (slack-page-state--notify-ready state)
      t))))

(defun slack-page-state-fail (state generation error)
  "Record ERROR on STATE when GENERATION is current."
  (when (and (= generation (slack-page-state-generation state))
             (slack-page-state-in-flight-p state))
    (setf (slack-page-state-status state) 'failed
          (slack-page-state-error state) error)
    (slack-page-state--notify-error state)
    t))

(defun slack-page-state-on-commit (state callback)
  "Run or register CALLBACK for STATE's current primary-page commit."
  (when (functionp callback)
    (cond
     ((and (> (slack-page-state-generation state) 0)
           (= (slack-page-state-committed-generation state)
              (slack-page-state-generation state)))
      (slack-page-state--invoke-callback callback state))
     ((slack-page-state-in-flight-p state)
      (push (cons (slack-page-state-generation state) callback)
            (slack-page-state-commit-waiters state))))))

(defun slack-page-state-on-ready (state callback)
  "Run or register CALLBACK for STATE's current hydrated completion."
  (when (functionp callback)
    (cond
     ((and (> (slack-page-state-generation state) 0)
           (= (slack-page-state-ready-generation state)
              (slack-page-state-generation state)))
      (slack-page-state--invoke-callback callback state))
     ((or (slack-page-state-in-flight-p state)
          (and (eq 'ready (slack-page-state-status state))
               (= (slack-page-state-committed-generation state)
                  (slack-page-state-generation state))
               (/= (slack-page-state-ready-generation state)
                   (slack-page-state-generation state))))
      (push (cons (slack-page-state-generation state) callback)
            (slack-page-state-ready-waiters state))))))

(defun slack-page-state-on-error (state callback)
  "Run or register CALLBACK for STATE's current terminal failure."
  (when (functionp callback)
    (cond
     ((eq 'failed (slack-page-state-status state))
      (slack-page-state--invoke-callback
       callback state (slack-page-state-error state)))
     ((slack-page-state-in-flight-p state)
      (push (cons (slack-page-state-generation state) callback)
            (slack-page-state-error-waiters state))))))

(defun slack-page-state--invoke-callback (callback &rest arguments)
  "Invoke CALLBACK with ARGUMENTS, reporting ordinary callback errors."
  (condition-case callback-error
      (apply callback arguments)
    (error
     (display-warning
      'slack-page-state
      (format "Page-state callback failed: %S" callback-error)
      :error))))

(defun slack-page-state--partition-waiters (waiters generation)
  "Partition WAITERS into GENERATION matches and remaining entries."
  (let (matches remaining)
    (dolist (waiter waiters)
      (if (= generation (car waiter))
          (push waiter matches)
        (push waiter remaining)))
    (cons (nreverse matches) (nreverse remaining))))

(defun slack-page-state--discard-waiters (state generation)
  "Discard every STATE waiter belonging to GENERATION."
  (setf (slack-page-state-commit-waiters state)
        (cdr (slack-page-state--partition-waiters
              (slack-page-state-commit-waiters state) generation))
        (slack-page-state-ready-waiters state)
        (cdr (slack-page-state--partition-waiters
              (slack-page-state-ready-waiters state) generation))
        (slack-page-state-error-waiters state)
        (cdr (slack-page-state--partition-waiters
              (slack-page-state-error-waiters state) generation))))

(defun slack-page-state--notify-commit (state)
  "Run and clear primary-page commit waiters for STATE."
  (let* ((generation (slack-page-state-generation state))
         (parts (slack-page-state--partition-waiters
                 (slack-page-state-commit-waiters state) generation)))
    (setf (slack-page-state-commit-waiters state) (cdr parts))
    (dolist (waiter (car parts))
      (slack-page-state--invoke-callback (cdr waiter) state))))

(defun slack-page-state--notify-ready (state)
  "Run and clear ready waiters for STATE."
  (let* ((generation (slack-page-state-generation state))
         (parts (slack-page-state--partition-waiters
                 (slack-page-state-ready-waiters state) generation)))
    (setf (slack-page-state-ready-waiters state) (cdr parts)
          (slack-page-state-error-waiters state)
          (cdr (slack-page-state--partition-waiters
                (slack-page-state-error-waiters state) generation)))
    (dolist (waiter (car parts))
      (slack-page-state--invoke-callback (cdr waiter) state))))

(defun slack-page-state--notify-error (state)
  "Run and clear error waiters for STATE."
  (let* ((generation (slack-page-state-generation state))
         (terminal-error (slack-page-state-error state))
         (parts (slack-page-state--partition-waiters
                 (slack-page-state-error-waiters state) generation)))
    (setf (slack-page-state-error-waiters state) (cdr parts))
    (slack-page-state--discard-waiters state generation)
    (dolist (waiter (car parts))
      (slack-page-state--invoke-callback
       (cdr waiter) state terminal-error))))

(provide 'slack-page-state)
;;; slack-page-state.el ends here
