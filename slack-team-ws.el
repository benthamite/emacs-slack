;;; slack-team-ws.el ---                             -*- lexical-binding: t; -*-

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

(require 'cl-lib)
(require 'eieio)

(defclass slack-team-ws ()
  ((url :initarg :url)
   (conn :initarg :conn :initform nil)
   (ping-timer :initform nil)
   (check-ping-timeout-timer :initform nil)
   (check-ping-timeout-sec :initarg :check-ping-timeout-sec :initform 20) ;; seconds before declaring a ping lost
   (connected :initform nil)
   (reconnect-auto :initarg :reconnect-auto :initform t)
   (reconnect-timer :initform nil)
   (reconnect-after-sec :initform 2)
   (reconnect-after-sec-base :initform 2) ;; initial backoff; doubles each attempt via `slack-ws-reconnect-backoff'
   (reconnect-after-sec-max :initform 120) ;; 2-minute cap on exponential backoff
   (reconnect-count :initform 0)
   (reconnect-count-max :initarg :reconnect-count-max :initform 360) ;; ~12 hours at max backoff before giving up
   (last-pong :initform nil)
   (waiting-send :initform nil)
   (ping-check-timers :initform (make-hash-table :test 'equal))
   (reconnect-url :initform "" :type string)
   (connect-timeout-timer :initform nil)
   ;; Slack's RTM URL expires after 30 seconds; we timeout at 20 to
   ;; leave margin for the reconnect attempt before expiry.
   (connect-timeout-sec :type number :initform 20)
   (inhibit-reconnection :initform nil)
   (websocket-nowait :initarg :websocket-nowait :initform nil)
   ))

(cl-defmethod slack-ws-cancel-connect-timeout-timer ((ws slack-team-ws))
  "Cancel the pending connect-timeout timer on websocket WS, if any."
  (when (timerp (oref ws connect-timeout-timer))
    (cancel-timer (oref ws connect-timeout-timer))
    (oset ws connect-timeout-timer nil)))

(cl-defmethod slack-ws-set-connect-timeout-timer ((ws slack-team-ws) fn &rest fn-args)
  "Schedule FN with FN-ARGS to fire after the connect-timeout on WS."
  (slack-ws-cancel-connect-timeout-timer ws)
  (oset ws connect-timeout-timer
        (apply #'run-at-time (oref ws connect-timeout-sec)
               nil
               fn fn-args)))

(cl-defmethod slack-ws-cancel-ping-timer ((ws slack-team-ws))
  "Cancel the pending ping timer on websocket WS, if any."
  (with-slots (ping-timer) ws
    (if (timerp ping-timer)
        (cancel-timer ping-timer))
    (setq ping-timer nil)))

(cl-defmethod slack-ws-set-ping-timer ((ws slack-team-ws) fn &rest fn-args)
  "Schedule FN with FN-ARGS to fire 10 seconds later as the ping timer on WS."
  (slack-ws-cancel-ping-timer ws)
  (oset ws ping-timer (apply #'run-at-time 10 nil fn fn-args)))

(cl-defmethod slack-ws-cancel-ping-check-timers ((ws slack-team-ws))
  "Cancel all pending ping-check timers on websocket WS."
  (maphash #'(lambda (_key value)
               (if (timerp value)
                   (cancel-timer value)))
           (oref ws ping-check-timers))
  (oset ws ping-check-timers (make-hash-table :test 'equal)))

(cl-defmethod slack-ws-set-ping-check-timer ((ws slack-team-ws) time fn &rest fn-args)
  "Schedule FN with FN-ARGS as the ping-check timer keyed by TIME on WS."
  (puthash time (apply #'run-at-time
                       (oref ws check-ping-timeout-sec)
                       nil fn fn-args)
           (oref ws ping-check-timers)))

(cl-defmethod slack-ws-cancel-reconnect-timer ((ws slack-team-ws))
  "Cancel the pending reconnect timer on websocket WS, if any."
  (with-slots (reconnect-timer) ws
    (if (timerp reconnect-timer)
        (cancel-timer reconnect-timer))
    (setq reconnect-timer nil)))

(cl-defmethod slack-ws-set-reconnect-timer ((ws slack-team-ws) fn &rest fn-args)
  "Schedule FN with FN-ARGS to fire after the current reconnect delay on WS."
  (slack-ws-cancel-reconnect-timer ws)
  (oset ws reconnect-timer
        (apply #'run-at-time (oref ws reconnect-after-sec)
               nil
               fn fn-args)))

(cl-defmethod slack-ws-reconnect-count-exceed-p ((ws slack-team-ws))
  "Return non-nil when WS has exceeded the maximum reconnect attempts."
  (< (oref ws reconnect-count-max)
     (oref ws reconnect-count)))

(cl-defmethod slack-ws-reconnect-backoff ((ws slack-team-ws))
  "Apply exponential backoff to reconnect delay and return the new delay."
  (let* ((current (oref ws reconnect-after-sec))
         (doubled (min (* current 2) (oref ws reconnect-after-sec-max))))
    (oset ws reconnect-after-sec doubled)
    current))

(cl-defmethod slack-ws-reconnect-reset-backoff ((ws slack-team-ws))
  "Reset reconnect delay to the base value after a successful connection."
  (oset ws reconnect-after-sec (oref ws reconnect-after-sec-base)))

(cl-defmethod slack-ws-inc-reconnect-count ((ws slack-team-ws))
  "Increment the reconnect attempt counter on WS."
  (cl-incf (oref ws reconnect-count)))

(cl-defmethod slack-ws-use-reconnect-url-p ((ws slack-team-ws))
  "Return non-nil when WS has a cached reconnect URL to reuse."
  (< 0 (length (oref ws reconnect-url))))

(provide 'slack-team-ws)
;;; slack-team-ws.el ends here
