;;; slack-request.el --- slack request function       -*- lexical-binding: t; -*-

;; Copyright (C) 2015  南優也

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

(require 'subr-x)
(require 'eieio)
(require 'json)
(require 'mm-util)
(require 'request)
(require 'slack-util)
(require 'slack-log)
(require 'slack-defcustoms)

(defconst slack-request-max-retry 3
  "Maximum number of retries for failed request.")

;; Dynamic token selection cache.
;; Maps URL -> 'team for endpoints that require team tokens.
;; Endpoints not in this table default to the enterprise token.
;; The cache is self-healing: token restriction errors flip the
;; preference and retry the request once.
(defvar slack-token-preference (make-hash-table :test #'equal)
  "Hash table mapping API URL to preferred token type.
Values are the symbol `team' for endpoints requiring team tokens.
Absence means use enterprise token (the default).")

(defun slack-token-preference-init ()
  "Pre-seed the token preference cache with known endpoint preferences.
Called once at load time.  The cache self-corrects at runtime when
Slack returns token restriction errors, so this seed list need not
be exhaustive or up-to-date."
  (dolist (url '("https://slack.com/api/rtm.connect"
                 "https://slack.com/api/conversations.list"
                 "https://slack.com/api/files.getUploadURLExternal"
                 "https://slack.com/api/files.completeUploadExternal"
                 "https://slack.com/api/chat.getPermalink"
                 "https://slack.com/api/conversations.members"
                 "https://slack.com/api/conversations.open"
                 "https://slack.com/api/usergroups.list"
                 "https://slack.com/api/conversations.join"))
    (puthash url 'team slack-token-preference)))

(slack-token-preference-init)

(defun slack-select-token (url team)
  "Select the appropriate token for URL and TEAM.
Uses the enterprise token by default, falling back to the team
token for endpoints recorded in `slack-token-preference'."
  (if-let ((etoken (slack-team-enterprise-token team)))
      (if (eq 'team (gethash url slack-token-preference))
          (slack-team-token team)
        etoken)
    (slack-team-token team)))

(defun slack-token-restriction-error-p (err)
  "Return non-nil if ERR is a token restriction error string."
  (and (stringp err)
       (or (string= err "team_is_restricted")
           (string= err "enterprise_is_restricted"))))

(defcustom slack-request-timeout 30
  "Request Timeout in seconds."
  :type 'integer
  :group 'slack)

(defun slack-need-cookie-p (token)
  "Return non-nil when TOKEN is a browser-style token requiring cookie auth."
  (and token (>= (length token) 4)
       (string= "xoxc" (substring token 0 4))))

(defun slack-url-cookie-store (team)
  "Store required cookies for websocket connection for given TEAM."
  (url-cookie-store "d" (slack-team-d-cookie team) nil ".slack.com" "/" t)
  (url-cookie-store "d-s" (slack-team-d-s-cookie team) nil ".slack.com" "/" t)
  (url-cookie-store "lc" (slack-team-lc-cookie team) nil ".slack.com" "/" t))

(defun slack-parse ()
  "Parse the JSON response body in the current buffer into a plist."
  (json-parse-buffer :object-type 'plist :array-type 'list :false-object :json-false :null-object nil))

(defun slack-request-parse-payload (payload)
  "Issue the `parse PAYLOAD' request against the Slack API."
  (condition-case err-var
      (json-parse-string payload :object-type 'plist :array-type 'list :false-object :json-false :null-object nil)
    (error (message "[Slack] Error on parse JSON: %S, ERR: %S"
                    payload
                    err-var)
           nil)))

(defclass slack-request-request ()
  ((response :initform nil)
   (url :initarg :url)
   (team :initarg :team)
   (type :initarg :type :initform "GET")
   (success :initarg :success)
   (error :initarg :error :initform nil)
   (params :initarg :params :initform nil)
   (data :initarg :data :initform nil)
   (parser :initarg :parser :initform #'slack-parse)
   (sync :initarg :sync :initform nil)
   (files :initarg :files :initform nil)
   (headers :initarg :headers :initform nil)
   (timeout :initarg :timeout :initform nil)
   (execute-at :initarg :execute-at :initform 0.0 :type float)
   (retry-count :initarg :retry-count :initform 0 :type number)
   (no-retry :initarg :no-retry :initform nil :type boolean)
   (without-auth :initarg :without-auth :initform nil :type boolean)
   (token-retried :initform nil :type boolean
                  :documentation "Non-nil if this request already retried with a different token.")))

(cl-defun slack-request-create
    (url team &key type success error params data parser sync files headers (timeout slack-request-timeout) without-auth no-retry)
  "Issue the `create' request against the Slack API.
URL is the url argument.
TEAM is the team argument."
  (let ((args (list
               :url url :team team :type type
               :success success :error error
               :params params :data data :parser parser
               :sync sync :files files :headers headers
               :timeout timeout
               :without-auth without-auth
               :no-retry no-retry))
        (ret nil))
    (mapc #'(lambda (maybe-key)
              (let ((value (plist-get args maybe-key)))
                (when value
                  (setq ret (plist-put ret maybe-key value)))))
          args)
    (apply #'make-instance 'slack-request-request ret)))

(cl-defmethod slack-equalp ((this slack-request-request) other)
  "Return non-nil when the request request equals the OTHER argument.
THIS is the slack-request-request instance."
  (and (slack-equalp (oref this team)
                     (oref other team))
       (string= "GET" (oref this type))
       (string= "GET" (oref other type))
       (string= (oref this url)
                (oref other url))
       (equal (oref this params)
              (oref other params))))

(cl-defmethod slack-request-retry-request ((req slack-request-request) retry-after)
  "Reschedule request REQ to execute after RETRY-AFTER seconds and requeue it."
  (oset req execute-at (+ retry-after (time-to-seconds)))
  (oset req retry-count (1+ (oref req retry-count)))
  (oset req response nil)
  (slack-request-worker-push req))

(cl-defmethod slack-request-retry-failed-request-p ((req slack-request-request) error-thrown symbol-status)
  "Return non-nil when REQ should be retried given ERROR-THROWN and SYMBOL-STATUS."
  (with-slots (no-retry type retry-count) req
    (and (not no-retry)
         (or (zerop slack-request-max-retry)
             (< retry-count slack-request-max-retry))
         (and (string= type "GET")
              (or (and error-thrown
                       (eq 'end-of-file (car error-thrown)))
                  (eq symbol-status 'timeout))))))

(cl-defmethod slack-request-log-failed-retry ((req slack-request-request) error-thrown symbol-status data)
  "Log that REQ will be retried after failing with ERROR-THROWN, SYMBOL-STATUS, DATA."
  (with-slots (url params team retry-count) req
    (slack-log (format "Retry Request by Error. URL: %S, PARAMS: %S, ERROR-THROWN: %S, SYMBOL-STATUS: %S, DATA: %S, COUNT: %d"
                       url
                       params
                       error-thrown
                       symbol-status
                       data
                       retry-count)
               team :level 'warn)))

(cl-defmethod slack-request-log-retry ((req slack-request-request) retry-after-sec)
  "Log that REQ will be retried after RETRY-AFTER-SEC seconds."
  (with-slots (url params team) req
    (slack-log (format "Retrying Request After: %s second, URL: %s, PARAMS: %s"
                       retry-after-sec
                       url
                       params)
               team)))

(cl-defmethod slack-request-log-failed ((req slack-request-request) error-thrown symbol-status data)
  "Log that REQ failed, reporting ERROR-THROWN, SYMBOL-STATUS, and DATA."
  (with-slots (url params team) req
    (slack-log (format "REQUEST FAILED. URL: %S, PARAMS: %S, ERROR-THROWN: %S, SYMBOL-STATUS: %S, DATA: %S"
                       url
                       params
                       error-thrown
                       symbol-status
                       data)
               team :level 'error)))

(cl-defmethod slack-request-log-success ((req slack-request-request) data)
  "Log that REQ finished successfully with response DATA at trace level."
  (with-slots (url params team) req
    (slack-log (format "REQUEST FINISHED. URL: %S, PARAMS: %S, DATA: %S"
                       url
                       params
                       data)
               team :level 'trace)))

(cl-defmethod slack-request ((req slack-request-request) &key (on-success nil) (on-error nil))
  "Execute request REQ, retrying and handling token errors as needed.
ON-SUCCESS and ON-ERROR, when functions, are invoked after the
request's own success and error handlers run."
  (let* (;; we don't want to save cookies because they break switching teams using the trick from  https://github.com/tkf/emacs-request/issues/155
         (request-curl-options slack-request-curl-options)
         (temp-cookie (unless (oref req sync)
                        (expand-file-name (make-temp-name "my-cookie-")
                                          temporary-file-directory)))
         (request--curl-cookie-jar (or temp-cookie request--curl-cookie-jar))
         (team (oref req team)))
    (cl-labels
        ((cleanup-temp-cookie ()
           (when (and temp-cookie (file-exists-p temp-cookie))
             (delete-file temp-cookie)))
         (-on-success (&key data &allow-other-keys)
           (let ((err (and (eq (plist-get data :ok) :json-false)
                           (plist-get data :error))))
             (if (and err
                      (slack-token-restriction-error-p err)
                      (not (oref req token-retried))
                      (slack-team-enterprise-token team))
                 ;; Token mismatch: flip preference and retry once
                 (progn
                   (if (string= err "team_is_restricted")
                       (remhash (oref req url) slack-token-preference)
                     (puthash (oref req url) 'team slack-token-preference))
                   (slack-log (format "Token restriction (%s) for %s, retrying"
                                      err (oref req url))
                              team :level 'info)
                   (oset req token-retried t)
                   (oset req response nil)
                   (slack-request req :on-success on-success :on-error on-error))
               (unwind-protect
                   (progn
                     (funcall (oref req success) :data data)
                     (slack-request-log-success req data))
                 (cleanup-temp-cookie)
                 (when (functionp on-success)
                   (funcall on-success))))))
         (-on-error (&key error-thrown symbol-status response data)
           (unwind-protect
               (progn
                 (slack-if-let* ((retry-after (request-response-header response "retry-after"))
                                 (retry-after-sec (string-to-number retry-after)))
                     (progn
                       (slack-request-retry-request req retry-after-sec)
                       (slack-request-log-retry req retry-after-sec))
                   (slack-request-log-failed req error-thrown symbol-status data)
                   (if (slack-request-retry-failed-request-p req error-thrown symbol-status)
                       (progn
                         (slack-request-log-failed-retry req error-thrown symbol-status data)
                         (slack-request-retry-request req 1))
                     (when (functionp (oref req error))
                       (funcall (oref req error)
                                :error-thrown error-thrown
                                :symbol-status symbol-status
                                :response response
                                :data data)))))
             (cleanup-temp-cookie)
             (when (functionp on-error)
               (funcall on-error)))))
      (with-slots (url type params data parser sync files headers timeout without-auth) req
        (oset req response
              (request
                url
                :type type
                :sync sync
                :params params
                :data data
                :files files
                :headers
                (append
                 (if without-auth nil
                   (list (cons "Authorization"
                               (format "Bearer %s"
                                       (slack-select-token url team)))))
                 (when (slack-need-cookie-p (slack-team-token team))
                   (list (cons "Cookie" (format "d=%s; " (slack-team-cookie team)))))
                 headers)
                :parser parser
                :success (unless sync #'-on-success)
                :error (unless sync #'-on-error)
                :timeout timeout))
        req))))


(cl-defmacro slack-request-handle-error ((data req-name &optional handler) &body body)
  "Bind error to e if present in DATA.
BODY is the body argument."
  `(if (eq (plist-get ,data :ok) :json-false)
       (if ,handler
           (funcall ,handler (plist-get ,data :error))
         (message "Failed to request %s: %s"
                  ,req-name
                  (plist-get ,data :error)))
     (progn
       ,@body)))

;; Request Worker

(defcustom slack-request-worker-max-request-limit 30
  "Max request count perform simultaneously."
  :type 'integer
  :group 'slack)

(defvar slack-request-worker-instance nil)

(defclass slack-request-worker ()
  ((queue :initform '() :type list)
   (timer :initform nil)
   (current-request-count :initform 0 :type integer)
   ))

(defun slack-request-worker-create ()
  "Create `slack-request-worker' instance."
  (make-instance 'slack-request-worker))

(cl-defmethod slack-request-worker-push ((this slack-request-worker) req)
  "Enqueue the request worker on the shared request worker.
THIS is the slack-request-worker instance.
REQ is the req argument."
  (cl-pushnew req (oref this queue) :test #'slack-equalp))

(defun slack-request-worker-on-timeout ()
  "Issue the `worker on timeout' request against the Slack API."
  (slack-request-worker-execute)
  (when (and slack-request-worker-instance
             (timerp (oref slack-request-worker-instance timer)))
    (cancel-timer (oref slack-request-worker-instance timer))
    (oset slack-request-worker-instance timer nil))
  (slack-request-worker-set-timer))

(defun slack-request-worker-set-timer ()
  "Issue the `worker set timer' request against the Slack API."
  (oset slack-request-worker-instance
        timer
        (run-at-time 1 nil #'slack-request-worker-on-timeout)))

(defun slack-request-worker-execute ()
  "Pop requests from queue and execute.
Stop after `slack-request-worker-max-request-limit'."
  (when slack-request-worker-instance
    (let ((do '())
          (skip '())
          (current (time-to-seconds))
          (limit (- slack-request-worker-max-request-limit
                    (oref slack-request-worker-instance
                          current-request-count))))

      (cl-loop for req in (reverse (oref slack-request-worker-instance queue))
               do (if (and (< (oref req execute-at) current)
                           (< (length do) limit))
                      (push req do)
                    (push req skip)))

      ;; (let ((current (oref slack-request-worker-instance current-request-count))
      ;;       (queue (length (oref slack-request-worker-instance queue)))
      ;;       (max slack-request-worker-max-request-limit))
      ;;   (message "[WORKER] QUEUE: %s, LIMIT: %s, CURRENT: %s, DO: %s, SKIP: %s"
      ;;            queue
      ;;            max
      ;;            current
      ;;            (length do)
      ;;            (length skip)))

      (oset slack-request-worker-instance queue skip)

      (cl-labels
          ((decl-request-count
            ()
            (cl-decf (oref slack-request-worker-instance
                           current-request-count))))
        (cl-loop for req in do
                 do (progn
                      (cl-incf (oref slack-request-worker-instance
                                     current-request-count))
                      (slack-request req
                                     :on-success #'decl-request-count
                                     :on-error #'decl-request-count
                                     )))))))

(cl-defmethod slack-request-worker-push ((req slack-request-request))
  "Enqueue the request request on the shared request worker.
REQ is the req argument."
  (unless slack-request-worker-instance
    (setq slack-request-worker-instance
          (slack-request-worker-create)))
  (slack-request-worker-push slack-request-worker-instance req)
  (unless (oref slack-request-worker-instance timer)
    (slack-request-worker-set-timer)))

(defun slack-request-worker-quit ()
  "Cancel timer and remove `slack-request-worker-instance'."
  (when (and slack-request-worker-instance
             (timerp (oref slack-request-worker-instance timer)))
    (cancel-timer (oref slack-request-worker-instance timer)))
  (setq slack-request-worker-instance nil))

(defun slack-request-worker-remove-request (team)
  "Remove request from TEAM in queue."
  (when slack-request-worker-instance
    (let ((to-remove '())
          (new-queue '())
          (all (oref slack-request-worker-instance queue)))

      (dolist (req all)
        (if (slack-equalp (oref req team) team)
            (push req to-remove)
          (push req new-queue)))

      (dolist (req to-remove)
        (when (and (oref req response)
                   (not (request-response-done-p (oref req response))))
          (request-abort (oref req response))))

      (oset slack-request-worker-instance queue new-queue)
      (slack-log (format "Remove Request from Worker, ALL: %s, REMOVED: %s, NEW-QUEUE: %s"
                         (length all)
                         (length to-remove)
                         (length new-queue))
                 team :level 'debug))))

(cl-defun slack-url-copy-file (url newname team &key (success nil) (error nil) (sync nil) (token nil) (cookie nil))
  "Download URL to NEWNAME for TEAM, using curl when available.
SUCCESS and ERROR are callbacks.  SYNC forces a blocking request.
TOKEN and COOKIE supply auth credentials for Slack-hosted URLs."
  (if (executable-find "curl")
      (slack-curl-downloader url newname team
                             :success success
                             :error error
                             :token token
                             :cookie cookie)
    (cl-labels
        ((on-success (&key _data &allow-other-keys)
           (when (functionp success) (funcall success)))
         (on-error (&key error-thrown symbol-status response _data)
           (message "Error Download File: %s %s %s, url: %s"
                    (request-response-status-code response)
                    error-thrown symbol-status url)
           (if (file-exists-p newname)
               (delete-file newname))
           (cl-case (request-response-status-code response)
             (403 nil)
             (404 nil)
             (t (when (functionp error)
                  (funcall error
                           (request-response-status-code response)
                           error-thrown
                           symbol-status
                           url)))))
         (parser () (mm-write-region (point-min) (point-max)
                                     newname nil nil nil 'binary t)))
      (let* ((url-obj (url-generic-parse-url url))
             (need-token-p (and url-obj
                                (string-match-p "slack"
                                                (url-host url-obj))))
             (use-https-p (and url-obj
                               (string= "https" (url-type url-obj)))))
        (request
          url
          :success #'on-success
          :error #'on-error
          :parser #'parser
          :sync sync
          :headers (when (and token use-https-p need-token-p)
                     (append
                      (list (cons "Authorization" (format "Bearer %s" token)))
                      (when (slack-need-cookie-p token)
                        (list (cons "Cookie" (format "d=%s; " cookie)))))))))))

(cl-defun slack-curl-downloader (url name team &key (success nil) (error nil) (token nil) (cookie nil))
  "Download URL to NAME asynchronously with curl, logging to TEAM on failure.
SUCCESS and ERROR are callbacks; TOKEN and COOKIE supply Slack auth."
  (cl-labels
      ((sentinel (proc event)
         (cond
          ((string-equal "finished\n" event)
           (when (functionp success) (funcall success)))
          (t
           (let ((status (process-status proc))
                 (output (with-current-buffer (process-buffer proc)
                           (buffer-substring-no-properties (point-min)
                                                           (point-max)))))
             (if (functionp error)
                 (funcall error status output url name)
               (slack-log
                (format "slack-curl-downloader: Download Failed. STATUS: %s, EVENT: %s, URL: %s, NAME: %s, OUTPUT: %s"
                        status
                        event
                        url
                        name
                        output)
                team
                :level 'warn))
             (if (file-exists-p name)
                 (delete-file name))
             (delete-process proc))))))
    (let* ((url-obj (url-generic-parse-url url))
           (need-token-p (and url-obj
                              token
                              (string-match-p "slack" (url-host url-obj))
                              (string-prefix-p "https" url)))
           (proc (apply #'start-process
                        "slack-curl-downloader"
                        "slack-curl-downloader"
                        (executable-find "curl")
                        "--silent"
                        "--show-error"
                        "--fail"
                        "--location"
                        "--output" name
                        "--url" url
                        (append
                         (when need-token-p
                           `("-H" ,(format "Authorization: Bearer %s" token)))
                         (when (and need-token-p (slack-need-cookie-p token))
                           `("-H" ,(format "Cookie: d=%s; " cookie)))))))
      (set-process-sentinel proc #'sentinel))))

(provide 'slack-request)
;;; slack-request.el ends here
