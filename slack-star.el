;;; slack-stars.el ---                               -*- lexical-binding: t; -*-

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
(require 'slack-request)
(require 'slack-team)
(require 'slack-file)
(require 'slack-buffer)
(require 'slack-create-message)
(require 'slack-room)

;; Stars were renamed to "saved items" in July 2023.
;; https://api.slack.com/changelog/2023-07-its-later-already-for-stars-and-reminders
;; Internal names retain "star" for compatibility; the API uses saved.list.
(defconst slack-stars-list-url "https://slack.com/api/saved.list")

(defclass slack-star ()
  ((cursor :initarg :cursor :type (or null string) :initform nil)
   (items :initarg :items :type (or null list) :initform nil)))

(defcustom slack-star-mutation-journal-limit 256
  "Maximum idle saved-item mutation entries retained per team.
Entries needed by an active saved-list request are retained until that
request consumes or releases them, then the journal returns to this
bound."
  :type 'integer
  :group 'slack)

(defclass slack-star-mutation-journal ()
  ((sequence :initform 0 :type integer)
   (entries :initform nil :type list)
   (active-tokens :initform nil :type list)
   (pending-writes :initform nil :type list)))

(defclass slack-star-mutation-token ()
  ((journal :initarg :journal :type slack-star-mutation-journal)
   (sequence :initarg :sequence :type integer)
   (pending-writes :initarg :pending-writes :type list)))

(defclass slack-star-pending-write ()
  ((state :initform 'pending :type symbol)
   (rollbacks :initform nil :type list)))

(defvar slack-star--current-pending-write nil
  "Saved-item write whose optimistic mutation is currently being applied.")

(defvar slack-star--mutation-journals
  (make-hash-table :test #'eq :weakness 'key)
  "Weak mapping from Slack teams to their saved-item mutation journals.")

(defclass slack-star-item ()
  ((item-id :initarg :item-id :type string)
   (item-type :initarg :item-type :type string)
   (date-created :initarg :date-created :type integer)
   (date-due :initarg :date-due :type integer)
   (date-completed :initarg :date-completed :type integer)
   (date-updated :initarg :date-updated :type integer)
   (is-archived :initarg :is-archived :type boolean)
   (date-snoozed-until :initarg :date-snoozed-until :type integer)
   (ts :initarg :ts :type string)
   (thread-ts :initarg :thread-ts :type (or null string) :initform nil)
   (state :initarg :state :type string)
   (file :initarg :file :type (or null slack-file) :initform nil)))


(cl-defmethod slack-ts ((this slack-star-item))
  "Get THIS star item timestamp."
  (oref this ts))

(cl-defmethod slack-star-has-next-page-p ((this slack-star))
  "Are there more saved for later for THIS?
Slack's cursor pagination returns an empty next_cursor on the last
page, so only a non-empty cursor means another page exists."
  (< 0 (length (oref this cursor))))

(cl-defmethod slack-star-items ((this slack-star))
  "GET THIS star items."
  (oref this items))

(cl-defmethod slack-merge ((old slack-star) new)
  "Append NEW page of saved items to OLD, updating cursor."
  (with-slots (cursor items) old
    (setq cursor (oref new cursor))
    (setq items (append items (oref new items)))))

(defun slack-create-star-items (payload)
  "Create and return a new star items instance from PAYLOAD."
  (mapcar #'(lambda (e) (slack-create-star-item e))
          payload))

(defun slack-create-star-item (payload)
  "Make `slack-star-item' from PAYLOAD."
  (make-instance 'slack-star-item
                 :item-id (or (plist-get payload :item_id)
                              (plist-get payload :channel)
                              (when-let ((file (plist-get payload :file)))
                                (if (eieio-object-p file)
                                    (oref file id)
                                  (plist-get file :id)))
                              "")
                 :item-type (or (plist-get payload :item_type)
                                (plist-get payload :type)
                                "")
                 :date-created (or (plist-get payload :date_created) 0)
                 :date-due (or (plist-get payload :date_due) 0)
                 :date-completed (or (plist-get payload :date_completed) 0)
                 :date-updated (or (plist-get payload :date_updated) 0)
                 :is-archived (when-let ((e (plist-get payload :is_archived))) (not (equal e :json-false)))
                 :date-snoozed-until (or (plist-get payload :date_snoozed_until) 0)
                 :ts (or (plist-get payload :ts)
                         (when-let ((message (plist-get payload :message)))
                           (plist-get message :ts))
                         (when-let ((file (plist-get payload :file)))
                           (if (eieio-object-p file)
                               (slack-ts file)
                             (when-let ((created (plist-get file :created)))
                               (number-to-string created))))
                         "")
                 :thread-ts (or (plist-get payload :thread_ts)
                                (when-let ((message (plist-get payload :message)))
                                  (plist-get message :thread_ts)))
                 :state (or (plist-get payload :state) "")
                 :file (when-let ((file (plist-get payload :file)))
                         (if (eieio-object-p file)
                             file
                           (slack-file-create file)))))

(cl-defmethod slack-star-item-file ((item slack-star-item) file-id)
  "Return the file of starred ITEM when its id equals FILE-ID."
  (let ((file (oref item file)))
    (when (and file
               (string= (oref file id) file-id))
      file)))

(defun slack-create-star (payload)
  "Make star items from PAYLOAD."
  (let ((items (slack-create-star-items (plist-get payload :saved_items)))
        (cursor (plist-get (plist-get payload :response_metadata) :next_cursor)))
    (make-instance 'slack-star
                   :items items
                   :cursor cursor)))

(defun slack-star-item-key (item)
  "Return the stable identity key for saved ITEM."
  (list (oref item item-type) (oref item item-id) (slack-ts item)))

(defun slack-star--mutation-journal (team)
  "Return TEAM's saved-item mutation journal, creating it when absent."
  (or (gethash team slack-star--mutation-journals)
      (let ((journal (make-instance 'slack-star-mutation-journal)))
        (puthash team journal slack-star--mutation-journals)
        journal)))

(defun slack-star-mutation-journal--prune (journal)
  "Prune obsolete entries from saved-item mutation JOURNAL.
The idle history is bounded by `slack-star-mutation-journal-limit'.
Entries needed by an active list request or an unacknowledged write remain
protected; only older diagnostic history is pruned."
  (let* ((entries (oref journal entries))
         (tokens (oref journal active-tokens))
         (oldest-active
          (when tokens
            (apply #'min
                   (mapcar (lambda (token) (oref token sequence)) tokens))))
         (protected-writes
          (append
           (oref journal pending-writes)
           (cl-mapcan
            (lambda (token) (copy-sequence (oref token pending-writes)))
            tokens)))
         (required
          (cl-remove-if-not
           (lambda (entry)
             (or (and oldest-active
                      (> (plist-get entry :sequence) oldest-active))
                 (memq (plist-get entry :write) protected-writes)))
           entries))
         (optional
          (cl-set-difference entries required :test #'eq))
         (remaining
          (max 0 (- slack-star-mutation-journal-limit
                    (length required))))
         (kept-optional
          (cl-subseq optional 0 (min remaining (length optional)))))
    (oset journal entries (append required kept-optional))))

(defun slack-star-mutation-journal-register (team)
  "Register a saved-list request for TEAM and return its replay token."
  (let* ((journal (slack-star--mutation-journal team))
         (token
          (make-instance
           'slack-star-mutation-token
           :journal journal
           :sequence (oref journal sequence)
           :pending-writes (copy-sequence (oref journal pending-writes)))))
    (push token (oref journal active-tokens))
    token))

(defun slack-star-mutation-journal-release (team token)
  "Release TEAM's saved-list request TOKEN and prune its journal."
  (let ((journal (slack-star--mutation-journal team)))
    (when (and (object-of-class-p token 'slack-star-mutation-token)
               (eq journal (oref token journal)))
      (oset journal active-tokens
            (delq token (oref journal active-tokens)))
      (slack-star-mutation-journal--prune journal))))

(defun slack-star-mutation-journal-entries-since (team token)
  "Return TEAM's saved-item mutations after TOKEN in occurrence order."
  (let ((journal (slack-star--mutation-journal team)))
    (unless (and (object-of-class-p token 'slack-star-mutation-token)
                 (eq journal (oref token journal)))
      (error "Saved-item mutation token belongs to another team"))
    (nreverse
     (cl-remove-if-not
      (lambda (entry)
        (let ((write (plist-get entry :write)))
          (and (not (and write (eq 'failed (oref write state))))
               (or (> (plist-get entry :sequence) (oref token sequence))
                   (memq write (oref token pending-writes))))))
      (copy-sequence (oref journal entries))))))

(defun slack-star-mutation-journal-size (team)
  "Return the number of retained saved-item mutations for TEAM."
  (length (oref (slack-star--mutation-journal team) entries)))

(defun slack-star-mutation-journal--record (team operation &rest properties)
  "Record TEAM's saved-item OPERATION with entry PROPERTIES."
  (let* ((journal (slack-star--mutation-journal team))
         (sequence (1+ (oref journal sequence)))
         (entry (append (list :sequence sequence
                              :operation operation
                              :write slack-star--current-pending-write)
                        properties)))
    (oset journal sequence sequence)
    (push entry (oref journal entries))
    (slack-star-mutation-journal--prune journal)
    entry))

(defun slack-star-mutation-journal-record-add (team item)
  "Record that saved ITEM was added to TEAM."
  (slack-star-mutation-journal--record team 'add :item item))

(defun slack-star-mutation-journal-record-remove
    (team ts &optional item-type item-id)
  "Record removal of TEAM's saved item identified by TS.
When ITEM-TYPE and ITEM-ID are non-nil, retain that exact identity so replay
does not remove a same-timestamp item from another conversation."
  (slack-star-mutation-journal--record
   team 'remove :ts ts :item-type item-type :item-id item-id))

(defun slack-star-item-matches-p (item ts &optional item-type item-id)
  "Return non-nil when saved ITEM matches TS and optional identity fields.
Omitted ITEM-TYPE or ITEM-ID acts as a wildcard for legacy timestamp-only
callers."
  (and (string= (slack-ts item) ts)
       (or (null item-type)
           (string= (oref item item-type) item-type))
       (or (null item-id)
           (string= (oref item item-id) item-id))))

(defun slack-star--add-item (items item)
  "Return ITEMS with saved ITEM at the front and no identity duplicate."
  (let ((key (slack-star-item-key item)))
    (cons item
          (cl-remove-if
           (lambda (existing)
             (equal key (slack-star-item-key existing)))
           items))))

(defun slack-star--apply-mutations (star entries &optional removals-only)
  "Apply saved-item ENTRIES to STAR in occurrence order.
When REMOVALS-ONLY is non-nil, suppress stale page rows without
duplicating live additions already present in the combined cache."
  (let ((items (copy-sequence (slack-star-items star))))
    (dolist (entry entries)
      (pcase (plist-get entry :operation)
        ('add
         (unless removals-only
           (setq items
                 (slack-star--add-item items (plist-get entry :item)))))
        ('remove
         (let ((ts (plist-get entry :ts))
               (item-type (plist-get entry :item-type))
               (item-id (plist-get entry :item-id)))
           (setq items
                 (cl-remove-if
                  (lambda (item)
                    (slack-star-item-matches-p
                     item ts item-type item-id))
                  items))))))
    (oset star items items)
    star))

(defun slack-star--append-unique-items (first second)
  "Return saved items from FIRST followed by new identities in SECOND."
  (let ((seen (make-hash-table :test #'equal))
        result)
    (dolist (item (append first second))
      (let ((key (slack-star-item-key item)))
        (unless (gethash key seen)
          (puthash key t seen)
          (push item result))))
    (nreverse result)))

(defun slack-star-create-embedded-messages (payload team)
  "Create saved-list message candidates from PAYLOAD for TEAM.
Return `(ROOM . MESSAGE)' pairs without mutating either cache."
  (cl-loop
   for item in (plist-get payload :saved_items)
   for candidate =
   (when-let* ((message-payload (plist-get item :message))
               (room-id (or (plist-get item :item_id)
                            (plist-get item :channel)
                            (plist-get message-payload :channel)))
               (room (slack-room-find room-id team))
               (message (slack-message-create
                         (copy-sequence message-payload) team room)))
     (cons room message))
   when candidate collect candidate))

(defun slack-star-cache-embedded-messages (candidates team)
  "Atomically publish saved-list message CANDIDATES to TEAM's room caches.
These are historical cache fills, so they do not advance TEAM's unread-count
latest markers.  Build every replacement cache before changing any room."
  (ignore team)
  (let (updates)
    (dolist (candidate candidates)
      (let* ((room (car candidate))
             (message (cdr candidate))
             (update
              (or (cl-find-if
                   (lambda (value) (eq room (plist-get value :room)))
                   updates)
                  (let ((value
                         (list
                          :room room
                          :messages (copy-hash-table (oref room messages))
                          :message-ids (copy-sequence (oref room message-ids))
                          :message-revision (oref room message-revision)
                          :message-revisions
                          (copy-hash-table (oref room message-revisions)))))
                    (push value updates)
                    value)))
             (ts (slack-ts message))
             (revision (1+ (plist-get update :message-revision))))
        (puthash ts message (plist-get update :messages))
        (cl-pushnew ts (plist-get update :message-ids) :test #'string=)
        (setf (plist-get update :message-revision) revision)
        (puthash ts revision (plist-get update :message-revisions))))
    (dolist (update updates)
      (setf (plist-get update :message-ids)
            (cl-sort (plist-get update :message-ids) #'string<)))
    (dolist (update updates)
      (let ((room (plist-get update :room)))
        (oset room messages (plist-get update :messages))
        (oset room message-ids (plist-get update :message-ids))
        (oset room message-revision
              (plist-get update :message-revision))
        (oset room message-revisions
              (plist-get update :message-revisions))))))

(defun slack-star-user-ids (star team &optional candidates)
  "Return user IDs referenced by saved STAR's renderable items on TEAM.
CANDIDATES are unpublished `(ROOM . MESSAGE)' pairs from the same response."
  (cl-remove-if-not
   #'stringp
   (cl-loop
    for item in (slack-star-items star)
    nconc
    (cond
     ((oref item file)
      (slack-message-user-ids (oref item file)))
     (t
      (when-let* ((room (slack-room-find (oref item item-id) team))
                  (message
                   (or
                    (cdr
                     (cl-find-if
                      (lambda (candidate)
                        (and (eq room (car candidate))
                             (string= (slack-ts item)
                                      (slack-ts (cdr candidate)))))
                      candidates))
                    (slack-room-find-message room (slack-ts item)))))
        (slack-message-user-ids message)))))))

(defun slack-stars-list-request
    (team &optional cursor after-success on-error on-primary-page)
  "Fetch TEAM's saved items from CURSOR.
AFTER-SUCCESS runs after supplemental user hydration.  ON-ERROR
runs for API, transport, response-normalization, or cache failures.
ON-PRIMARY-PAGE receives the response's `slack-star' page and TEAM's stored,
combined star cache after embedded messages and that cache are stored, but
before user hydration begins.
The first four argument positions remain compatible with older callers."
  (let ((primary-called-p nil)
        (journal-token (slack-star-mutation-journal-register team)))
    (cl-labels
      ((release-journal ()
         (when journal-token
           (slack-star-mutation-journal-release team journal-token)
           (setq journal-token nil)))
       (callback ()
         (when (functionp after-success)
           (funcall after-success)))
       (fail (&rest args)
         (release-journal)
         (when (functionp on-error)
           (apply on-error args)))
       (on-success (&key data &allow-other-keys)
         (unwind-protect
             (slack-request-handle-error
              (data "slack-stars-list-request" #'fail)
              (let ((normalized
                     (slack-request-normalize-response
                      (lambda ()
                        (let* ((star (slack-create-star data))
                               (current-star (oref team star))
                               (candidates
                                (slack-star-create-embedded-messages data team))
                               (mutations
                                (slack-star-mutation-journal-entries-since
                                 team journal-token))
                               stored-star)
                          (if cursor
                              (progn
                                (slack-star--apply-mutations star mutations t)
                                (setq stored-star
                                      (make-instance
                                       'slack-star
                                       :items
                                       (slack-star--append-unique-items
                                        (and current-star
                                             (slack-star-items current-star))
                                        (slack-star-items star))
                                       :cursor (oref star cursor))))
                            (slack-star--apply-mutations star mutations)
                            (setq stored-star star))
                          (let ((user-ids
                                 (slack-team-missing-user-ids
                                  team
                                  (slack-star-user-ids
                                   stored-star team candidates))))
                            ;; Publish only after the complete response and its
                            ;; supplemental identity set have normalized.
                            (slack-star-cache-embedded-messages candidates team)
                            (oset team star stored-star)
                            (list star stored-star user-ids))))
                      #'fail)))
                (when normalized
                  (pcase-let ((`(,page ,stored-star ,user-ids)
                               (cdr normalized)))
                    (when (and (not primary-called-p)
                               (functionp on-primary-page))
                      (setq primary-called-p t)
                      (funcall on-primary-page page stored-star))
                    (if (< 0 (length user-ids))
                        (slack-users-info-request
                         user-ids team
                         :after-success (lambda () (callback)))
                      (callback))))))
           (release-journal))))
    (condition-case request-error
        (slack-request
         (slack-request-create
          slack-stars-list-url
          team
          :type "POST"
          :data (list (when cursor (cons "cursor" cursor)))
          :success #'on-success
          :error (lambda (&rest args) (apply #'fail args))))
      (error
       (release-journal)
       (signal (car request-error) (cdr request-error)))))))

(defun slack-star-api-request (url params team &optional optimistic-change)
  "Send a saved-item request to URL with PARAMS for TEAM.
OPTIMISTIC-CHANGE, when non-nil, runs before the request starts.  Its saved
cache mutation remains replayable until the write reaches a terminal outcome."
  (let ((write
         (when optimistic-change
           (slack-star-pending-write-start team))))
    (cl-labels
        ((fail-write ()
           (when write
             (slack-star-pending-write-fail team write)))
         (api-failed (error)
           (fail-write)
           (message "Failed to request %s: %s" url error))
         (on-success (&key data &allow-other-keys)
           (slack-request-handle-error
            (data url #'api-failed)
            (when write
              (slack-star-pending-write-succeed team write)))))
      (condition-case request-error
          (progn
            (when write
              (let ((slack-star--current-pending-write write))
                (let ((rollback (funcall optimistic-change)))
                  (when (functionp rollback)
                    (slack-star-pending-write-add-rollback
                     write rollback)))))
            (slack-request
             (slack-request-create
              url
              team
              :params params
              :success #'on-success
              :error (when write (lambda (&rest _errors) (fail-write))))))
        (error
         (fail-write)
         (signal (car request-error) (cdr request-error)))))))

(cl-defmethod slack-star-remove-star
  ((this slack-star) ts team &optional item-type item-id optimistic-change)
  "Remove from THIS stars the saved item at TS for TEAM.
Optional ITEM-TYPE and ITEM-ID disambiguate equal timestamps.
OPTIMISTIC-CHANGE is forwarded to `slack-star-api-request'."
  (slack-if-let* ((item
                   (--find
                    (slack-star-item-matches-p it ts item-type item-id)
                    (oref this items))))
      (slack-star-api-request slack-message-stars-remove-url
                              (list (cons "ts" ts)
                                    (cons "item_id" (oref item item-id))
                                    (cons "item_type" (oref item item-type)))
                              team optimistic-change)
    (error "Could not find star to remove for ts")))

(defun slack-star--contains-ts-p (star ts &optional item-type item-id)
  "Return non-nil when STAR contains TS and optional saved-item identity."
  (cl-some (lambda (item)
             (slack-star-item-matches-p item ts item-type item-id))
           (slack-star-items star)))

(defun slack-ts-saved-p (team ts &optional item-type item-id)
  "Return non-nil when a saved item with TS exists in TEAM's saved list.
Returns nil when TEAM has not loaded its saved items list yet, so
callers must treat nil as \"unknown or not saved\", not \"definitely
not saved\".  Optional ITEM-TYPE and ITEM-ID disambiguate equal timestamps."
  (when-let* ((star (oref team star)))
    (slack-star--contains-ts-p star ts item-type item-id)))

(defun slack-team-mark-saved (team channel ts)
  "Record that the message at TS in CHANNEL is saved for TEAM.
Initializes TEAM's saved items list when it has not been loaded, so
subsequent saved-state checks work even for users who have never
opened the saved items buffer."
  (slack-team-mark-saved-item
   team
   (slack-create-star-item
    (list :item_id channel :item_type "message" :ts ts))))

(defun slack-team-mark-saved-item (team item)
  "Record that normalized saved ITEM was added to TEAM.
This initializes an empty cache when necessary and records the
mutation independently so an older in-flight saved-list response
cannot erase it."
  (unless (oref team star)
    (oset team star (make-instance 'slack-star :items nil :cursor nil)))
  (let* ((star (oref team star))
         (key (slack-star-item-key item))
         (previous
          (cl-find-if
           (lambda (existing)
             (equal key (slack-star-item-key existing)))
           (slack-star-items star)))
         (entry (slack-star-mutation-journal-record-add team item)))
    (oset star items
          (slack-star--add-item (slack-star-items star) item))
    (when slack-star--current-pending-write
      (slack-star-pending-write-add-rollback
       slack-star--current-pending-write
       (lambda ()
         (slack-star--rollback-add team entry item previous))))
    entry))

(defun slack-team-mark-unsaved (team ts &optional item-type item-id)
  "Record that the message at TS is no longer saved for TEAM.
The removal is journaled even when TEAM's saved cache is empty, so
an older in-flight response cannot reintroduce the item.  Optional ITEM-TYPE
and ITEM-ID scope the removal; omitted identity preserves the legacy
timestamp-wide match."
  (let* ((star (oref team star))
         (removed
          (and star
               (cl-remove-if-not
                (lambda (item)
                  (slack-star-item-matches-p item ts item-type item-id))
                (slack-star-items star))))
         (entry
          (slack-star-mutation-journal-record-remove
           team ts item-type item-id)))
    (when star
      (oset star items
            (cl-remove-if
             (lambda (item)
               (slack-star-item-matches-p item ts item-type item-id))
             (slack-star-items star))))
    (when slack-star--current-pending-write
      (slack-star-pending-write-add-rollback
       slack-star--current-pending-write
       (lambda ()
         (slack-star--rollback-remove team entry removed))))
    entry))

(defun slack-star-pending-write-start (team)
  "Create and retain an unacknowledged saved-item write for TEAM."
  (let* ((journal (slack-star--mutation-journal team))
         (write (make-instance 'slack-star-pending-write)))
    (push write (oref journal pending-writes))
    write))

(defun slack-star-pending-write-add-rollback (write rollback)
  "Register ROLLBACK for pending saved-item WRITE."
  (oset write rollbacks
        (append (oref write rollbacks) (list rollback))))

(defun slack-star-pending-write-succeed (team write)
  "Record successful acknowledgement of TEAM's saved-item WRITE."
  (when (eq 'pending (oref write state))
    (oset write state 'succeeded)
    (slack-star-pending-write-finish team write)))

(defun slack-star-pending-write-fail (team write)
  "Rollback TEAM's failed saved-item WRITE and record its terminal state."
  (when (eq 'pending (oref write state))
    (oset write state 'failed)
    (dolist (rollback (oref write rollbacks))
      (funcall rollback))
    (slack-star-pending-write-finish team write)))

(defun slack-star-pending-write-finish (team write)
  "Release terminal saved-item WRITE from TEAM's pending set."
  (let ((journal (slack-star--mutation-journal team)))
    (oset journal pending-writes
          (delq write (oref journal pending-writes)))
    (slack-star-mutation-journal--prune journal)))

(defun slack-star--rollback-add (team entry item previous)
  "Rollback TEAM's optimistic add ENTRY for ITEM to PREVIOUS state."
  (unless (slack-star-mutation-journal-later-entry-p team entry)
    (when-let* ((star (oref team star)))
      (let* ((key (slack-star-item-key item))
             (items
              (cl-remove-if
               (lambda (current)
                 (equal key (slack-star-item-key current)))
               (slack-star-items star))))
        (oset star items
              (if previous
                  (slack-star--add-item items previous)
                items))))))

(defun slack-star--rollback-remove (team entry removed)
  "Rollback TEAM's optimistic remove ENTRY by restoring REMOVED items."
  (unless (or (null removed)
              (slack-star-mutation-journal-later-entry-p team entry))
    (unless (oref team star)
      (oset team star (make-instance 'slack-star :items nil :cursor nil)))
    (let ((star (oref team star)))
      (oset star items
            (slack-star--append-unique-items
             removed (slack-star-items star))))))

(defun slack-star-mutation-journal-later-entry-p (team entry)
  "Return non-nil when TEAM has a later live mutation overlapping ENTRY."
  (cl-some
   (lambda (candidate)
     (let ((write (plist-get candidate :write)))
       (and (> (plist-get candidate :sequence)
               (plist-get entry :sequence))
            (not (and write (eq 'failed (oref write state))))
            (slack-star-mutation-entries-overlap-p entry candidate))))
   (oref (slack-star--mutation-journal team) entries)))

(defun slack-star-mutation-entries-overlap-p (first second)
  "Return non-nil when saved-item mutations FIRST and SECOND overlap."
  (pcase-let ((`(,first-ts ,first-type ,first-id)
               (slack-star-mutation-entry-identity first))
              (`(,second-ts ,second-type ,second-id)
               (slack-star-mutation-entry-identity second)))
    (and (string= first-ts second-ts)
         (or (null first-type) (null second-type)
             (string= first-type second-type))
         (or (null first-id) (null second-id)
             (string= first-id second-id)))))

(defun slack-star-mutation-entry-identity (entry)
  "Return ENTRY's timestamp, item type, and item id identity."
  (if (eq 'add (plist-get entry :operation))
      (let ((item (plist-get entry :item)))
        (list (slack-ts item) (oref item item-type) (oref item item-id)))
    (list (plist-get entry :ts)
          (plist-get entry :item-type)
          (plist-get entry :item-id))))

(provide 'slack-star)
;;; slack-star.el ends here
