# Visible-First Foundation and Conversations Implementation Plan

> **For agentic workers:** REQUIRED SUB-SKILL: Use superpowers:subagent-driven-development (recommended) or superpowers:executing-plans to implement this plan task-by-task. Steps use checkbox (`- [ ]`) syntax for tracking.

**Goal:** Add the durable page lifecycle and use it to show rooms, existing threads, and identified deep-link destinations before their Slack requests or supplemental identity hydration finish.

**Architecture:** `slack-page-state.el` is a payload-agnostic state machine. Room history owns one state directly on `slack-room`; other logical pages are stored under stable keys in a `slack-team` registry. `slack-buffer.el` owns only presentation and captured-live-buffer safety, while conversations APIs expose parsed pages before preserving their existing hydrated completion callback.

**Tech Stack:** Emacs Lisp, `cl-defstruct`, EIEIO, ERT, Slack conversations APIs, Lui.

---

Use the worktree-local `make` targets for behavioral evidence because the global
`elisp-ert` wrapper resolves the active profile's canonical source directory, not
this nested worktree. Before every Elisp commit, also run the hook-required
`~/My\ Drive/dotfiles/claude/bin/batch-test.sh emacs-slack` marker command.

### Task 1: Add durable page state and the team registry

**Files:**

- Create: `slack-page-state.el`
- Modify: `slack-team.el:50-115`
- Modify: `slack.el:75-90`
- Create: `test/test-page-state.el`
- Modify: `Makefile:18-36`

- [ ] **Step 1: Create the focused test target and state tests**

Add `test-page-state` to `.PHONY` and this recipe to `Makefile`:

```make
test-page-state:
	$(BATCH) -l emacs-slack -l test/test-page-state.el \
	  --eval '(ert-run-tests-batch-and-exit)'
```

Create `test/test-page-state.el` with these complete first tests:

```elisp
;;; test-page-state.el --- async page-state tests  -*- lexical-binding: t; -*-

(require 'ert)
(require 'slack-page-state)
(require 'slack-team)

(ert-deftest slack-test-page-state-commits-an-empty-page ()
  (let* ((state (slack-page-state-create))
         (generation (slack-page-state-begin state)))
    (should (eq 'loading (slack-page-state-status state)))
    (should (slack-page-state-commit state generation nil "next" t))
    (should (slack-page-state-loaded-p state))
    (should (eq 'ready (slack-page-state-status state)))
    (should-not (slack-page-state-value state))
    (should (equal "next" (slack-page-state-continuation state)))
    (should (slack-page-state-has-more state))))

(ert-deftest slack-test-page-state-coalesces-and-rejects-stale-results ()
  (let* ((state (slack-page-state-create))
         (first (slack-page-state-begin state)))
    (should-not (slack-page-state-begin state))
    (let ((second (slack-page-state-restart state)))
      (should (< first second))
      (should-not (slack-page-state-commit state first '(old) nil nil))
      (should (slack-page-state-commit state second '(new) nil nil))
      (should (equal '(new) (slack-page-state-value state))))))

(ert-deftest slack-test-page-state-refresh-preserves-stale-value-on-failure ()
  (let* ((state (slack-page-state-create))
         (first (slack-page-state-begin state)))
    (slack-page-state-commit state first '(cached) nil nil)
    (let ((second (slack-page-state-begin state t)))
      (should (eq 'refreshing (slack-page-state-status state)))
      (should (slack-page-state-fail state second 'network-error))
      (should (eq 'failed (slack-page-state-status state)))
      (should (equal '(cached) (slack-page-state-value state)))
      (should (slack-page-state-loaded-p state)))))

(ert-deftest slack-test-page-state-primary-commit-precedes-ready ()
  (let* ((state (slack-page-state-create))
         (events nil)
         (generation (slack-page-state-begin state)))
    (slack-page-state-on-commit
     state (lambda (_state) (push 'commit events)))
    (slack-page-state-on-ready
     state (lambda (_state) (push 'ready events)))
    (slack-page-state-commit state generation '(value) nil nil t)
    (should (equal '(commit) events))
    (should (slack-page-state-in-flight-p state))
    (slack-page-state-ready state generation)
    (slack-page-state-ready state generation)
    (should (equal '(ready commit) events))))

(ert-deftest slack-test-page-state-does-not-reuse-completed-error-waiter ()
  (let* ((state (slack-page-state-create))
         (errors 0)
         (first (slack-page-state-begin state)))
    (slack-page-state-on-error
     state (lambda (_state _error) (cl-incf errors)))
    (slack-page-state-commit state first '(value) nil nil)
    (let ((second (slack-page-state-begin state t)))
      (slack-page-state-fail state second 'network-error))
    (should (= 0 errors))))

(ert-deftest slack-test-team-page-state-survives-buffer-cache-removal ()
  (let* ((team (make-instance 'slack-team))
         (first (slack-team-page-state team '(search messages "query" recent desc))))
    (oset team slack-message-buffer (make-hash-table :test 'equal))
    (puthash 'temporary-buffer t (oref team slack-message-buffer))
    (clrhash (oref team slack-message-buffer))
    (should (eq first
                (slack-team-page-state
                 team '(search messages "query" recent desc))))))

(provide 'test-page-state)
;;; test-page-state.el ends here
```

- [ ] **Step 2: Run the tests to verify RED**

Run:

```sh
make test-page-state PROFILE_DIR='/Users/pablostafforini/.config/emacs-profiles/8.3.0-dev'
```

Expected: nonzero exit because `slack-page-state` cannot be required.

- [ ] **Step 3: Implement the state machine**

Create `slack-page-state.el` with this public contract:

```elisp
;;; slack-page-state.el --- remote page lifecycle  -*- lexical-binding: t; -*-

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
  (slack-page-state-discard-waiters
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
  (when (= generation (slack-page-state-generation state))
    (setf (slack-page-state-loaded-p state) t
          (slack-page-state-value state) value
          (slack-page-state-continuation state) continuation
          (slack-page-state-has-more state) has-more
          (slack-page-state-error state) nil
          (slack-page-state-updated-at state) (current-time)
          (slack-page-state-committed-generation state) generation)
    (slack-page-state-notify-commit state)
    (unless defer-ready
      (slack-page-state-ready state generation))
    t))

(defun slack-page-state-ready (state generation)
  "Finish STATE GENERATION after supplemental hydration."
  (when (= generation (slack-page-state-generation state))
    (setf (slack-page-state-status state) 'ready
          (slack-page-state-ready-generation state) generation)
    (slack-page-state-notify-ready state)
    t))

(defun slack-page-state-fail (state generation error)
  "Record ERROR on STATE when GENERATION is current."
  (when (= generation (slack-page-state-generation state))
    (setf (slack-page-state-status state) 'failed
          (slack-page-state-error state) error)
    (slack-page-state-notify-error state)
    t))

(defun slack-page-state-on-commit (state callback)
  "Run or register CALLBACK for STATE's current primary-page commit."
  (if (= (slack-page-state-committed-generation state)
         (slack-page-state-generation state))
      (when (functionp callback) (funcall callback state))
    (when (functionp callback)
      (push (cons (slack-page-state-generation state) callback)
            (slack-page-state-commit-waiters state)))))

(defun slack-page-state-on-ready (state callback)
  "Run or register CALLBACK for STATE's current hydrated completion."
  (if (= (slack-page-state-ready-generation state)
         (slack-page-state-generation state))
      (when (functionp callback) (funcall callback state))
    (when (functionp callback)
      (push (cons (slack-page-state-generation state) callback)
            (slack-page-state-ready-waiters state)))))

(defun slack-page-state-on-error (state callback)
  "Run or register CALLBACK for STATE's current terminal failure."
  (if (eq 'failed (slack-page-state-status state))
      (progn
        (when (functionp callback)
          (funcall callback state (slack-page-state-error state)))
        (slack-page-state-discard-waiters
         state (slack-page-state-generation state)))
    (when (functionp callback)
      (push (cons (slack-page-state-generation state) callback)
            (slack-page-state-error-waiters state)))))

(defun slack-page-state-partition-waiters (waiters generation)
  "Partition WAITERS into GENERATION matches and remaining entries."
  (let (matches remaining)
    (dolist (waiter waiters)
      (if (= generation (car waiter))
          (push waiter matches)
        (push waiter remaining)))
    (cons (nreverse matches) (nreverse remaining))))

(defun slack-page-state-discard-waiters (state generation)
  "Discard every STATE waiter belonging to GENERATION."
  (setf (slack-page-state-commit-waiters state)
        (cdr (slack-page-state-partition-waiters
              (slack-page-state-commit-waiters state) generation))
        (slack-page-state-ready-waiters state)
        (cdr (slack-page-state-partition-waiters
              (slack-page-state-ready-waiters state) generation))
        (slack-page-state-error-waiters state)
        (cdr (slack-page-state-partition-waiters
              (slack-page-state-error-waiters state) generation))))

(defun slack-page-state-notify-commit (state)
  "Run and clear primary-page commit waiters for STATE."
  (let* ((generation (slack-page-state-generation state))
         (parts (slack-page-state-partition-waiters
                 (slack-page-state-commit-waiters state) generation)))
    (setf (slack-page-state-commit-waiters state) (cdr parts))
    (dolist (waiter (car parts)) (funcall (cdr waiter) state))))

(defun slack-page-state-notify-ready (state)
  "Run and clear ready waiters for STATE."
  (let* ((generation (slack-page-state-generation state))
         (parts (slack-page-state-partition-waiters
                 (slack-page-state-ready-waiters state) generation)))
    (setf (slack-page-state-ready-waiters state) (cdr parts)
          (slack-page-state-error-waiters state)
          (cdr (slack-page-state-partition-waiters
                (slack-page-state-error-waiters state) generation)))
    (dolist (waiter (car parts)) (funcall (cdr waiter) state))))

(defun slack-page-state-notify-error (state)
  "Run and clear error waiters for STATE."
  (let* ((generation (slack-page-state-generation state))
         (parts (slack-page-state-partition-waiters
                 (slack-page-state-error-waiters state) generation)))
    (setf (slack-page-state-error-waiters state) (cdr parts))
    (dolist (waiter (car parts))
      (funcall (cdr waiter) state (slack-page-state-error state)))
    (slack-page-state-discard-waiters state generation)))

(provide 'slack-page-state)
;;; slack-page-state.el ends here
```

Add `(require 'slack-page-state)` before `slack-team` in `slack.el`. Add this
slot to `slack-team`:

```elisp
(page-states :initform (make-hash-table :test 'equal))
```

Add this accessor after the class:

```elisp
(defun slack-team-page-state (team key)
  "Return TEAM's durable remote page state under logical KEY."
  (or (gethash key (oref team page-states))
      (let ((state (slack-page-state-create)))
        (puthash key state (oref team page-states))
        state)))
```

- [ ] **Step 4: Verify GREEN and commit**

Run:

```sh
make compile test-page-state PROFILE_DIR='/Users/pablostafforini/.config/emacs-profiles/8.3.0-dev'
~/My\ Drive/dotfiles/claude/bin/batch-test.sh emacs-slack
git add Makefile slack-page-state.el slack-team.el slack.el test/test-page-state.el
git commit -m "slack: add durable async page state"
```

Expected: compilation and all five focused tests pass; commit succeeds.

### Task 2: Add the visible-first buffer presenter

**Files:**

- Modify: `slack-buffer.el:35-340`
- Modify: `test/test-page-state.el`

- [ ] **Step 1: Add presenter ordering and killed-buffer tests**

Append these tests:

```elisp
(ert-deftest slack-test-present-page-displays-before-loader ()
  (let* ((state (slack-page-state-create))
         (events nil)
         (object (make-instance 'slack-test-page-buffer)))
    (cl-letf (((symbol-function 'slack-buffer-display)
               (lambda (_object) (push 'display events))))
      (slack-buffer-present-page
       object state
       (lambda (_generation _success _error) (push 'request events))
       (lambda (_object _state) (push 'render events))))
    (should (equal '(request display render) events))))

(ert-deftest slack-test-present-page-does-not-resurrect-killed-buffer ()
  (let* ((state (slack-page-state-create))
         (object (make-instance 'slack-test-page-buffer))
         success)
    (cl-letf (((symbol-function 'slack-buffer-display) #'ignore))
      (slack-buffer-present-page
       object state
       (lambda (_generation on-success _on-error)
         (setq success on-success))
       #'ignore))
    (let ((dead (oref object buf)))
      (kill-buffer dead)
      (funcall success '(value) nil nil)
      (should-not (buffer-live-p dead))
      (should (eq dead (oref object buf))))))
```

Define the test buffer immediately above them:

```elisp
(defclass slack-test-page-buffer (slack-buffer) ())

(cl-defmethod slack-buffer-name ((_this slack-test-page-buffer))
  "Return the test page buffer name."
  " *slack test page*")

(cl-defmethod slack-buffer-key ((_this slack-test-page-buffer)) 'test-page)
(cl-defmethod slack-team-buffer-key ((_this slack-test-page-buffer))
  'slack-message-buffer)
```

- [ ] **Step 2: Run RED**

Run the focused target. Expected: failure because
`slack-buffer-present-page` is undefined.

- [ ] **Step 3: Implement presenter and status rows**

Add buffer-local retry state and these exact public functions to
`slack-buffer.el`:

```elisp
(defvar-local slack-buffer-page-retry-function nil
  "Function used to retry the current remote page request.")

(defun slack-buffer-page-retry (&optional _button)
  "Retry the failed remote page displayed in the current buffer."
  (interactive)
  (if (functionp slack-buffer-page-retry-function)
      (funcall slack-buffer-page-retry-function)
    (user-error "This Slack page cannot be retried")))

(defun slack-buffer-remove-page-status (_object)
  "Remove all shared remote-page status rows for OBJECT in the current buffer."
  (let ((inhibit-read-only t)
        (position (point-min)))
    (while position
      (let ((next (next-single-property-change
                   position 'slack-page-status nil (point-max))))
        (when (get-text-property position 'slack-page-status)
          (delete-region position (or next (point-max))))
        (setq position (and next (< next (point-max)) next))))))

(defun slack-buffer-insert-page-status (object state)
  "Insert OBJECT's shared loading or failure row from STATE."
  (slack-buffer-remove-page-status object)
  (let ((status (slack-page-state-status state))
        (start (point)))
    (pcase status
      ('loading (insert "Loading Slack data…\n"))
      ('refreshing (insert "Refreshing Slack data…\n"))
      ('failed
       (insert (format "Slack request failed%s  "
                       (if (slack-page-state-error state)
                           (format ": %s" (slack-page-state-error state))
                         "")))
       (insert-text-button "Retry" 'action #'slack-buffer-page-retry
                           'follow-link t)
       (insert "\n")))
    (when (< start (point))
      (put-text-property start (point) 'slack-page-status status))))

(defun slack-buffer-present-page
    (object state loader renderer &optional refresh on-ready on-error)
  "Display OBJECT from STATE, then invoke LOADER and RENDERER.
REFRESH reloads a ready value while coalescing in-flight work.  ON-READY and ON-ERROR are
terminal callbacks for the current request generation."
  (let* ((generation (slack-page-state-begin state refresh))
         (buffer (slack-buffer-buffer object)))
    (cl-labels
        ((live-buffer-p ()
           (and (buffer-live-p buffer)
                (slot-boundp object 'buf)
                (eq buffer (oref object buf))))
         (render (_state)
           (when (live-buffer-p)
             (with-current-buffer buffer
               (funcall renderer object state))))
         (retry ()
           (slack-buffer-present-page
            object state loader renderer t on-ready on-error)))
      (with-current-buffer buffer
        (setq-local slack-buffer-page-retry-function #'retry)
        (funcall renderer object state))
      (slack-buffer-display object)
      (slack-page-state-on-commit state #'render)
      (slack-page-state-on-ready state on-ready)
      (slack-page-state-on-error
       state
       (lambda (failed-state error)
         (render failed-state)
         (when (functionp on-error)
           (funcall on-error failed-state error))))
      (when generation
        (funcall
         loader generation
         (lambda (value continuation has-more &optional defer-ready)
           (slack-page-state-commit
            state generation value continuation has-more defer-ready))
         (lambda (error)
           (slack-page-state-fail state generation error)))))))
```

The renderer owns payload insertion. It calls
`slack-buffer-remove-page-status` with the buffer object, inserts its loaded value
when `slack-page-state-loaded-p` is non-nil, renders its own domain-specific empty
row when that loaded value contains no items, then calls
`slack-buffer-insert-page-status` with the buffer object and state.

- [ ] **Step 4: Verify and commit**

Run compile, `test-page-state`, full `test-buffer`, and `batch-test.sh`; then:

```sh
git add slack-buffer.el test/test-page-state.el
git commit -m "slack: add visible-first page presenter"
```

### Task 3: Expose parsed conversation pages before hydration

**Files:**

- Modify: `slack-conversations.el:450-590`
- Modify: `test/run-test.el`

- [ ] **Step 1: Add history and replies callback-order tests**

Add one ERT test for each function using its existing request-capture pattern.
For history, the essential assertions are:

```elisp
(ert-deftest slack-test-conversations-history-publishes-page-before-users ()
  (slack-test-setup
    (let (request page-callback hydrated-callback events)
      (cl-letf (((symbol-function 'slack-request)
                 (lambda (value &rest _) (setq request value)))
                ((symbol-function 'slack-team-missing-user-ids)
                 (lambda (&rest _) '("U-missing")))
                ((symbol-function 'slack-user-info-request)
                 (lambda (_ids _team &rest args)
                   (setq hydrated-callback (plist-get args :after-success)))))
        (slack-conversations-history
         channel team
         :on-primary-page (lambda (&rest _) (push 'page events))
         :after-success (lambda (&rest _) (push 'hydrated events)))
        (setq page-callback (oref request success))
        (funcall page-callback :data (list :ok t :messages nil))
        (should (equal '(page) events))
        (funcall hydrated-callback)
        (should (equal '(hydrated page) events))))))
```

The replies test uses `:on-primary-page` with the same event ordering and confirms the
page callback receives `(messages next-cursor has-more)`.

- [ ] **Step 2: Run RED**

Run:

```sh
make compile test-upstream PROFILE_DIR='/Users/pablostafforini/.config/emacs-profiles/8.3.0-dev'
```

Expected: the new tests fail because `:on-primary-page` is not accepted/called.

- [ ] **Step 3: Add the backward-compatible callback**

Add `(on-primary-page nil)` to both `slack-conversations-history` and
`slack-conversations-replies`. Immediately after parsing messages and pagination,
call `on-primary-page`. Then perform existing missing-user hydration and call the
unchanged `after-success`. Route API and transport failure to the existing
`on-error`; do not call either success callback on failure.

Use this ordering in history:

```elisp
(when (functionp on-primary-page)
  (funcall on-primary-page messages next-cursor))
(if (< 0 (length user-ids))
    (slack-user-info-request
     user-ids team
     :after-success (lambda () (callback messages next-cursor)))
  (callback messages next-cursor))
```

Use the corresponding three-argument call for replies.

- [ ] **Step 4: Verify and commit**

Run compile, `test-upstream`, and `batch-test.sh`; then:

```sh
git add slack-conversations.el test/run-test.el
git commit -m "slack: publish conversation pages before hydration"
```

### Task 4: Make room history visible first and prefetch-consistent

**Files:**

- Modify: `slack-room.el:40-65,230-275`
- Modify: `slack-message-buffer.el:230-330,737-775`
- Modify: `slack-websocket.el:352-405`
- Modify: `test/run-test.el`
- Modify: `test/test-buffer-rendering.el`

- [ ] **Step 1: Add direct room-display regressions**

Add tests that stub `slack-buffer-display` and capture the history callbacks.
The cold-order test must call `slack-room-display` and assert `display` precedes
`request`; the retained reopen test seeds one message, kills the live buffer,
reopens, and asserts the same message is present before invoking `on-primary-page`.
Additional complete test cases use the same harness to assert:

```elisp
(should (= 1 request-count))                 ; duplicate display coalesces
(should (slack-page-state-loaded-p state))   ; empty response is loaded
(should (equal "cursor-1"
               (slack-page-state-continuation state)))
(should-not (buffer-live-p killed-buffer))   ; late callback stays dead
(should (slack-room-find-message room websocket-ts)) ; concurrent event survives
```

The ready-callback test records `(display request page hydrated ready)` and
asserts `ready` occurs only after the captured hydrated callback, while page
rendering occurs at `page`.

- [ ] **Step 2: Run RED**

Run compile, `test-upstream`, and `test-buffer`. Expected: order/retained/cache
tests fail against current wait-first `slack-room-display`.

- [ ] **Step 3: Add room-owned history state and merge helpers**

Add `(history-state :initform (slack-page-state-create))`,
`(message-revision :initform 0)`, and
`(message-revisions :initform (make-hash-table :test 'equal))` to `slack-room`.
Make `slack-room-set-messages` increment `message-revision` and store the new
revision under each message timestamp. Add a
`slack-room-touch-message-revision` helper and call it from
`slack-room-push-message`, `slack-room-set-messages`, and
`slack-room-delete-message`. A delete retains its new revision as a tombstone
rather than removing it, so an older HTTP response cannot resurrect the message.
`slack-room-clear-messages` touches every removed timestamp before clearing data
and retains those tombstones. Then add
these functions after `slack-room-set-messages`:

```elisp
(defun slack-room-touch-message-revision (room ts)
  "Record one cache mutation for timestamp TS in ROOM."
  (cl-incf (oref room message-revision))
  (puthash ts (oref room message-revision) (oref room message-revisions)))

(defun slack-room-history-start-snapshot (room)
  "Return ROOM's timestamp-to-revision snapshot for a history request."
  (let ((snapshot (make-hash-table :test 'equal)))
    (maphash (lambda (ts revision) (puthash ts revision snapshot))
             (oref room message-revisions))
    snapshot))

(defun slack-room-merge-history-page (room messages team start-snapshot)
  "Merge history MESSAGES into ROOM without overwriting concurrent events.
START-SNAPSHOT maps timestamps to cache revisions at request start; TEAM owns the
room caches."
  (let ((response-ids (make-hash-table :test 'equal)))
    (dolist (message messages)
      (puthash (slack-ts message) t response-ids))
    (dolist (old-message
             (slack-page-state-value (oref room history-state)))
      (let ((ts (slack-ts old-message)))
        (when (and (not (gethash ts response-ids))
                   (equal (gethash ts start-snapshot)
                          (gethash ts (oref room message-revisions))))
          (slack-room-delete-message room ts)))))
  (dolist (message messages)
    (let* ((ts (slack-ts message))
           (start-revision (gethash ts start-snapshot))
           (current-revision (gethash ts (oref room message-revisions))))
      (when (or (null current-revision)
                (equal current-revision start-revision))
        (slack-room-set-messages room (list message) team))))
  (oset room message-ids (cl-sort (oref room message-ids) #'string<))
  (when-let* ((counts (oref team counts))
              (latest (car (last (oref room message-ids)))))
    (slack-room--update-latest room counts latest)))
```

- [ ] **Step 4: Implement message-buffer rendering and room orchestration**

Add `slack-message-buffer-render-history-state` to erase only rendered output,
reinsert load-more from the state's continuation, insert sorted room messages,
and append the shared status row. It must preserve the Lui prompt/markers and use
`slack-buffer-with-deferred-hooks` for the message batch.

Add `slack-room-history-load`. It receives `(room team generation success error)`;
for an IM its loader first calls `slack-conversations-open` and starts history from
that callback, while other rooms start history directly. Both open and history
errors call `error`. The history primary callback performs the revision-aware merge
and calls `success` with deferred ready; hydrated success calls
`slack-page-state-ready` for `generation`.

Refactor `slack-room-display` into the following ordering:

```elisp
(defun slack-room-display (room team &optional success-callback)
  "Display TEAM ROOM before history loading completes.
Call SUCCESS-CALLBACK after supplemental identity hydration."
  (let* ((current-room (or (slack-room-find (oref room id) team) room))
         (state (oref current-room history-state))
         (buffer (slack-create-message-buffer
                  current-room
                  (or (slack-page-state-continuation state) "")
                  team)))
    (slack-buffer-present-page
     buffer state
     (lambda (generation success error)
       (slack-room-history-load
        current-room team generation success error))
     #'slack-message-buffer-render-history-state
     t
     (when (functionp success-callback)
       (lambda (_state) (funcall success-callback))))))
```

`slack-room-history-load` captures the revision snapshot immediately before its
history request and calls `slack-conversations-history` with:

- `:on-primary-page`: merge through `slack-room-merge-history-page`, then call the
  presenter's success with sorted messages, cursor, nonempty-cursor has-more, and
  `defer-ready` non-nil;
- `:after-success`: after verifying the captured generation is still current, call
  `slack-page-state-ready` and rerender the same live buffer so hydrated names
  replace placeholders; the presenter's wrapped ready waiter owns invocation of
  the legacy zero-argument `success-callback`; and
- `:on-error`: invoke the presenter's error callback.

Never call `slack-room-clear-messages` from this path and never call
`slack-buffer-buffer` inside either late callback. In particular, the presenter
displays an IM shell before `slack-room-history-load` invokes
`slack-conversations-open` for the dormant-DM precondition.

- [ ] **Step 5: Route unread prefetch through the same state**

In `slack-prefetch-unread-channels`, skip rooms whose history state is loaded or
in flight. Begin the room state before scheduling a request. Use `:on-primary-page` to
merge and commit the first page plus cursor, then notify ready waiters; use
`:on-error` to fail the generation. The prefetch does not create a buffer. A later
display reads the state and retained room messages synchronously.

- [ ] **Step 6: Verify and commit**

Run compile, `test-page-state`, `test-upstream`, `test-buffer`, full `make test`,
and `batch-test.sh`; then:

```sh
git add slack-room.el slack-message-buffer.el slack-websocket.el test/run-test.el test/test-buffer-rendering.el
git commit -m "slack: display room history before refresh"
```

### Task 5: Make existing threads and deep links visible first

**Files:**

- Modify: `slack-thread-message-buffer.el:45-180`
- Modify: `slack-thread.el:45-70`
- Modify: `slack-message-buffer.el:1115-1140,1272-1395`
- Modify: `test/run-test.el`
- Modify: `test/test-buffer-rendering.el`

- [ ] **Step 1: Add thread and deep-link RED tests**

Use the stable key `(list 'thread (oref room id) thread-ts)` with
`slack-team-page-state`. Tests must prove:

- `slack-thread-show-messages` records display before the replies request;
- cached replies are in the first render;
- two opens before response issue one request;
- killing the thread buffer before `on-primary-page` does not recreate it; and
- `slack-open-message` displays an identified room/thread before fetching but does
  not call `slack-buffer-goto` until the target timestamp is cached.

The navigation assertion records events and checks:

```elisp
(should (equal '(request display) events-before-page))
(funcall captured-page messages "" nil)
(should-not navigated-ts)
(funcall captured-hydrated "" nil)
(should (equal target-ts navigated-ts))
```

- [ ] **Step 2: Run RED**

Run compile, `test-upstream`, and `test-buffer`. Expected: existing threads remain
request-gated and the new ordering tests fail.

- [ ] **Step 3: Adapt replies and thread rendering**

Add `slack-thread-page-key`, `slack-thread-page-state`, and
`slack-thread-message-buffer-render-page-state`. Change
`slack-thread-show-messages` to create/display the stable thread buffer, then use
`slack-buffer-present-page`. Its loader calls `slack-thread-replies` with
`:on-primary-page` to store replies, next cursor, and has-more before user hydration;
`:after-success` rerenders hydrated identities and runs the existing success
callback; `:on-error` fails visibly. Keep `slack-buffer-start-thread` unchanged for
new local threads.

Extend `slack-thread-replies` in `slack-thread.el` with the keyword
`on-primary-page`. Pass it through to `slack-conversations-replies`. The primary
wrapper stores the messages and reply relation, then calls `on-primary-page` with
`(messages next-cursor has-more)`. The existing `after-success` wrapper retains its
current `(next-cursor has-more)` arguments and hydrated timing.

- [ ] **Step 4: Separate displayed destination from ready navigation**

Refactor `slack-open-message--open-channel` and
`slack-open-message--open-thread` so they first display the known destination.
The channel path waits for initial room readiness once. If the target is then
cached, it navigates immediately. Otherwise it runs the existing sequential
`slack-messages-before` and `slack-messages-after` range fetches and invokes the
navigation callback directly from the final range callback; it does not wait on
the initial-page state again. Browser fallback runs only after that final callback
still cannot find the target.

Make `slack-messages-paginate` capture the current live message buffer before its
request and update it only when the captured object remains live and identical.
Add an optional error callback and propagate history errors, so a failed deep-link
range reaches the explicit browser/error path rather than leaving navigation
pending forever.

For a thread link, display the thread destination and wait for that thread page's
ready event once. Navigate immediately if either `ts` or `thread-ts` is then
present; otherwise use the browser fallback. Do not start the channel-range fetch
path for a thread destination.

- [ ] **Step 5: Verify and commit**

Run compile, all three test targets, full `make test`, and `batch-test.sh`; then:

```sh
git add slack-thread-message-buffer.el slack-thread.el slack-message-buffer.el test/run-test.el test/test-buffer-rendering.el
git commit -m "slack: open threads and links before hydration"
```

### Task 6: Foundation review and executable live measurement

**Files:**

- Modify when findings require it: all files from Tasks 1-5

- [ ] **Step 1: Run independent spec and quality reviews**

Dispatch one reviewer against the committed design and one reviewer for Elisp
lifecycle/code quality. Fix every confirmed issue with a focused failing test,
rerun the affected targets plus `batch-test.sh`, and amend the logical commit or
make a dedicated repair commit.

- [ ] **Step 2: Run the complete automated foundation suite**

Run:

```sh
make test PROFILE_DIR='/Users/pablostafforini/.config/emacs-profiles/8.3.0-dev'
~/My\ Drive/dotfiles/claude/bin/batch-test.sh emacs-slack
git diff --check
```

Expected: zero exits and no whitespace errors.

- [ ] **Step 3: Measure the exact live selector path without changing read state**

Only if the active Emacs server responds, evaluate a bounded helper definition
that does no character-wise scanning. The helper must:

1. select five rooms with no live message buffer;
2. save each team's `mark-as-read-immediately` slot and bind/set it to nil under
   `unwind-protect`;
3. call the real interactive `slack-channel-select` with
   `slack-completing-read-function` temporarily returning the chosen channel;
4. record `float-time` immediately before the command and when
   `slack-buffer-function` receives the buffer;
5. capture only buffer name, elapsed milliseconds, `eq` identity before/after
   ready, and final page status; and
6. restore the option even if a request errors.

The bounded result must contain five elapsed values below 250 ms, five unchanged
buffer identities, and five terminal `ready` or `failed` states. Do not print room,
team, message, or request objects. Do not signal, restart, or kill the active Emacs
if it remains unresponsive; in that case record this exact check as blocked and do
not claim the reported behavior fixed.
