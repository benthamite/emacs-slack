# Visible-First Activity and Saved Feeds Implementation Plan

> **For agentic workers:** REQUIRED SUB-SKILL: Use superpowers:subagent-driven-development (recommended) or superpowers:executing-plans to implement this plan task-by-task. Steps use checkbox (`- [ ]`) syntax for tracking.

**Goal:** Make cold Activity Feed and saved-items commands show stable buffers immediately, commit their index pages before child-message hydration, and refresh without hiding or recreating buffers.

**Architecture:** Reuse the durable state and presenter from the foundation plan. Activity snapshots use `(activity-feed MODE)` keys and saved items use `saved-items` in the team registry. A small exactly-once barrier replaces bespoke pending counters so child requests can complete or fail independently after the primary page is visible.

**Tech Stack:** Emacs Lisp, EIEIO, ERT, Slack activity/saved APIs, Lui.

---

### Task 1: Add the shared supplemental-hydration barrier

**Files:**

- Modify: `slack-page-state.el`
- Modify: `test/test-page-state.el`

- [ ] **Step 1: Add zero-work, mixed-completion, and exactly-once tests**

Append:

```elisp
(ert-deftest slack-test-async-barrier-finishes-zero-work-immediately ()
  (let ((calls 0))
    (slack-async-barrier-create 0 (lambda () (cl-incf calls)))
    (should (= 1 calls))))

(ert-deftest slack-test-async-barrier-finishes-exactly-once ()
  (let* ((calls 0)
         (barrier (slack-async-barrier-create
                   2 (lambda () (cl-incf calls)))))
    (slack-async-barrier-done barrier)
    (should (= 0 calls))
    (slack-async-barrier-done barrier)
    (slack-async-barrier-done barrier)
    (should (= 1 calls))))
```

- [ ] **Step 2: Run RED**

Run `make compile test-page-state PROFILE_DIR='/Users/pablostafforini/.config/emacs-profiles/8.3.0-dev'`. Expected: both barrier symbols
are undefined.

- [ ] **Step 3: Implement the exact barrier**

Add:

```elisp
(cl-defstruct (slack-async-barrier
               (:constructor slack-async-barrier--create))
  remaining
  callback
  finished-p)

(defun slack-async-barrier-create (count callback)
  "Return a barrier for COUNT completions that runs CALLBACK once."
  (let ((barrier (slack-async-barrier--create
                  :remaining count :callback callback)))
    (when (= count 0)
      (slack-async-barrier-finish barrier))
    barrier))

(defun slack-async-barrier-done (barrier)
  "Record one success or failure completion on BARRIER."
  (unless (slack-async-barrier-finished-p barrier)
    (cl-decf (slack-async-barrier-remaining barrier))
    (when (<= (slack-async-barrier-remaining barrier) 0)
      (slack-async-barrier-finish barrier))))

(defun slack-async-barrier-finish (barrier)
  "Finish BARRIER exactly once."
  (unless (slack-async-barrier-finished-p barrier)
    (setf (slack-async-barrier-finished-p barrier) t)
    (when (functionp (slack-async-barrier-callback barrier))
      (funcall (slack-async-barrier-callback barrier)))))
```

- [ ] **Step 4: Verify and commit**

Run compile, `test-page-state`, and `batch-test.sh`; then commit:

```sh
git add slack-page-state.el test/test-page-state.el
git commit -m "slack: add supplemental hydration barrier"
```

### Task 2: Move Activity Feed cache and cold open to durable page state

**Files:**

- Modify: `slack-activity-feed-buffer.el:470-650,673-840,1250-1350`
- Modify: `test/run-test.el`
- Modify: `test/test-buffer-rendering.el`

- [ ] **Step 1: Add page-key, cold-order, cache, and kill tests**

Add:

```elisp
(ert-deftest slack-test-activity-feed-cold-open-displays-before-request ()
  (slack-test-setup
    (let ((slack-current-team team)
          request-success
          events)
      (cl-letf (((symbol-function 'slack-activity-feed--selected-team)
                 (lambda () team))
                ((symbol-function 'slack-buffer-display)
                 (lambda (_buffer) (push 'display events)))
                ((symbol-function 'slack-activity-feed-request)
                 (lambda (_team success &rest _)
                   (setq request-success success)
                   (push 'request events))))
        (slack-activity-feed-show)
        (should (equal '(request display) events))
        (should (functionp request-success))))))

(ert-deftest slack-test-activity-feed-mode-has-distinct-page-state ()
  (slack-test-setup
    (let ((slack-activity-feed-mode-show-only-unread t))
      (should (equal '(activity-feed t)
                     (slack-activity-feed--page-key)))
      (should (equal '(activity-feed nil)
                     (slack-activity-feed--page-key nil)))
      (should-not
       (eq (slack-team-page-state
            team (slack-activity-feed--page-key))
           (slack-team-page-state
            team (slack-activity-feed--page-key nil)))))))
```

Retain and adapt the existing tests named
`slack-test-activity-feed-show-renders-cache-before-refresh`,
`slack-test-activity-feed-displays-before-room-prefetch-finishes`, and
`slack-test-feed-render-stops-when-buffer-killed`. Add an assertion that the
captured buffer object is `eq` before and after primary-page application.

- [ ] **Step 2: Run RED**

Run compile, `test-upstream`, and `test-buffer`. Expected: the cold command does
not display before request and the new durable-state assertion fails.

- [ ] **Step 3: Back cache compatibility helpers with the team registry**

Use this stable key:

```elisp
(cl-defun slack-activity-feed--page-key
    (&optional (mode slack-activity-feed-mode-show-only-unread))
  "Return the durable Activity Feed key for MODE."
  (list 'activity-feed mode))
```

`slack-activity-feed--cache-get` returns the state value.
`slack-activity-feed--cache-put` calls `slack-page-state-store` with the existing
snapshot plist, its `:pagination`, and a nonempty-pagination has-more flag.
`--cache-put-key` resolves the team from the key's captured team id, then stores
under `(activity-feed MODE)`. `--cache-keys-for-team` scans `(oref team
page-states)` and returns only keys whose car is `activity-feed`. Remove the
separate `slack-activity-feed--cache` hash after all tests use team state.

- [ ] **Step 4: Implement the Activity Feed state renderer and command**

Allow `slack-create-activity-feed-buffer` to receive an empty
`slack-activity-feed` object. Add:

```elisp
(defun slack-activity-feed-render-page-state (buffer state)
  "Render BUFFER from Activity Feed STATE."
  (let* ((snapshot (slack-page-state-value state))
         (activities (plist-get snapshot :activities))
         (pagination (plist-get snapshot :pagination)))
    (oset buffer activity-feed
          (make-instance 'slack-activity-feed
                         :activities activities
                         :pagination pagination
                         :last nil))
    (slack-activity-feed--replace-live-contents buffer activities)
    (slack-buffer-insert-page-status buffer state)))
```

The replace helper cancels the old render timer, erases the Lui output region,
starts the existing incremental batches, and never calls `slack-buffer-buffer`.

Refactor `slack-activity-feed-show` to create the empty stable buffer and call
`slack-buffer-present-page` with `refresh` non-nil. The loader captures the mode,
uses `slack-activity-feed-request`, parses the index and watched-channel merge into
the existing snapshot, and calls presenter's success before
`slack-activity-feed--prefetch-rooms` or `--prefetch-messages`. Child hydration
uses `slack-async-barrier`; its completion rerenders changed rows and calls
`slack-page-state-ready` for the captured generation.

`slack-activity-feed-refresh` calls the same command-level loader with refresh
non-nil and never replaces the buffer object. Event-driven cache refresh stores a
ready snapshot without displaying it. Load-more keeps its separate guard and
updates the same state's snapshot and continuation.

- [ ] **Step 5: Verify and commit**

Run compile, `test-page-state`, `test-upstream`, `test-buffer`, full `make test`,
and `batch-test.sh`; then:

```sh
git add slack-activity-feed-buffer.el test/run-test.el test/test-buffer-rendering.el
git commit -m "slack: show activity feed before hydration"
```

### Task 3: Commit saved-items index before child-message hydration

**Files:**

- Modify: `slack-star.el:140-185`
- Modify: `slack-stars-buffer.el:40-180,285-430`
- Modify: `test/run-test.el`
- Modify: `test/test-buffer-rendering.el`

- [ ] **Step 1: Add request ordering and stable-refresh tests**

Add:

```elisp
(ert-deftest slack-test-saved-items-cold-open-displays-before-index ()
  (slack-test-setup
    (let (primary events)
      (cl-letf (((symbol-function 'slack-team-select) (lambda () team))
                ((symbol-function 'slack-team-ensure-conversations-loaded)
                 #'ignore)
                ((symbol-function 'slack-buffer-display)
                 (lambda (_buffer) (push 'display events)))
                ((symbol-function 'slack-stars-list-request)
                 (lambda (_team _cursor _ready _error on-primary-page)
                   (setq primary on-primary-page)
                   (push 'request events))))
        (slack-saved-items)
        (should (equal '(request display) events))
        (should (functionp primary))))))

(ert-deftest slack-test-saved-items-refresh-keeps-buffer-alive ()
  (slack-test-setup
    (let* ((buffer (slack-create-stars-buffer team))
           (emacs-buffer (slack-buffer-buffer buffer)))
      (with-current-buffer emacs-buffer
        (cl-letf (((symbol-function 'slack-saved-items) #'ignore))
          (slack-saved-items-refresh-buffer)))
      (should (buffer-live-p emacs-buffer))
      (kill-buffer emacs-buffer))))
```

Add a child-hydration test with two missing items: invoke one success and one error
callback, assert the primary page was already rendered, the barrier completes, and
the page reaches ready without converting the failed row to an empty page.

- [ ] **Step 2: Run RED**

Run compile, `test-upstream`, and `test-buffer`. Expected: cold open remains gated
and refresh kills the buffer.

- [ ] **Step 3: Split saved index from hydration**

Change the existing formal argument list to `(team &optional cursor
after-success on-error on-primary-page)` without changing the first four
positions. Update the docstring to document `ON-PRIMARY-PAGE`. Add exactly one
call to
`on-primary-page` after embedded messages and `(oref team star)` are stored but
before `slack-users-info-request`. Preserve `after-success` timing and `on-error`.

- [ ] **Step 4: Implement the saved page renderer and command**

Use the durable key `saved-items` and `(oref team star)` as the page value. Add
`slack-stars-buffer-render-page-state` to erase/reinsert the output region from the
state's star items, insert the shared status, and start missing-message hydration
only after primary rendering.

Refactor `slack-saved-items` to create/display `slack-stars-buffer` through
`slack-buffer-present-page` before `slack-stars-list-request`. The primary callback
calls success with the star, its cursor, nonempty-cursor has-more, and deferred
ready. The hydrated callback runs `slack-stars--prefetch-messages` using the shared
barrier; on completion it rerenders the exact captured buffer and calls
`slack-page-state-ready`.

Change `slack-stars--prefetch-single`, `--prefetch-reply`, and
`--prefetch-from-history` to call `slack-async-barrier-done` on success, API
failure, or synchronous error. Remove boxed pending counters. Change
`slack-saved-items-refresh-buffer` to invoke the same loader with refresh non-nil;
never call `kill-buffer`.

Keep load-more separate: append the new page to both `(oref team star)` and the
durable state value, preserve the new cursor, hydrate only new items, and reset its
in-flight guard on success or failure.

- [ ] **Step 5: Verify and commit**

Run compile, all test targets, full `make test`, and `batch-test.sh`; then:

```sh
git add slack-star.el slack-stars-buffer.el test/run-test.el test/test-buffer-rendering.el
git commit -m "slack: show saved items before hydration"
```

### Task 4: Feed-wide review and verification

**Files:** All files changed in Tasks 1-3.

- [ ] Dispatch independent specification and Elisp quality reviews. Fix confirmed
      findings with focused RED/GREEN tests and rerun the full affected targets.
- [ ] Run:

```sh
make test PROFILE_DIR='/Users/pablostafforini/.config/emacs-profiles/8.3.0-dev'
~/My\ Drive/dotfiles/claude/bin/batch-test.sh emacs-slack
git diff --check
```

- [ ] If the active Emacs is responsive, disable immediate read marking under
      `unwind-protect`, invoke a cold `slack-activity-feed-show` and
      `slack-saved-items`, and record only buffer name, display-before-request
      boolean, pre/post buffer identity, and final status. If it remains hung, do
      not signal it and keep live verification explicitly blocked.
