# Visible-First Remote Views Implementation Plan

> **For agentic workers:** REQUIRED SUB-SKILL: Use superpowers:subagent-driven-development (recommended) or superpowers:executing-plans to implement this plan task-by-task. Steps use checkbox (`- [ ]`) syntax for tracking.

**Goal:** Migrate every remaining stable request-backed Slack buffer to the shared visible-first lifecycle: files, searches, all threads, scheduled messages, pins, file details, channel bookmarks, and remote dialogs.

**Architecture:** Each view keeps its native payload and renderer but stores the last successful page in the team registry under a stable domain key. Requests that currently delay completion for missing users gain an `on-primary-page` callback; simple requests use the presenter's immediate-ready success. Payload slots become optional only where a stable identity is known before the payload.

**Tech Stack:** Emacs Lisp, EIEIO, ERT, Slack Web/API callbacks, Lui.

---

### Task 1: Make the file list visible before `files.list`

**Files:**

- Modify: `slack-file.el:560-620`
- Modify: `slack-file-list-buffer.el:40-135,235-250`
- Modify: `test/run-test.el`
- Modify: `test/test-buffer-rendering.el`

- [ ] **Step 1: Add cold-order, pagination, empty, and error tests**

Add a command test that stubs `slack-file-list-request`, records display and
request events, and asserts `(request display)` after `slack-file-list`. Add
renderer tests that apply `(:files nil :page 1 :pages 1)` and distinguish the
shared empty row from loading/error. Add a reopen test that kills the buffer,
creates it again, and asserts the durable state's `:files` and page counts survive.

The pagination assertion is:

```elisp
(let ((state (slack-team-page-state team 'file-list)))
  (should (= 1 (plist-get (slack-page-state-value state) :page)))
  (should (= 3 (plist-get (slack-page-state-value state) :pages)))
  (should (equal 2 (slack-page-state-continuation state)))
  (should (slack-page-state-has-more state)))
```

- [ ] **Step 2: Run RED**

Run compile, `test-upstream`, and `test-buffer`; expect the cold-order and durable
pagination tests to fail.

- [ ] **Step 3: Expose the primary file page**

Add `on-primary-page` as an optional keyword to `slack-file-list-request`, next to its
existing `after-success` and error callback. Call it with `(page pages)` immediately
after parsed files are stored on the team, before any missing-user request.
Preserve the hydrated timing and arguments of `after-success` and forward API and
transport failures to `on-error`.

- [ ] **Step 4: Create/display the stable file buffer and render state**

Use key `file-list` and value `(:files FILES :page PAGE :pages PAGES)`. Construct
the cold buffer with page/pages zero. `slack-file-list-buffer-render-page-state`
copies page metadata to slots, clears the output region, inserts team files with
deferred Lui hooks, and renders loading/empty/error status.

`slack-file-list` calls `slack-buffer-present-page` before the request. Primary
success commits the value with next page number as continuation and defers ready;
hydrated success calls `slack-page-state-ready`. Load-more retains its guard,
appends files, and updates the same durable value and page metadata.

- [ ] **Step 5: Verify and commit**

Run all test targets, full `make test PROFILE_DIR='/Users/pablostafforini/.config/emacs-profiles/8.3.0-dev'`, and `batch-test.sh`; then:

```sh
git add slack-file.el slack-file-list-buffer.el test/run-test.el test/test-buffer-rendering.el
git commit -m "slack: show file list before hydration"
```

### Task 2: Make message and file searches visible before results

**Files:**

- Modify: `slack-search.el:46-62,176-275`
- Modify: `slack-search-result-buffer.el:40-220`
- Modify: `test/run-test.el`
- Modify: `test/test-buffer-rendering.el`

- [ ] **Step 1: Add both command-order and stable-query tests**

Stub `slack-search-query-params` to return `(team "needle" "timestamp" "desc")`
and record the page keys passed by both commands. Assert exactly:

```elisp
(should (member '(search messages "needle" "timestamp" "desc") keys))
(should (member '(search file "needle" "timestamp" "desc") keys))
```

For each command, capture the request completion and assert the correctly named
search buffer was displayed first. Add a repeated-query test asserting the same
Emacs buffer remains live and receives new results in place. Add empty/error and
pagination-survives-kill renderer tests.

- [ ] **Step 2: Run RED**

Run compile, `test-upstream`, and `test-buffer`; expect both commands to remain
request-gated and repeated search to kill/recreate its buffer.

- [ ] **Step 3: Add primary search completion**

Extend the existing positional method's formal list to `((this
slack-search-result) after-success team &optional (page 1) on-error
on-primary-page)` without reordering the first five arguments. Invoke
`on-primary-page` with `this` immediately after
`slack-merge` and pagination parsing, before missing-user hydration. Preserve
`after-success`; forward failure to `on-error`.

- [ ] **Step 4: Create query-identity shells and render in place**

Add `slack-search-empty-pagination`, returning a fully initialized
`slack-search-pagination` with all numeric slots zero. Both commands create an
empty result with query/sort/direction, `:total 0`, `:matches nil`, and that
pagination before calling the presenter.

Remove the kill/reset branch from `slack-create-search-result-buffer`; update its
result slot and rerender the existing live buffer. Use the keys asserted above and
store the result object as value. `slack-search-result-buffer-render-page-state`
clears output, inserts matches, adds load-more according to pagination, and renders
the shared status. Primary success defers ready; hydrated success marks ready.
Load-more updates state after `slack-merge`.

- [ ] **Step 5: Verify and commit**

Run all suites plus `batch-test.sh`; then:

```sh
git add slack-search.el slack-search-result-buffer.el test/run-test.el test/test-buffer-rendering.el
git commit -m "slack: show search buffers before hydration"
```

### Task 3: Make All Threads visible without clearing unread state early

**Files:**

- Modify: `slack-all-threads-buffer.el:120-350`
- Modify: `test/run-test.el`
- Modify: `test/test-buffer-rendering.el`

- [ ] **Step 1: Add ordering and side-effect tests**

Add a cold-order test and this essential side-effect sequence:

```elisp
(slack-all-threads)
(should displayed)
(should-not cleared)
(funcall primary 0 0 nil nil)
(should cleared)
```

Also test refresh retains buffer identity, empty/error states, pagination, and a
killed buffer is not recreated by late hydration.

- [ ] **Step 2: Run RED**

Run compile and both ERT targets. Expected: display waits, and buffer initialization
calls `slack-subscriptions-thread-clear-all` before any successful page.

- [ ] **Step 3: Split primary threads from user hydration**

Extend `slack-subscriptions-thread-get-view` to optional positional arguments
`(team &optional current-ts after-success on-primary-page on-error)`. After parsing
the four current values, call `on-primary-page` with those values before missing
users. Preserve the same four arguments and post-hydration timing for
`after-success`. Forward errors.

- [ ] **Step 4: Render the team-owned page**

Use key `all-threads` and value:

```elisp
(:total-unread-replies TOTAL
 :new-threads-count NEW
 :threads THREADS
 :has-more HAS-MORE
 :current-ts CURRENT-TS)
```

Allow the constructor to default counts to zero, threads to nil, and has-more to
nil. Remove unread clearing from `slack-buffer-init-buffer`. The presenter displays
the shell first; primary success stores/renders the value, then calls
`slack-subscriptions-thread-clear-all` exactly once for that generation and defers
ready. Hydrated success rerenders names and marks ready. Refresh and pagination
reuse the same buffer/state.

- [ ] **Step 5: Verify and commit**

Run all suites plus `batch-test.sh`; then:

```sh
git add slack-all-threads-buffer.el test/run-test.el test/test-buffer-rendering.el
git commit -m "slack: show all threads before hydration"
```

### Task 4: Refresh scheduled messages in place

**Files:**

- Modify: `slack-scheduled-messages-buffer.el:90-325`
- Modify: `test/run-test.el`
- Modify: `test/test-buffer-rendering.el`

- [ ] **Step 1: Add cold-order, identity, empty, and error tests**

Capture `slack-list-scheduled-messages-request`'s callback; assert the stable
scheduled buffer displays before it is called. Seed a live buffer, invoke refresh,
deliver a new draft list, and assert `(eq old-emacs-buffer (oref object buf))`.
Test nil drafts render empty, while transport failure renders failed with retry.

- [ ] **Step 2: Run RED**

Run compile and both ERT targets; expect display ordering and identity to fail.

- [ ] **Step 3: Add errors and a stable constructor**

Change `slack-list-scheduled-messages-request` to `(team after-success &optional
on-error)` and set the request object's error callback. Add
`slack-create-scheduled-messages-buffer`, which finds or creates the team buffer
with `:messages nil` and caches the team.

- [ ] **Step 4: Use the presenter**

Extract the existing response-to-message conversion into
`slack-scheduled-messages-parse`. Use key `scheduled-messages`, with the sorted
message list as value. The loader parses and calls success directly—there is no
supplemental hydration, so ready is immediate. The renderer updates the messages
slot and output in place. `g`, create-success, and delete-success call this same
refresh path; remove all old-buffer killing.

- [ ] **Step 5: Verify and commit**

Run all suites plus `batch-test.sh`; then:

```sh
git add slack-scheduled-messages-buffer.el test/run-test.el test/test-buffer-rendering.el
git commit -m "slack: keep scheduled messages visible during refresh"
```

### Task 5: Make pinned items visible before `pins.list`

**Files:**

- Modify: `slack-pinned-item.el:70-110`
- Modify: `slack-pinned-items-buffer.el:40-105`
- Modify: `slack-message-buffer.el:379-390`
- Modify: `test/run-test.el`
- Modify: `test/test-buffer-rendering.el`

- [ ] **Step 1: Add command-order and hydration tests**

Invoke `slack-buffer-display-pins-list` on a message buffer, capture the pins
primary and hydrated callbacks, and assert display occurs first. Deliver a parsed
item before missing-user completion and assert it renders immediately. Add room-key
isolation, empty/error, same-buffer refresh, and kill-before-hydration tests.

- [ ] **Step 2: Run RED**

Run compile and both ERT targets; expect the buffer to remain request-gated.

- [ ] **Step 3: Split primary pins and tolerate empty construction**

Change the request signature to `(room team after-success &optional
on-primary-page on-error)`. Call the primary callback with parsed items before
`slack-users-info-request`; preserve hydrated `after-success`; add request/API
error forwarding. Let `slack-create-pinned-items-buffer` accept optional items.

- [ ] **Step 4: Present room-keyed pins**

Use key `(pins ROOM-ID)` and the item list as value. The source buffer method
creates the empty room-scoped buffer and calls the presenter before `slack-pins-list`.
Primary success defers ready; hydrated success marks ready. The renderer updates
the items slot, clears output, inserts items, and adds shared status without lazy
buffer access in late callbacks.

- [ ] **Step 5: Verify and commit**

Run all suites plus `batch-test.sh`; then:

```sh
git add slack-pinned-item.el slack-pinned-items-buffer.el slack-message-buffer.el test/run-test.el test/test-buffer-rendering.el
git commit -m "slack: show pinned items before hydration"
```

### Task 6: Make file detail visible from its stable file id

**Files:**

- Modify: `slack-file.el:360-400`
- Modify: `slack-file-info-buffer.el:35-115`
- Modify: `test/run-test.el`
- Modify: `test/test-buffer-rendering.el`

- [ ] **Step 1: Add cached-summary/cold/error/identity tests**

Use a concrete `slack-message-buffer` as the source. Assert
`slack-buffer-display-file` displays a `slack-file-info-buffer` named with the file
id before `slack-file-request-info`. If a summary exists in the team file cache,
assert it renders before completion. Deliver full info and assert buffer identity;
fail and assert stale summary plus retry remain.

- [ ] **Step 2: Run RED**

Run compile and both ERT targets; expect the detail buffer to wait for full info.

- [ ] **Step 3: Separate identity from optional payload**

Change the class to:

```elisp
(defclass slack-file-info-buffer (slack-buffer)
  ((file-id :initarg :file-id :type string)
   (file :initarg :file :initform nil :type (or null slack-file))))
```

Change constructor to `(team file-id &optional file)`, key/name to use `file-id`,
and every renderer/action to tolerate nil until loaded. Change
`slack-file-request-info` to add optional `on-error` without reordering existing
arguments.

- [ ] **Step 4: Present and refresh by file id**

Use key `(file-info FILE-ID)` and `slack-file` as value. Seed state synchronously
from `slack-file-find` when present, create/display the shell, then refresh through
the presenter. The simple info callback commits ready immediately. Make
`slack-file-update` store and rerender through the same state.

- [ ] **Step 5: Verify and commit**

Run all suites plus `batch-test.sh`; then:

```sh
git add slack-file.el slack-file-info-buffer.el test/run-test.el test/test-buffer-rendering.el
git commit -m "slack: show file details before full metadata"
```

### Task 7: Make remote dialogs visible before schema fetch

**Files:**

- Modify: `slack-dialog-buffer.el:90-130,350-440`
- Modify: `test/run-test.el`
- Modify: `test/test-buffer-rendering.el`

- [ ] **Step 1: Add cold-order, consumed-state, error, and kill tests**

Capture the request object from `slack-dialog-get`; assert the dialog-id-named shell
displays before its success callback. Apply a schema and assert same-buffer render.
Fail and assert retry. Submit and cancel tests must assert `(dialog ID)` is removed
from `(oref team page-states)`. A late callback after kill must not recreate it.

- [ ] **Step 2: Run RED**

Run compile and both ERT targets; expect schema-gated display and missing state
cleanup.

- [ ] **Step 3: Allow a schema-less shell**

Change `dialog` to `:initform nil :type (or null slack-dialog)`. Preserve constructor
argument order `(dialog-id dialog team)` so callers pass nil during load. Use dialog
id in the loading name and make insertion/render methods handle nil by rendering
shared page status only.

- [ ] **Step 4: Present and clear consumed schemas**

Use key `(dialog DIALOG-ID)` and the `slack-dialog` as value. `slack-dialog-get`
creates/displays the shell, then sends the request; success parses and commits ready,
error fails. On successful submit, explicit cancel, or dialog buffer teardown after
either action, call:

```elisp
(remhash (list 'dialog dialog-id) (oref team page-states))
```

Do not clear state merely because the user temporarily kills an unsubmitted dialog;
that would defeat reopen caching while its request is in flight.

- [ ] **Step 5: Verify and commit**

Run all suites plus `batch-test.sh`; then:

```sh
git add slack-dialog-buffer.el test/run-test.el test/test-buffer-rendering.el
git commit -m "slack: show remote dialogs before schema load"
```

### Task 8: Make channel bookmarks visible before `bookmarks.list`

**Files:**

- Create: `slack-channel-bookmarks-buffer.el`
- Modify: `slack-channel.el:106-130`
- Modify: `slack-team.el:80-105`
- Modify: `slack.el:100-115,480-510`
- Modify: `test/run-test.el`
- Modify: `test/test-buffer-rendering.el`

- [ ] **Step 1: Add cold-order, error/retry, kill, and renderer tests**

Stub `slack-bookmarks-request`, call `slack-show-channel-bookmarks`, and record
display and request events. Assert the command returns the displayed object and:

```elisp
(should (equal '(display request) (nreverse events)))
(should (equal '(channel-bookmarks "C11111")
               (slack-channel-bookmarks-page-key "C11111")))
```

Fail the captured request and assert an in-buffer retry starts a second request
without replacing the object or Emacs buffer. Kill the buffer before success,
invoke the late callback, and assert the durable bookmark value is committed but
no buffer is recreated. In `test/test-buffer-rendering.el`, cover cold loading,
an empty ready page, cached bookmarks during refresh, and a failed page with retry.

- [ ] **Step 2: Run RED**

Run compile, `test-upstream`, and `test-buffer`; expect the existing help-window
implementation to remain request-gated and to lack durable failure/retry state.

- [ ] **Step 3: Add the stable bookmark buffer and presenter**

Create `slack-channel-bookmarks-buffer`, keyed by channel id in the team buffer
registry. Store pages under `(channel-bookmarks CHANNEL-ID)`. Its renderer inserts
the shared loading/refresh/error status and, when loaded, an Org heading plus one
link per bookmark or `(No bookmarks.)`. Its presenter must have this shape:

```elisp
(defun slack-channel-bookmarks-buffer--present (channel-id team refresh)
  (let* ((state (slack-team-page-state
                 team (list 'channel-bookmarks channel-id)))
         (buffer (slack-create-channel-bookmarks-buffer channel-id team)))
    (slack-buffer-present-page
     buffer state
     (lambda (_generation success error)
       (slack-bookmarks-request
        channel-id team
        (lambda (data)
          (funcall success
                   (slack-seq-to-list (plist-get data :bookmarks)) nil nil))
        error))
     #'slack-channel-bookmarks-buffer-render-page-state
     refresh)
    buffer))
```

Normalize bookmark payloads inside `condition-case` and route normalization
errors through `error`; do not partially mutate team or buffer state first.

- [ ] **Step 4: Wire request errors, registration, and the command**

Extend `slack-bookmarks-request` to accept optional `on-error`, pass it to
`slack-request-handle-error`, and route transport errors to it. Add the
`slack-channel-bookmarks-buffer` team registry slot, require the new module from
`slack.el`, and replace the delayed help-window callback with:

```elisp
(if (and channel-id team)
    (slack-channel-bookmarks-buffer--present channel-id team t)
  (user-error "Not in a Slack channel buffer"))
```

- [ ] **Step 5: Verify and commit**

Run all suites plus `batch-test.sh`; then:

```sh
git add README.md slack-channel-bookmarks-buffer.el slack-channel.el \
  slack-team.el slack.el test/run-test.el test/test-buffer-rendering.el
git commit -m "slack: show channel bookmarks before loading"
```

### Task 9: Audit all remaining request-backed entry points

**Files:**

- Modify when an omission is found: the owning module and its tests
- Modify: `IMPROVEMENTS.org`

- [ ] **Step 1: Run the complete inventory**

Run `rg -n` for every interactive `slack-buffer-display`, `slack-room-display`,
and request call. Classify every request-backed interactive path. The expected
applicable set is room selection, existing threads/deep links, Activity Feed,
saved items, channel bookmarks, file list, both searches, all threads, scheduled
messages, pins, file detail, and remote dialogs.

The documented exceptions are unresolved DM/MPIM identity,
conversation-member selection before the target user is chosen, local new-thread
and compose/edit/share/log buffers, and the already-visible-first user profile.
Any other wait-first path is a defect: add a failing display-before-response test
and migrate it in a dedicated commit before proceeding.

- [ ] **Step 2: Add a direct full-scope table test**

Add one ERT test whose 15 rows invoke the command adapters with captured pending
callbacks and assert each row recorded display before request completion. The rows
are room history, room deep link, existing thread, thread deep link, Activity Feed,
saved items, channel bookmarks, file list, message search, file search, All Threads,
scheduled messages, pins, file detail, and remote dialog. The explicit row count is
the mechanical completion measure; passing unrelated totals is not sufficient.

- [ ] **Step 3: Verify and record the closed issue**

Update the existing channel-loading entry in `IMPROVEMENTS.org` with durable state,
primary-before-hydration, visible failures/retry, coalescing, and killed-buffer
safety. Run full `make test PROFILE_DIR='/Users/pablostafforini/.config/emacs-profiles/8.3.0-dev'`, `batch-test.sh`, and `git diff --check`.
Commit any audit/test/doc changes as one focused cross-view invariant commit.
