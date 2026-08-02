# Timestamp Property Navigation Implementation Plan

> **For agentic workers:** REQUIRED SUB-SKILL: Use superpowers:subagent-driven-development (recommended) or superpowers:executing-plans to implement this plan task-by-task. Steps use checkbox (`- [ ]`) syntax for tracking.

**Goal:** Remove character-by-character timestamp scans from synchronous Slack buffer display and navigation while preserving their inclusive forward/reverse return positions.

**Architecture:** Keep the three public functions and their signatures. Add two private iterators that visit one `ts` property run at a time with `next-single-property-change` and `previous-single-property-change`, which are available at the package's Emacs 25.1 minimum. Tests assert exact positions and bound property reads directly.

**Tech Stack:** Emacs Lisp text properties, ERT.

---

### Task 1: Lock down current inclusive position semantics

**Files:**

- Modify: `test/test-suite.el`

- [ ] **Step 1: Add exact forward/reverse behavior tests**

Add:

```elisp
(ert-deftest slack-test-buffer-ts-property-navigation-preserves-positions ()
  (with-temp-buffer
    (insert (propertize "aaaaa" 'ts "1"))
    (insert "---")
    (insert (propertize "bbbbb" 'ts "2"))
    (let ((max (point-max)))
      (should (= 9 (slack-buffer-ts-eq 1 max "2")))
      (should (= 13 (slack-buffer-ts-eq max 1 "2")))
      (should (= 3 (slack-buffer-ts-eq 3 max "1")))
      (should (= 11 (slack-buffer-ts-eq 11 1 "2")))
      (should (= 9 (slack-buffer-next-point 1 max "1")))
      (should (= 5 (slack-buffer-prev-point max 1 "2")))
      (should (= 5 (slack-buffer-ts-eq 9 1 "1")))
      (should (= 5 (slack-buffer-prev-point 9 1 "2")))
      (should-not (slack-buffer-ts-eq 1 max "missing"))
      (should-not (slack-buffer-next-point 9 13 "2"))
      (should-not (slack-buffer-prev-point 5 1 "1")))))
```

This records the current contract: forward search returns the first visited
position of a matching run; reverse search returns the last visited position; a
start inside a matching run is returned unchanged.

- [ ] **Step 2: Run the existing implementation GREEN**

Run:

```sh
make compile test-suite PROFILE_DIR='/Users/pablostafforini/.config/emacs-profiles/8.3.0-dev'
```

Expected: the semantic test passes before optimization.

### Task 2: Add a direct complexity regression and jump by runs

**Files:**

- Modify: `slack-buffer.el:820-895`
- Modify: `test/test-suite.el`

- [ ] **Step 1: Add the property-read bound test**

Add:

```elisp
(ert-deftest slack-test-buffer-ts-search-cost-follows-property-runs ()
  (with-temp-buffer
    (dotimes (index 100)
      (insert (propertize (make-string 1000 ?x)
                          'ts (number-to-string index))))
    (let ((calls 0)
          (original (symbol-function 'get-text-property)))
      (cl-letf (((symbol-function 'get-text-property)
                 (lambda (&rest args)
                   (cl-incf calls)
                   (apply original args))))
        (should (slack-buffer-ts-eq
                 (point-min) (point-max) "99"))
        (should (< calls 250))))))
```

- [ ] **Step 2: Run RED**

Run `make compile test-suite PROFILE_DIR='/Users/pablostafforini/.config/emacs-profiles/8.3.0-dev'`. Expected: the read-count assertion
fails because the current function calls `get-text-property` about 99,000 times.

- [ ] **Step 3: Implement property-run iterators**

Add:

```elisp
(defun slack-buffer--find-ts-forward (start end predicate)
  "Return the first position from START through END whose ts satisfies PREDICATE."
  (let ((position start)
        found)
    (while (and (not found) (<= position end))
      (let ((ts (get-text-property position 'ts)))
        (if (and ts (funcall predicate ts))
            (setq found position)
          (setq position
                (and (< position end)
                     (next-single-property-change
                      position 'ts nil
                      (min (point-max) (1+ end))))))))
    found))

(defun slack-buffer--find-ts-backward (start end predicate)
  "Return the first reverse position from START through END matching PREDICATE."
  (let ((position (if (and (= start (point-max)) (> start end))
                      (1- start)
                    start))
        found)
    (while (and (not found) (>= position end))
      (let ((ts (get-text-property position 'ts)))
        (if (and ts (funcall predicate ts))
            (setq found position)
          (let ((boundary (previous-single-property-change
                           (min (point-max) (1+ position))
                           'ts nil end)))
            (setq position
                  (and boundary (> boundary end) (1- boundary)))))))
    found))
```

Then replace the public bodies with:

```elisp
(defun slack-buffer-next-point (start end ts)
  "Return the next position between START and END with a `ts' newer than TS."
  (slack-buffer--find-ts-forward
   start end (lambda (candidate) (string< ts candidate))))

(defun slack-buffer-prev-point (start end ts)
  "Return the prior position between START and END with a `ts' older than TS."
  (slack-buffer--find-ts-backward
   start end (lambda (candidate) (string< candidate ts))))

(defun slack-buffer-ts-eq (start end ts)
  "Return the position between START and END whose `ts' property equals TS."
  (when (and start end)
    (if (<= start end)
        (slack-buffer--find-ts-forward
         start end (lambda (candidate) (string= candidate ts)))
      (slack-buffer--find-ts-backward
       start end (lambda (candidate) (string= candidate ts))))))
```

- [ ] **Step 4: Verify focused and caller suites**

Run:

```sh
make compile test-suite test-buffer PROFILE_DIR='/Users/pablostafforini/.config/emacs-profiles/8.3.0-dev'
make test PROFILE_DIR='/Users/pablostafforini/.config/emacs-profiles/8.3.0-dev'
~/My\ Drive/dotfiles/claude/bin/batch-test.sh emacs-slack
```

Expected: exact-position and complexity tests pass, as do message, file, and saved
item callers.

- [ ] **Step 5: Commit**

```sh
git add slack-buffer.el test/test-suite.el
git commit -m "slack: jump across timestamp property runs"
```

### Task 3: Independent review and edge verification

**Files:** `slack-buffer.el`, `test/test-suite.el`.

- [ ] Review Emacs 25.1 compatibility and forward/reverse boundary behavior. Add a
      failing test before correcting any confirmed issue.
- [ ] Test one-character buffers, `start = end`, `start = point-max`, an entirely
      unpropertized buffer, and adjacent equal-valued property runs.
- [ ] Rerun full `make test PROFILE_DIR='/Users/pablostafforini/.config/emacs-profiles/8.3.0-dev'`, `batch-test.sh`, and
      `git diff --check`. Commit a review repair only if the review required code.
