# Emacs 29 Test Compatibility Implementation Plan

> **For agentic workers:** REQUIRED SUB-SKILL: Use superpowers:subagent-driven-development (recommended) or superpowers:executing-plans to implement this plan task-by-task. Steps use checkbox (`- [ ]`) syntax for tracking.

**Goal:** Make the two nil-return regression tests portable across the supported Emacs 29.4, 30.1, and snapshot matrix.

**Architecture:** Keep the production return-value fixes unchanged. Remove only the redundant pretty-printer string assertions; the adjacent `should-not` assertions already exercise the intended public behavior without depending on Emacs formatting details.

**Tech Stack:** Emacs Lisp, ERT, GitHub Actions matrix

---

### Task 1: Assert nil semantically

**Files:**
- Modify: `test/run-test.el:7150`
- Modify: `test/run-test.el:9345`
- Test: `test/run-test.el`

- [ ] **Step 1: Confirm the failing compatibility evidence**

Run: `gh run view 31181538107 -R benthamite/emacs-slack --job 92875745457 --log`

Expected: Emacs 29.4 reports both failures as `equal "nil\n" "nil"`, after each `should-not` assertion has already accepted the nil result.

- [ ] **Step 2: Remove the version-dependent assertions**

In both regression tests, retain:

```elisp
(should-not result)
```

Delete only:

```elisp
(should (equal "nil\n" (pp-to-string result)))
```

- [ ] **Step 3: Run the complete compile-first suite**

Run: `make test`

Expected: all local checks pass with zero unexpected ERT results.

- [ ] **Step 4: Run the standalone package verification**

Run: `~/My\ Drive/dotfiles/claude/bin/batch-test.sh emacs-slack`

Expected: package loads and batch verification exits successfully without stale-load warnings.

- [ ] **Step 5: Commit the isolated test repair**

```bash
git add test/run-test.el docs/superpowers/plans/2026-08-07-emacs-29-test-compatibility.md
git commit -m "test: keep nil assertions portable across Emacs"
```

- [ ] **Step 6: Push and verify the exact failed matrix job**

Run: `git push origin master`, then monitor the resulting Tests workflow.

Expected: Emacs 29.4, 30.1, and snapshot all complete successfully.
