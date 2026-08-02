# Visible-First Slack Buffer Loading Implementation Plan

> **For agentic workers:** REQUIRED SUB-SKILL: Use superpowers:subagent-driven-development (recommended) or superpowers:executing-plans to implement this plan task-by-task. Steps use checkbox (`- [ ]`) syntax for tracking.

**Goal:** Complete the visible-first migration for every applicable request-backed Slack buffer without leaving a second-stage migration for later.

**Architecture:** This is the execution index for four implementation-ready subplans. Room history state lives on rooms; every other page lives in a durable team registry. Disposable buffer objects render those states, and parsed primary pages commit before supplemental identity or message hydration.

**Tech Stack:** Emacs Lisp, EIEIO, ERT, Slack async request callbacks, Lui.

---

The subplans divide independent code units so each can be implemented, tested,
reviewed, and committed without redesigning the others. Completing this index
means completing all four subplans in order during this branch:

- [ ] Execute
      `2026-08-02-visible-first-foundation-and-conversations.md` completely.
- [ ] Execute `2026-08-02-visible-first-feeds.md` completely.
- [ ] Execute `2026-08-02-visible-first-remote-views.md` completely.
- [ ] Execute `2026-08-02-timestamp-property-navigation.md` completely.
- [ ] Update `IMPROVEMENTS.org` with the closed lifecycle defect and the invariant
      that domain readiness is never inferred from buffer existence.
- [ ] Update `README.md` under “How to use” with this user-facing guarantee:
      channel/group/DM/thread selection, Activity Feed, saved items, channel
      bookmarks, file lists, searches, all threads, scheduled messages, pins,
      file details, and remote dialogs display a cached result or loading shell
      before their Slack request finishes, refresh in place, coalesce duplicate
      opens, retain pagination, and show a retry control without discarding stale
      data on failure.
- [ ] Run an `rg` inventory of every interactive request-backed buffer entry point
      and add any applicable omission to the relevant subplan before final review.
- [ ] Run independent specification and code-quality reviews over the combined
      diff, fix every confirmed finding, and rerun all checks.
- [ ] Run `~/My Drive/dotfiles/claude/bin/batch-test.sh emacs-slack` and
      `make test PROFILE_DIR='/Users/pablostafforini/.config/emacs-profiles/8.3.0-dev'`.
- [ ] Perform the live procedure in the foundation subplan for five direct
      `slack-channel-select` invocations plus one cold Activity Feed, saved-items,
      and message-search open. Restore all temporarily changed team options with
      `unwind-protect`.
- [ ] Remove the branch-local documentation deferral with
      `git config --unset branch.codex/async-buffer-lifecycle.deferDocUpdates`,
      stage the `README.md` and `IMPROVEMENTS.org` updates with the last Elisp
      milestone, and confirm the setting is absent.
- [ ] Run `git diff --check`, `git status --short`,
      `git log --oneline master..HEAD`, and `git diff --stat master...HEAD`.
- [ ] Use the finishing-development-branch workflow only after every preceding box
      is complete.

For every Elisp commit in the subplans, first run:

```sh
~/My\ Drive/dotfiles/claude/bin/batch-test.sh emacs-slack
```

The branch has `branch.codex/async-buffer-lifecycle.deferDocUpdates=true` only for
the deliberate multi-commit refactor. It is removed at the final documentation
milestone above.
