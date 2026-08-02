# Visible-First Buffer Loading Design

## Problem

`slack-channel-select` resolves a room quickly, but `slack-room-display` does not
show a buffer until `conversations.history` and any missing user or bot hydration
finish. Reopening a killed room is especially wasteful: the room still owns up to
100 cached messages, yet `slack-room-display` clears them and waits for the same
history again. The unread-room prefetch path writes to that cache, but the display
path discards it, so the producer and consumer disagree about cache ownership.

The same structural problem appears in other request-backed views. Activity Feed
already proves that a cached snapshot can be shown before a refresh and that child
message hydration can update a live buffer later. That implementation is useful
precedent, but it is view-specific and a cold Activity Feed still waits before a
buffer appears.

A second, independent delay exists after data is available: timestamp lookups and
next/previous-message navigation scan message buffers one character at a time.
Large buffers therefore do unnecessary work on the synchronous display path.

## Goals

- Create and display the selected view synchronously once its identity is known.
- Render retained data immediately; otherwise render an explicit loading state.
- Keep one authoritative load state per logical view, independent of whether its
  Emacs buffer is currently alive.
- Coalesce duplicate loads and ignore stale or out-of-order callbacks.
- Update only the exact live buffer instance that initiated or subscribed to a
  request. A late callback must never recreate a killed buffer.
- Preserve usable stale data during refresh and surface terminal failures with a
  retry action.
- Preserve pagination state, including the first response's continuation token.
- Apply the lifecycle consistently to every request-backed view for which the
  logical target is known before the request begins.
- Replace character-by-character timestamp scans with text-property jumps.
- Preserve callback semantics for callers that need a loaded message before they
  navigate, especially deep links and thread opening.

## Non-goals

- Replacing the callback-based request layer with promises or another async model.
- Forcing unrelated API payloads into one generic renderer.
- Displaying a destination before it can be identified. An unresolved direct or
  multi-person conversation must first obtain its stable room id.
- Treating local compose, edit, share, and log buffers as remote pages.

## Architecture

### Page state

A small shared page-state object records the lifecycle of a logical remote view:

- `status`: `unloaded`, `loading`, `ready`, `refreshing`, or `failed`;
- `generation`: a monotonically increasing request generation;
- `loaded-p`: whether any successful page, including an empty one, has committed;
- `value`: the last successfully loaded domain value;
- `continuation`: an opaque cursor or page token owned by the adapter;
- `has-more`: whether another page exists;
- `error`: the most recent terminal error;
- `updated-at`: the time of the last successful commit; and
- `waiters`: callbacks waiting for the current generation to become ready.

The shared state machine owns request ordering, duplicate-load coalescing, waiter
delivery, and failure transitions. It deliberately does not know how Slack APIs
encode pages or how a view renders them.

Room history state belongs to `slack-room`, because its message cache already
survives buffer death and is updated by websocket events and prefetch. Every
other page is stored in a team-owned page-state registry under a stable domain
key such as `(search messages QUERY SORT DIRECTION)`, `(pins ROOM-ID)`, or
`saved-items`. Buffer objects remain disposable presenters and subscribers; they
are never the cache-validity owner.

### State transitions

An initial load changes `unloaded` or `failed` to `loading`. A refresh with a
usable value changes `ready` or `failed` to `refreshing`. A duplicate start while
the same state is `loading` or `refreshing` joins the existing generation instead
of issuing another request.

Only the current generation may commit or fail. A successful commit atomically
updates the value and continuation metadata, clears the error, changes the state
to `ready`, and runs that generation's waiters exactly once. A failure preserves
the last successful value, records the error, changes the state to `failed`, and
notifies subscribers so a visible retry control can be rendered.

Starting a forced refresh increments the generation. Any older response is then
ignored. Killing a buffer does not cancel or invalidate domain data, but the late
callback checks the captured buffer object with `buffer-live-p`; it never calls
`slack-buffer-buffer`, which would recreate the buffer.

### Visible-first presenter

The shared presenter helper performs this ordering:

1. Find or create the stable buffer object.
2. Initialize its Emacs buffer from the last successful value, or insert a loading
   row when no value exists.
3. Display the Emacs buffer.
4. Start or join the asynchronous load.
5. On a current-generation result, replace the loading/error presentation or
   refresh the stale presentation in the captured live buffer.

Renderers stay view-specific. Shared helpers only manage the loading, refreshing,
empty, and error affordances and safe in-place redisplay. Refresh never kills and
recreates a buffer. The user's window, point, and live buffer identity are kept
where the view can meaningfully preserve them.

`displayed` and `ready` are separate events. Ordinary interactive commands need
only `displayed`. Deep links, thread navigation, and callers with success callbacks
register a ready waiter and perform their target lookup only after the necessary
payload has committed.

### Room history

`slack-room-display` creates the room's message-buffer object and displays it
before starting history. If retained messages exist, they are rendered as a stale
snapshot; otherwise the buffer shows a loading row. It does not clear retained
messages before a request.

The conversations request layer exposes a primary-page callback immediately after
parsing the API response, while preserving its existing post-user-hydration
callback for callers that need resolved identities. The primary callback commits
the page, stores the cursor, and rerenders the captured live buffer. The later
hydration callback replaces affected rendered entries without changing page
generation or pagination.

The room records a per-timestamp cache revision at request start. When the primary
response arrives, it may replace or remove an initial-page entry only when that
timestamp's revision is unchanged; a websocket create, edit, or delete after the
request began therefore wins even when a handler mutates an object in place.
Messages that arrived after the request began are never removed. An empty
successful page is represented by `ready` plus an explicit loaded flag and empty
value, so it is distinguishable from `unloaded`.

Unread-room prefetch uses the same room history state and stores its continuation.
A later room display consumes that state immediately and refreshes according to
the same policy; it cannot discard prefetched data merely because no buffer exists.
Direct and multi-person conversations still resolve their server-side room id
first when necessary. Dormant direct conversations also retain their existing
`conversations.open` precondition even when a stable id already exists, because
Slack can reject history for a closed DM. Once the stable room identity is known,
they enter the same visible room-history lifecycle and display its shell; the
loader performs `conversations.open` before requesting history.

### Supplemental hydration

Activity Feed, saved items, threads, and similar views may receive references whose
messages, users, or bots are not locally available. Their primary page commits and
becomes visible first. Supplemental requests use a shared completion barrier and
replace the affected entries as dependencies arrive. A hydration failure leaves a
visible placeholder or partial entry and reports the failure; it does not postpone
the entire page.

## View coverage

The complete migration covers these request-gated paths:

| View or command | Visible-first behavior |
| --- | --- |
| Channel, group, IM, MPIM, all-room, and unread-room selection | Show retained history or loading row, then merge initial history in place. |
| Room/thread deep links | Show the identified room/thread shell immediately; perform target navigation only when ready. |
| Existing thread view | Show cached root/replies or loading row, then load replies in place. New local thread buffers remain immediate. |
| Activity Feed | Show cached snapshot or a cold loading shell before the initial index request; hydrate referenced messages after display. |
| Saved items | Show cached items or a cold loading shell; refresh in place and hydrate referenced messages after display. |
| Channel bookmarks | Show the channel-scoped shell immediately; retain cached bookmarks during refresh. |
| File list | Show cached files or a cold loading shell, retaining pagination. |
| All threads | Show cached threads or a cold loading shell; refresh in place. |
| Message and file search | Create the query-named result buffer immediately, then replace its loading state with results. |
| Scheduled messages | Show the stable buffer immediately and refresh it in place. |
| Pinned items | Show the room-scoped shell immediately and fill it in place. |
| File detail | Show the file-scoped shell immediately and fill it in place. |
| Remote dialog schema | Show a dialog loading shell after its stable dialog id is known, then render controls in place. |

The already-visible-first user profile keeps its behavior and is aligned with the
shared state presentation where doing so removes duplicate lifecycle code. Local
compose/edit/share/log buffers remain request-free. Member selection cannot create
a profile buffer until the selected user is known. An unresolved DM/MPIM cannot
create the final room buffer until Slack returns the room id.

## Errors and retry

Every initial-page adapter supplies an error callback. With no successful value,
the loading row becomes a concise error row with a retry button. With stale data,
the data remains visible and the refresh failure appears as a non-destructive
status row/message. Retrying creates a new generation. Errors are never converted
to empty successful pages, and there is no silent fallback to the old wait-first
path.

## Pagination

The first successful response stores its continuation before rendering. Existing
load-more commands consume the state object's opaque continuation and update it on
each successful page. Initial loads and load-more requests have separate in-flight
guards so a refresh cannot accidentally duplicate a page request. Adapters retain
their native cursor/page interpretation.

## Rendering performance

`slack-buffer-ts-eq`, `slack-buffer-next-point`, and
`slack-buffer-prev-point` will jump between `ts` text-property boundaries rather
than inspect every character. Tests cover forward and backward ranges, positions
inside a property run, gaps, endpoints, missing timestamps, and large unpropertized
spans.

Large message collections continue to use deferred Lui hooks. Initial display is
allowed to render the retained snapshot synchronously because it is bounded by the
existing room-cache trim. Remote results arrive after the shell is visible and may
be inserted in batches when a renderer's measured cost warrants it; batching must
not change ready semantics or reorder entries.

## Verification

Automated regression tests will prove:

- display occurs before request completion on every migrated command family;
- retained room messages survive reopen and render before refresh;
- a successful empty page becomes `ready` and does not refetch on every display;
- duplicate opens coalesce to one request;
- stale generations cannot overwrite a newer result;
- killed buffers are not recreated by late callbacks;
- failures preserve stale data and expose retry;
- first-page continuation is preserved and load-more uses it;
- ready callbacks run once and only after the relevant data commits;
- websocket/prefetch data and initial history are merged without loss; and
- text-property navigation crosses large gaps without character-wise scanning.

The full byte-compile and ERT suite must pass. End-to-end verification will use the
active Emacs profile and real Slack data without marking rooms read: five cold room
opens must display their buffer shell or retained messages within 250 ms, remain
the same buffer object when history arrives, and finish with populated or explicit
empty/error state. Representative cold Activity Feed, saved-items, and search
opens must likewise display before their requests complete. If the active Emacs
session remains unavailable, that exact user-visible verification gap will be
reported rather than described as a completed fix.
