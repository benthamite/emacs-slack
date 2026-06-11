<p>
  <a href="https://melpa.org/#/slack"><img alt="MELPA" src="https://melpa.org/packages/slack-badge.svg"/></a>
</p>
<p align="center"><img src="https://raw.githubusercontent.com/yuya373/emacs-slack/assets/assets/slack-logo.svg?sanitize=true" width=300 height=126/></p>
<p align="center"><b>Emacs Slack</b></p>
<p align="center">GNU Emacs client for <a href="https://slack.com/">Slack</a>.</p>

---

## Preview

You can see some gifs on the [wiki](https://github.com/emacs-slack/emacs-slack/wiki/ScreenShots).

## Configuration

[How to get token and cookie](#how-to-get-token-and-cookie)

If your token expires, you can use `slack-refresh-token` for a way to refresh interactively.

```elisp
(use-package slack
  :bind (("C-c S K" . slack-stop)
         ("C-c S c" . slack-select-rooms)
         ("C-c S u" . slack-select-unread-rooms)
         ("C-c S U" . slack-user-select)
         ("C-c S s" . slack-search-from-messages)
         ("C-c S J" . slack-jump-to-browser)
         ("C-c S j" . slack-jump-to-app)
         ("C-c S e" . slack-insert-emoji)
         ("C-c S E" . slack-message-edit)
         ("C-c S r" . slack-message-add-reaction)
         ("C-c S t" . slack-thread-show-or-create)
         ("C-c S g" . slack-message-redisplay)
         ("C-c S G" . slack-conversations-list-update-quick)
         ("C-c S q" . slack-quote-and-reply)
         ("C-c S Q" . slack-quote-and-reply-with-link)
         (:map slack-mode-map
               (("@" . slack-message-embed-mention)
                ("#" . slack-message-embed-channel)))
         (:map slack-thread-message-buffer-mode-map
               (("C-c '" . slack-message-write-another-buffer)
                ("@" . slack-message-embed-mention)
                ("#" . slack-message-embed-channel)))
         (:map slack-message-buffer-mode-map
               (("C-c '" . slack-message-write-another-buffer)))
         (:map slack-message-compose-buffer-mode-map
               (("C-c '" . slack-message-send-from-buffer)))
         )
  :custom
  (slack-extra-subscribed-channels (mapcar 'intern (list "some-channel")))
  (slack-activity-feed-watch-channels '("announcements" "C1234567890"))
  :config
  (slack-register-team
     :name "clojurians"
     :token "xoxc-sssssssssss-88888888888-hhhhhhhhhhh-jjjjjjjjjj"
     :cookie "xoxd-sssssssssss-88888888888-hhhhhhhhhhh-jjjjjjjjjj; d-s=888888888888; lc=888888888888"
     :full-and-display-names t
     :default t
     :subscribed-channels nil ;; using slack-extra-subscribed-channels because I can change it dynamically
     ))

(use-package alert
  :commands (alert)
  :init
  (setq alert-default-style 'notifier))

```

## How to get token and cookie

Use the interactive command `slack-refresh-token` because it has the
most complete instructions to get token and cookies required for emacs-slack to work.

For further explanation, see the documentation for the emojme project:
[(github.com/jackellenberger/emojme)](https://github.com/jackellenberger/emojme#slack-for-web)

Note that it is only possible to obtain the cookie manually, not through
client-side javascript, due to it being set as `HttpOnly` and `Secure`. See
[OWASP HttpOnly](https://owasp.org/www-community/HttpOnly#Browsers_Supporting_HttpOnly).

## How to secure your token

If someone steals your token they can use the token to impersonate
you, reading and posting to Slack as if they were you.  It's important
to take reasonable precautions to secure your token.

One way to do this is by using the Emacs `auth-source` library. Read
the [auth-source
documentation](https://www.gnu.org/software/emacs/manual/html_node/auth/index.html)
to learn how to use it to store login information for remote services.

Then configure the `auth-sources` variable to select a "backend"
store. The default backend is `~/.authinfo` file, which is simple but
also un-encrypted. A more complex option is to encrypt that
`.~/authinfo` file with `gnupg` and configure `auth-sources` to use
`~/.authinfo.gpg` as the source for all passwords and secrets. Other
backends exist beyond these; read the documentation for details.

How to store your slack tokens in your `auth-source` backend will vary
depending which backend you chose. See documentation for details. The
"host" and "user" fields can be whatever you like as long as they are
unique; as a suggestion use "myslackteam.slack.com" for host, and use
your email address for user. The "secret" or "password" field should
contain the token you obtained earlier ([How to get
token and cookie](#how-to-get-token-and-cookie)).

Do the same for the cookie, however for the "user" field append `^cookie`, so if
for the token you picked `user@email.com` then for the cookie use
`user@email.com^cookie`.

Then finally, in your Emacs init read the token from your
`auth-source`:

``` elisp
(slack-register-team
 :name "myslackteam"
 :token (auth-source-pick-first-password
         :host "myslackteam.slack.com"
         :user "me@example.com")
 :subscribed-channels '((channel1 channel2)))
```

If your token starts with `xoxc` you'll also need to manually obtain the cookie
as described in [How to get token and cookie](#how-to-get-token-and-cookie) and
make sure the "user" has `^cookie` in it as described above in [How to secure
your token](#how-to-secure-your-token):

``` elisp
(slack-register-team
 :name "myslackteam"
 :token (auth-source-pick-first-password
         :host "myslackteam.slack.com"
         :user "me@example.com")
 :cookie (auth-source-pick-first-password
         :host "myslackteam.slack.com"
         :user "me@example.com^cookie")
 :subscribed-channels '((channel1 channel2)))
```

If you do not specify `:cookie` then you'll automatically be prompted for one if
you are using an `xoxc` token.

For Enterprise Grid workspaces, `slack-register-team` also accepts
`:enterprise-token`. When both `:token` and `:enterprise-token` are configured,
emacs-slack chooses the token per Slack API endpoint and retries once with the
other token if Slack reports that the selected token is invalid or restricted
for that endpoint.

## How to use

I recommend to chat with slackbot for tutorial using `slack-im-select`.

Some terminology in the `slack-` functions:
- `im`: An IM (instant message) is a direct message between you and exactly one other Slack user.
- `channel`: A channel is a Slack channel which you are a member of
- `group`. Any chat (direct message or channel) which isn't an IM is a group.

- `slack-register-team`
  - set team configuration and create team.
  - :name and :token are required
- `slack-change-current-team`
  - change `slack-current-team` var
- `slack-start`
  - do authorize and initialize
- `slack-ws-close`
  - turn off websocket connection
- `slack-group-select`
  - select group from list
- `slack-im-select`
  - select direct message from list
- `slack-channel-select`
  - select channel from list
- `slack-group-list-update`
  - update group list
- `slack-im-list-update`
  - update direct message list
- `slack-channel-list-update`
  - update channel list
- `slack-activity-feed-show`
  - show the activity feed
  - if the Activity feed buffer is already visible, showing or refreshing it
    updates that buffer instead of opening the same feed in another window
  - when cached data is available, display it immediately and refresh the
    cache in the background; if that refresh returns newer rows, the visible
    feed is redrawn from the fresh snapshot
  - set `slack-activity-feed-watch-channels` to include recent messages from
    specific channels in the feed, even when Slack would not otherwise show
    those messages as activity
  - watched-channel messages newer than the channel's last-read marker also
    contribute to the unread Activity indicator
  - unread-only Activity feeds show only unread watched-channel messages
  - incoming messages in watched channels update that unread indicator without
    waiting for the next Activity feed refresh
  - distinct Slack Activity entries for the same message, such as a mention
    and a reaction, are kept as separate feed rows
  - large Activity feeds keep their newest rows while asynchronous rendering
    finishes, including when an existing Activity feed buffer is refreshed
  - missing Activity feed messages are fetched asynchronously and replace
    loading placeholders in place without redisplaying or reordering the buffer
  - opening a watched-channel Activity feed entry marks that channel read and
    clears the local Activity unread state for the entry after Slack confirms
    the mark
  - opening an already-read watched-channel entry never moves the channel's
    server-side read cursor backward (which would mark newer messages unread
    in every Slack client)
  - press <kbd>t</kbd> on an Activity feed thread entry to open the associated
    thread
  - opening a thread entry whose parent message is already loaded in the
    channel opens the thread correctly even when the cached parent lacks
    thread data
  - read Activity feed entries stay read in the local cache, so closing and
    reopening the feed does not bring back cleared unread dots
  - event-driven background refreshes update cached data without silently
    redrawing the visible Activity feed
  - Activity unread-summary refreshes also merge unread Activity rows into
    existing cached feed data, so opening the feed is less likely to show stale
    cached results after new notifications arrive
  - background unread-summary refreshes and event-driven cache refreshes store
    their snapshots under the feed mode they fetched, so the all-mode feed no
    longer loses read items (or its pagination cursor) every refresh cycle
  - the visible Activity feed is refreshed when the feed is shown or when
    <kbd>g</kbd> is pressed
  - `slack-activity-feed-watch-channel-limit` controls how many recent
    messages are fetched from each watched channel
  - entries can be channel names without `#` or channel IDs such as
    `C1234567890`
- `slack-saved-items`
  - show saved messages and cache message data returned by Slack so the buffer
    can render newly fetched saved items immediately
  - saved files render as entries (previously every file-type saved item was
    silently invisible)
  - press <kbd>RET</kbd> on a saved item's thread-status link ("N replies, Last
    reply ...") to open the associated thread
- `slack-room-pins-list`
  - press <kbd>RET</kbd> on a pinned message's thread-status link ("N replies,
    Last reply ...") to open the associated thread
- `slack-search-from-messages`
  - press <kbd>RET</kbd> on a search result's thread-status link ("N replies,
    Last reply ...") to open the associated thread
- `slack-message-embed-mention`
  - use to mention to user
- `slack-message-embed-channel`
  - use to mention to channel
- `slack-message-edit`
  - opens an edit buffer for the message at point, including messages shown in
    thread buffers
  - the edit is sent even when the original message has dropped out of the
    local message cache
- Compose, edit, and share buffers (<kbd>C-c C-c</kbd> to send)
  - attachments can be sent without any message text
  - the buffer closes only after Slack confirms the send; if the send fails
    (network error, archived channel, rejected message), the buffer stays
    open with your text intact and the error is shown in the echo area
- Message formatting on send
  - emoji shortcodes, links, inline code, and mentions inside `*bold*`,
    `_italic_`, and `~strike~` spans encode correctly instead of
    duplicating the span text or sending the mention as literal
    `<@USERID>` text
  - only `:shortcodes:` naming a known emoji (standard or team custom)
    are sent as emoji, so times like `12:30:45` stay plain text, and
    `:+1:` is now recognized
- Quoted Slack messages
  - press <kbd>RET</kbd> on a quoted/shared message to open the original
    message in emacs-slack
  - quoted-message permalinks can open messages from public channels, private
    channels, and direct messages
  - if emacs-slack has not loaded the target team or channel, the permalink
    opens in the default browser instead of raising an error
- `slack-file-upload`
  - uploads a file
  - the command allows to choose many channels via select loop. In order to finish the loop input an empty string. For helm that's <kbd>C+RET</kbd> or <kbd>M+TET</kbd>. In case of Ivy it's <kbd>C+M+j</kbd>.
- File info and file lists
  - re-opening a file's info and loading file-list pages that contain
    already-cached files merge correctly instead of erroring inside the
    request callback
- File and image downloads
  - downloading to an existing path asks for confirmation before
    overwriting
  - downloads are written to a temporary sibling file and renamed into
    place on success, so a failed download never deletes or corrupts an
    existing file at the target path

- Reliability fixes
  - synchronous API requests run their success/error handlers, so the
    standard-emoji picker data, attachment action suggestions, and
    dialog external-select suggestions load correctly
  - rate-limited (HTTP 429) requests honor no-retry and the retry cap
    instead of requeueing forever, and a malformed Retry-After header
    no longer causes immediate re-execution
  - the retry queue no longer silently drops a second identical
    request from a different feature (whose callbacks would never
    have fired)
  - reconnection after a long disconnect (laptop sleep) falls back to
    a fresh authorization instead of retrying Slack's expired
    reconnect URL for hours
  - typing indicators no longer leak repeating timers when people type
    in different channels in quick succession (the leaked timers
    erased the echo area every second for the rest of the session)
  - websocket URLs are logged with the workspace token redacted, so
    setting `slack-log-level` to `debug` no longer writes the
    credential to the log buffer
  - a failed attachment upload reports the failure once and stops,
    instead of erroring from a leaked once-per-second timer until
    Emacs restarts
  - adding or removing a reaction works even when the target message
    has dropped out of the local message cache (previously the emoji
    prompt appeared and then nothing happened)
  - removing a reaction updates the display immediately like adding
    one does, and sharing a message without a comment omits the
    comment payload instead of sending empty blocks
  - running `slack-start` while a previous authorization is pending
    reports "Authorize Already Requested" instead of signaling a type
    error
  - teams registered with cookie-less tokens (xoxp/xoxb) can connect:
    `slack-start` no longer crashes trying to parse a nil cookie
  - `slack-start-and-select` starts disconnected teams again (the
    disconnected check always came back empty, so it skipped straight
    to channel selection)
  - stopping Slack while a connection attempt is pending no longer
    leaves a stale connect-timeout timer that later errors (or pops
    the debugger) against the shut-down connection
  - the do-not-disturb ("Z") indicator works from the bulk DND fetch
    at connect time, not only after a live dnd_updated event
  - the sender-name tooltip shows the user's actual local time (the
    old arithmetic was wrong by hours) and no longer errors when
    hovering bot messages
  - thread fetches no longer send `oldest=nil`, duplicate `oldest`
    parameters, or junk `nil=nil` pairs to conversations.replies
  - partial thread fetches (loading messages missed while
    disconnected, pagination) merge into the thread's reply list
    instead of replacing it, so reopening a thread no longer shows
    only the most recently fetched slice
  - a live reply arriving in a partially loaded thread no longer
    advances the read mark past the unfetched replies, which both
    marked them read on the server and made load-more skip them
    forever
  - opening a permalink to a message far outside loaded history
    paginates correctly: the forward "(load more)" advances through
    the gap instead of refetching the same page and falsely reporting
    "(no more messages)", and the two-direction fetch no longer races
    against itself dropping one direction's messages
  - looking up a thread reply by timestamp fetches the requested
    reply rather than silently returning the thread parent, and
    tolerates rooms missing from the local cache
  - <kbd>p</kbd> in the Activity feed and All threads buffers lands on
    entry starts, so <kbd>RET</kbd> after moving backward opens the
    entry instead of saying "No message at point"
  - a failed or rate-limited history request no longer permanently
    disables "(load more)" in thread and feed buffers, and a buffer
    killed while the request is in flight stays dead instead of being
    re-created by the response (channel buffers included)
  - loading more threads in the All threads buffer restores point in
    that buffer instead of moving point in whatever buffer the
    response callback happened to land in
  - with two teams sharing a channel name, the unread "New Message"
    marker and first-open positioning operate on the right team's
    buffer instead of whichever buffer claimed the name first
  - messages containing Block Kit image blocks without size metadata
    (common for bot/app posts) render instead of aborting the whole
    message
  - custom emoji aliases whose target name contains the substring
    "alias" resolve correctly on the emojify (Emacs < 29) path
  - Slack-app dialogs with grouped select options work: selecting an
    option no longer signals, and the first press of an " Edit "
    button shows the edit buffer instead of an unrelated buffer
  - the saved-items buffer stops paginating at the last page instead
    of re-fetching page one forever and appending duplicate items
  - opening a thread from the pinned-items buffer works (the wrapper
    object was passed where the message was expected)
  - the pinned-items list opens even when the channel contains legacy
    or unknown pin types (they are skipped instead of crashing the
    response handler)
  - `slack-file-update` refreshes the file-info buffer it was invoked
    from (it previously sent a malformed request and updated only the
    file-list buffer)
  - `slack-scheduled-messages-show` reuses one buffer per team instead
    of accumulating duplicate "Scheduled Msgs<2>", "<3>", … buffers on
    every show or refresh
  - thread replies no longer re-count the parent's @-mention or flag
    the channel unread on every reply, so the modeline mention badge
    stops climbing in active threads
  - notification-eligible messages with empty text (block-only bot
    messages) notify without erroring, so their unread bookkeeping is
    no longer silently lost
  - a reaction on a thread reply that is not in the local cache no
    longer shows up on an unrelated older channel message
  - custom notification predicates honor `slack-notify-keep` (the
    persistent variant really persists), and one-shot predicates are
    no longer consumed by your own messages or by background history
    scans without an alert ever firing

### Tip

If your Slack team has a huge number of public channels, you may find
you hit your token rate limit. If that happens, set `(setq
slack-quick-update t)`: it will avoid the hit of rate limiting at
expense of functionality (like you will not have all public channel
available and search will not display room names when it doesn't know
about them).

## Notification

See [alert](https://github.com/jwiegley/alert).

## Extensions

- [helm-slack](https://github.com/yuya373/helm-slack)
- [ol-emacs-slack](https://github.com/ag91/ol-emacs-slack) 
  I use this to add slack messages to my Org Agenda
## How to debug

Please set `(setq slack-log-level 'debug)` to see useful messages that
may help pin point your issue. If you plan to add that to an issue,
make sure to edit out credentials. 

You can also use `(setq slack-log-level 'trace)`, but that only if you
need it because it will overwhelm your minibuffer with messages.

## Contributing

Please open an issue or better pull request for things that trouble you. 
## Dependencies

- [Alert](https://github.com/jwiegley/alert)
- [circe](https://github.com/jorgenschaefer/circe) (for the Linewise User
  Interface library).
- [Emojify](https://github.com/iqbalansari/emacs-emojify) (optional)
- [Oauth2](https://github.com/emacsmirror/oauth2/blob/master/oauth2.el)
  - do `package install`
- [request](https://github.com/tkf/emacs-request)
- [websocket](https://github.com/ahyatt/emacs-websocket)

## Similar projects

- https://github.com/wee-slack/wee-slack
