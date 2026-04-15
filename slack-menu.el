;;; slack-menu.el --- Transient menu for slack  -*- lexical-binding: t; -*-

;; Copyright (C) 2026

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <https://www.gnu.org/licenses/>.

;;; Commentary:

;; Transient-based command menu for emacs-slack.

;;; Code:

(require 'transient)

;;;###autoload (autoload 'slack-menu "slack-menu" nil t)
(transient-define-prefix slack-menu ()
  "Main menu for emacs-slack."
  [["Message"
    ("m t" "open thread" slack-thread-show-or-create)
    ("m r" "reply" slack-thread-reply)
    ("m q" "quote & reply" slack-quote-and-reply)
    ("m e" "edit" slack-message-edit)
    ("m d" "delete" slack-message-delete)
    ("m h" "share" slack-message-share)
    ("m u" "toggle subscribe" slack-thread-toggle-subscription)
    ("m k" "copy link" slack-message-copy-link)
    ("m i" "copy id" slack-message-copy-id)
    ("m s" "save for later" slack-message-save-for-later)
    ("m b" "jump to browser" slack-jump-to-browser)
    ("m ." "redisplay" slack-message-redisplay)
    ""
    "Compose"
    ("w c" "compose in buffer" slack-message-write-another-buffer)
    ("w y" "yank code block" slack-yank-code-block)
    ("w e" "insert emoji" slack-insert-emoji)
    ("w m" "embed mention" slack-message-embed-mention)
    ("w #" "embed channel" slack-message-embed-channel)]
   ["Navigate"
    ("n c" "channel" slack-channel-select)
    ("n g" "group" slack-group-select)
    ("n d" "direct message" slack-im-select)
    ("n r" "rooms" slack-select-rooms)
    ("n u" "unread rooms" slack-select-unread-rooms)
    ("n t" "all threads" slack-all-threads)
    ("n a" "activity feed" slack-activity-feed-show)
    ("n s" "saved items" slack-saved-items)
    ("n m" "scheduled messages" slack-scheduled-messages-show)
    ""
    "Search"
    ("s m" "messages" slack-search-from-messages)
    ("s f" "files" slack-search-from-files)]
   ["React"
    ("r a" "add" slack-message-add-reaction)
    ("r r" "remove" slack-message-remove-reaction)
    ""
    "Pin"
    ("p p" "pin" slack-message-pins-add)
    ("p u" "unpin" slack-message-pins-remove)
    ("p l" "list" slack-room-pins-list)
    ""
    "File"
    ("f u" "upload" slack-file-upload)
    ("f q" "quick upload" slack-file-upload-quick)
    ("f s" "upload snippet" slack-file-upload-snippet)
    ("f d" "download at point" slack-download-file-at-point)
    ("f c" "clipboard image" slack-clipboard-image-upload)]
   ["Channel"
    ("c c" "create" slack-create-channel)
    ("c j" "join" slack-channel-join)
    ("c l" "leave" slack-channel-leave)
    ("c t" "set topic" slack-channel-set-topic)
    ("c i" "info" slack-show-channel-info)
    ("c b" "bookmarks" slack-show-channel-bookmarks)
    ("c h" "huddle" slack-join-huddle)
    ""
    "Team & connection"
    ("t c" "change team" slack-change-current-team)
    ("t s" "start" slack-start-and-select)
    ("t q" "stop" slack-disconnect-all)
    ("t +" "set status" slack-user-set-status)
    ("t -" "reset status" slack-user-reset-status)
    ("t b" "open in browser" slack-jump-to-browser)
    ("t k" "kill all buffers" slack-kill-all-buffers)]])

(provide 'slack-menu)
;;; slack-menu.el ends here
