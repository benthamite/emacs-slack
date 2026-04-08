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
  [["Navigate"
    ("c" "channel" slack-channel-select)
    ("g" "group" slack-group-select)
    ("m" "direct message" slack-im-select)
    ("u" "rooms" slack-select-rooms)
    ("U" "unread rooms" slack-select-unread-rooms)
    ("a" "all threads" slack-all-threads)
    ("A" "activity feed" slack-activity-feed-show)
    ("*" "saved items" slack-saved-items)
    ("M" "scheduled messages" slack-scheduled-messages-show)]
   ["Message"
    ("d" "open thread" slack-thread-show-or-create)
    ("Z" "reply in thread" slack-thread-reply)
    ("W" "thread subscribe" slack-thread-toggle-subscription)
    ("e" "edit" slack-message-edit)
    ("D" "delete" slack-message-delete)
    ("s" "share" slack-message-share)
    ("l" "copy link" slack-message-copy-link)
    ("I" "copy id" slack-message-copy-id)
    ("Q" "quote & reply" slack-quote-and-reply)
    ("." "redisplay" slack-message-redisplay)]
   ["React & save"
    ("r" "add reaction" slack-message-add-reaction)
    ("R" "remove reaction" slack-message-remove-reaction)
    ("p" "pin" slack-message-pins-add)
    ("P" "unpin" slack-message-pins-remove)
    ("n" "pins list" slack-room-pins-list)
    ("B" "save for later" slack-message-save-for-later)]
   ["Search"
    ("/" "messages" slack-search-from-messages)
    ("\\" "files" slack-search-from-files)]]
  [["Compose"
    ("z" "compose in buffer" slack-message-write-another-buffer)
    ("y" "yank code block" slack-yank-code-block)
    ("E" "insert emoji" slack-insert-emoji)
    ("@" "embed mention" slack-message-embed-mention)
    ("#" "embed channel" slack-message-embed-channel)]
   ["File"
    ("f" "upload" slack-file-upload)
    ("F" "quick upload" slack-file-upload-quick)
    ("S" "upload snippet" slack-file-upload-snippet)
    ("x" "download at point" slack-download-file-at-point)
    ("X" "clipboard image" slack-clipboard-image-upload)]
   ["Channel"
    ("C" "create" slack-create-channel)
    ("j" "join" slack-channel-join)
    ("v" "leave" slack-channel-leave)
    ("t" "set topic" slack-channel-set-topic)
    ("i" "info" slack-show-channel-info)
    ("b" "bookmarks" slack-show-channel-bookmarks)
    ("h" "huddle" slack-join-huddle)]
   ["Team & connection"
    ("T" "change team" slack-change-current-team)
    ("o" "start" slack-start-and-select)
    ("O" "stop" slack-disconnect-all)
    ("+" "set status" slack-user-set-status)
    ("-" "reset status" slack-user-reset-status)
    ("w" "open in browser" slack-jump-to-browser)
    ("K" "kill all buffers" slack-kill-all-buffers)]])

(provide 'slack-menu)
;;; slack-menu.el ends here
