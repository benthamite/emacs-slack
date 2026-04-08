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
    ("a" "all threads" slack-all-threads)]
   ["Message"
    ("d" "thread" slack-thread-show-or-create)
    ("Z" "thread reply" slack-thread-reply)
    ("e" "edit" slack-message-edit)
    ("D" "delete" slack-message-delete)
    ("z" "compose in buffer" slack-message-write-another-buffer)
    ("s" "share" slack-message-share)
    ("l" "copy link" slack-message-copy-link)
    ("I" "copy id" slack-message-copy-id)
    ("Q" "quote & reply" slack-quote-and-reply)]
   ["React & pin"
    ("r" "add reaction" slack-message-add-reaction)
    ("R" "remove reaction" slack-message-remove-reaction)
    ("p" "pin" slack-message-pins-add)
    ("P" "unpin" slack-message-pins-remove)
    ("n" "pins list" slack-room-pins-list)]
   ["File"
    ("f" "upload" slack-file-upload)
    ("F" "quick upload" slack-file-upload-quick)
    ("S" "upload snippet" slack-file-upload-snippet)
    ("x" "download at point" slack-download-file-at-point)]]
  [["Search"
    ("/" "messages" slack-search-from-messages)
    ("\\" "files" slack-search-from-files)]
   ["Channel"
    ("C" "create" slack-create-channel)
    ("j" "join" slack-channel-join)
    ("v" "leave" slack-channel-leave)
    ("t" "set topic" slack-channel-set-topic)
    ("i" "info" slack-show-channel-info)
    ("b" "bookmarks" slack-show-channel-bookmarks)]
   ["Team & connection"
    ("T" "change team" slack-change-current-team)
    ("o" "start" slack-start-and-select)
    ("O" "stop" slack-disconnect-all)
    ("+" "set status" slack-user-set-status)
    ("-" "reset status" slack-user-reset-status)]
   ["Misc"
    ("*" "saved items" slack-saved-items)
    ("A" "activity feed" slack-activity-feed-show)
    ("M" "scheduled messages" slack-scheduled-messages-show)
    ("K" "kill all buffers" slack-kill-all-buffers)
    ("w" "open in browser" slack-jump-to-browser)
    ("y" "yank code block" slack-yank-code-block)]])

(provide 'slack-menu)
;;; slack-menu.el ends here
