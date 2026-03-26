;;; slack-reminder.el ---                            -*- lexical-binding: t; -*-

;; Copyright (C) 2016  南優也

;; Author: 南優也 <yuyaminami@minamiyuuya-no-MacBook.local>
;; Keywords:

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <http://www.gnu.org/licenses/>.

;;; Commentary:

;; Save-for-later with due dates using the saved.add API.
;; The old reminders API was deprecated by Slack in July 2023:
;; https://api.slack.com/changelog/2023-07-its-later-already-for-stars-and-reminders

;;; Code:

(require 'eieio)
(require 'slack-util)
(require 'slack-room)
(require 'slack-team)
(require 'slack-request)
(require 'slack-message)
(require 'slack-star)

(defvar slack-completing-read-function)

(cl-defun slack-reminder-add-from-message (_room message team)
  "Save MESSAGE for later with a due date, using the saved.add API.
Prompts for a reminder time and saves via TEAM."
  (let* ((time (funcall slack-completing-read-function
                        "When: "
                        '("In 20 minutes"
                          "In 1 hour"
                          "In 3 hours"
                          "Tomorrow"
                          "Next week")
                        nil t))
         (due-in-ms (cond
                     ((string= time "In 20 minutes") (* 20 60 1000))
                     ((string= time "In 1 hour") (* 60 60 1000))
                     ((string= time "In 3 hours") (* 3 60 60 1000))
                     ((string= time "Tomorrow") (* 24 60 60 1000))
                     (t (* 7 24 60 60 1000)))))
    (slack-star-api-request slack-message-stars-add-url
                            (append (list (cons "channel" (oref message channel)))
                                    (slack-message-star-api-params message due-in-ms))
                            team)))

(provide 'slack-reminder)
;;; slack-reminder.el ends here
