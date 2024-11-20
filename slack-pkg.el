(define-package "slack" "0"
  "Slack client for Emacs"
  '((websocket "1.8")
    (request "0.2.0")
    (oauth2 "0.10") ;; TODO not sure this is really needed
    (circe "2.2")
    (alert "1.2")
    (emojify "0.2")
    (emacs "25.1")
    (dash "2.19.1")
    (s "1.13.1")
    (ts "0.3"))
  :url "https://github.com/emacs-slack/emacs-slack")
