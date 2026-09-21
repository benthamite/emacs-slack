;;; test-quoted-message.el --- Quoted message rendering tests -*- lexical-binding: t; -*-

(require 'ert)
(require 'slack)
(require 'slack-message-formatter)

(ert-deftest slack-quoted-message-renders-one-body ()
  (dolist (case '((t t nil "quoted body")
                  (t nil nil "quoted body")
                  (nil t nil "quoted body")
                  (t t t "quoted body")))
    (pcase-let* ((`(,blocks ,text ,disabled ,expected) case)
                 (team (make-instance 'slack-team :disable-block-format disabled))
                 (url "https://example.slack.com/archives/C1/p1700000000000000")
                 (attachment
                  (slack-attachment-create
                   (list :is_share t :from_url url
                         :text (and text "quoted body")
                         :mrkdwn_in '("text")
                         :blocks (and blocks
                                      (list (list :type "section"
                                                  :text (list :type "mrkdwn"
                                                              :text "quoted body")))))))
                 (rendered (slack-message-to-string attachment team))
                 (pos (string-match expected rendered)))
      (should pos)
      (should-not (string-match expected rendered (+ pos (length expected))))
      (should (equal url (get-text-property pos 'slack-shared-message-url rendered)))
      (should (eq slack-shared-message-keymap (get-text-property pos 'keymap rendered))))))

(ert-deftest slack-quoted-message-prefers-blocks-over-legacy-text ()
  (let* ((team (make-instance 'slack-team))
         (attachment (slack-attachment-create
                      '(:is_share t :text "legacy representation"
                        :blocks ((:type "section"
                                  :text (:type "mrkdwn" :text "formatted quote"))))))
         (rendered (slack-message-to-string attachment team)))
    (should (string-match-p "formatted quote" rendered))
    (should-not (string-match-p "legacy representation" rendered))))

(provide 'test-quoted-message)
;;; test-quoted-message.el ends here
