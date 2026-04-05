;;; test-suite.el --- comprehensive emacs-slack tests  -*- lexical-binding: t; -*-

;; Tests for modules not covered by run-test.el: message creation dispatch,
;; room message storage, reaction operations, counts system, room finding,
;; team room management, utility functions, rate limiting, and permalink
;; round-trips.

(require 'ert)
(require 'slack-team)
(require 'slack-channel)
(require 'slack-group)
(require 'slack-im)
(require 'slack-room)
(require 'slack-usergroup)
(require 'slack-message)
(require 'slack-user-message)
(require 'slack-bot-message)
(require 'slack-create-message)
(require 'slack-reaction)
(require 'slack-counts)
(require 'slack-util)
(require 'slack-request)

(defvar slack-channel-button-keymap nil)

(defmacro slack-test-setup (&rest body)
  (declare (indent 0) (debug t))
  `(let* ((channel-id "C11111")
          (channel-name "TestChannel")
          (channel (make-instance 'slack-channel
                                  :id channel-id
                                  :name channel-name))
          (group-id "G22222")
          (group-name "TestGroup")
          (group (make-instance 'slack-group
                                :id group-id
                                :name group-name))
          (im-id "D33333")
          (im-user-id "U22222")
          (im (make-instance 'slack-im
                             :id im-id
                             :user im-user-id))
          (user-id "U11111")
          (user-name "TestUser")
          (real-name "RealName")
          (display-name "Display name")
          (user (list :name user-name :id user-id
                      :profile (list :display_name_normalized display-name
                                     :real_name_normalized real-name)))
          (usergroup-id "S88888")
          (usergroup-handle "TestUsergroup")
          (usergroup (make-instance 'slack-usergroup
                                    :id usergroup-id
                                    :handle usergroup-handle))
          (team (make-instance 'slack-team
                               :self-id "U38383838"
                               :channels (let ((h (make-hash-table :test 'equal)))
                                           (puthash channel-id channel h)
                                           h)
                               :groups (let ((h (make-hash-table :test 'equal)))
                                          (puthash group-id group h)
                                          h)
                               :ims (let ((h (make-hash-table :test 'equal)))
                                       (puthash im-id im h)
                                       h)
                               :users (let ((h (make-hash-table :test 'equal)))
                                        (puthash (plist-get user :id)
                                                 user h)
                                        h)
                               :usergroups (list usergroup))))
     ,@body))

;;; ---- Message creation dispatch ----

(ert-deftest slack-test-message-create-user-message ()
  (slack-test-setup
    (let* ((payload '(:type "message" :ts "1.0" :text "hello"
                      :user "U11111" :channel "C11111"))
           (msg (slack-message-create payload team)))
      (should (eq 'slack-user-message (eieio-object-class-name msg)))
      (should (string= "U11111" (oref msg user)))
      (should (string= "1.0" (oref msg ts)))
      (should (string= "hello" (oref msg text))))))

(ert-deftest slack-test-message-create-bot-message ()
  (slack-test-setup
    (let* ((payload '(:type "message" :subtype "bot_message"
                      :ts "2.0" :text "bot says hi"
                      :bot_id "B123" :channel "C11111"))
           (msg (slack-message-create payload team)))
      (should (eq 'slack-bot-message (eieio-object-class-name msg)))
      (should (string= "B123" (oref msg bot-id))))))

(ert-deftest slack-test-message-create-bot-by-bot-id-without-subtype ()
  (slack-test-setup
    (let* ((payload '(:type "message" :ts "2.1" :text "bot no subtype"
                      :bot_id "B456" :channel "C11111"))
           (msg (slack-message-create payload team)))
      (should (eq 'slack-bot-message (eieio-object-class-name msg))))))

(ert-deftest slack-test-message-create-reply-broadcast ()
  (slack-test-setup
    (let* ((payload '(:type "message" :subtype "thread_broadcast"
                      :ts "3.0" :text "broadcast"
                      :user "U11111" :channel "C11111"))
           (msg (slack-message-create payload team)))
      (should (eq 'slack-reply-broadcast-message
                  (eieio-object-class-name msg))))))

;; slack-reply class lives in slack-websocket.el which has heavy deps;
;; skip in batch mode.

(ert-deftest slack-test-message-create-fallback-to-base ()
  (slack-test-setup
    (let* ((payload '(:type "message" :ts "5.0" :text "mystery"
                      :channel "C11111"))
           (msg (slack-message-create payload team)))
      (should (eq 'slack-message (eieio-object-class-name msg))))))

(ert-deftest slack-test-message-create-nil-payload-returns-nil ()
  (slack-test-setup
    (should (null (slack-message-create nil team)))))

(ert-deftest slack-test-message-create-sets-reactions ()
  (slack-test-setup
    (let* ((payload '(:type "message" :ts "6.0" :text "reacted"
                      :user "U11111" :channel "C11111"
                      :reactions ((:name "thumbsup" :count 1
                                   :users ("U11111")))))
           (msg (slack-message-create payload team)))
      (should (eq 1 (length (oref msg reactions))))
      (should (string= "thumbsup"
                       (oref (car (oref msg reactions)) name))))))

(ert-deftest slack-test-message-create-sets-channel-from-room-object ()
  (slack-test-setup
    (let* ((payload '(:type "message" :ts "7.0" :text "from room"
                      :user "U11111"))
           (msg (slack-message-create payload team channel)))
      (should (string= channel-id (oref msg channel))))))

(ert-deftest slack-test-message-create-sets-channel-from-string ()
  (slack-test-setup
    (let* ((payload '(:type "message" :ts "8.0" :text "from string"
                      :user "U11111"))
           (msg (slack-message-create payload team "C99999")))
      (should (string= "C99999" (oref msg channel))))))

;;; ---- Message properties ----

(ert-deftest slack-test-message-equal ()
  (let ((m1 (make-instance 'slack-message :type "message" :ts "1.0"))
        (m2 (make-instance 'slack-message :type "message" :ts "1.0"))
        (m3 (make-instance 'slack-message :type "message" :ts "2.0")))
    (should (slack-message-equal m1 m2))
    (should-not (slack-message-equal m1 m3))))

(ert-deftest slack-test-message-edited-at ()
  (let ((msg (make-instance 'slack-message :type "message" :ts "1.0"
                            :edited (make-instance 'slack-message-edited
                                                   :user "U1" :ts "2.0"))))
    (should (string= "2.0" (slack-message-edited-at msg))))
  (let ((msg (make-instance 'slack-message :type "message" :ts "1.0")))
    (should (null (slack-message-edited-at msg)))))

;;; ---- Room message storage ----

(ert-deftest slack-test-room-push-and-find-message ()
  (slack-test-setup
    (let ((msg (make-instance 'slack-message :type "message" :ts "100.0")))
      (slack-room-push-message channel msg team)
      (should (eq msg (slack-room-find-message channel "100.0")))
      (should (member "100.0" (oref channel message-ids))))))

(ert-deftest slack-test-room-push-maintains-sort-order ()
  (slack-test-setup
    (let ((m1 (make-instance 'slack-message :type "message" :ts "300.0"))
          (m2 (make-instance 'slack-message :type "message" :ts "100.0"))
          (m3 (make-instance 'slack-message :type "message" :ts "200.0")))
      (slack-room-push-message channel m1 team)
      (slack-room-push-message channel m2 team)
      (slack-room-push-message channel m3 team)
      (should (equal '("100.0" "200.0" "300.0")
                     (oref channel message-ids))))))

(ert-deftest slack-test-room-push-deduplicates ()
  (slack-test-setup
    (let ((m1 (make-instance 'slack-message :type "message" :ts "1.0"))
          (m2 (make-instance 'slack-message :type "message" :ts "1.0")))
      (slack-room-push-message channel m1 team)
      (slack-room-push-message channel m2 team)
      (should (eq 1 (length (oref channel message-ids)))))))

(ert-deftest slack-test-room-delete-message ()
  (slack-test-setup
    (let ((msg (make-instance 'slack-message :type "message" :ts "1.0")))
      (slack-room-push-message channel msg team)
      (slack-room-delete-message channel "1.0")
      (should (null (slack-room-find-message channel "1.0")))
      (should (null (member "1.0" (oref channel message-ids)))))))

(ert-deftest slack-test-room-delete-nonexistent-is-safe ()
  (slack-test-setup
    (slack-room-delete-message channel "999.0")
    (should (null (oref channel message-ids)))))

(ert-deftest slack-test-room-clear-messages ()
  (slack-test-setup
    (let ((m1 (make-instance 'slack-message :type "message" :ts "1.0"))
          (m2 (make-instance 'slack-message :type "message" :ts "2.0")))
      (slack-room-push-message channel m1 team)
      (slack-room-push-message channel m2 team)
      (slack-room-clear-messages channel)
      (should (eq 0 (hash-table-count (oref channel messages))))
      (should (null (oref channel message-ids))))))

(ert-deftest slack-test-room-set-messages ()
  (slack-test-setup
    (let ((m1 (make-instance 'slack-message :type "message" :ts "1.0"))
          (m2 (make-instance 'slack-message :type "message" :ts "2.0"))
          (m3 (make-instance 'slack-message :type "message" :ts "3.0")))
      (slack-room-set-messages channel (list m1 m2 m3) team)
      (should (eq 3 (hash-table-count (oref channel messages))))
      (should (equal '("1.0" "2.0" "3.0")
                     (oref channel message-ids))))))

(ert-deftest slack-test-room-sorted-messages ()
  (slack-test-setup
    (let ((m1 (make-instance 'slack-message :type "message" :ts "1.0"))
          (m2 (make-instance 'slack-message :type "message" :ts "2.0"))
          (m3 (make-instance 'slack-message :type "message" :ts "3.0")))
      (slack-room-set-messages channel (list m3 m1 m2) team)
      (let ((sorted (slack-room-sorted-messages channel)))
        (should (eq 3 (length sorted)))
        (should (string= "1.0" (oref (nth 0 sorted) ts)))
        (should (string= "2.0" (oref (nth 1 sorted) ts)))
        (should (string= "3.0" (oref (nth 2 sorted) ts)))))))

(ert-deftest slack-test-room-trim-messages ()
  (slack-test-setup
    (dotimes (i 10)
      (let ((msg (make-instance 'slack-message
                                :type "message"
                                :ts (format "%d.0" i))))
        (slack-room-push-message channel msg team)))
    (slack-room-trim-messages channel 3)
    (should (eq 3 (hash-table-count (oref channel messages))))
    (should (eq 3 (length (oref channel message-ids))))
    (should (equal '("7.0" "8.0" "9.0")
                   (oref channel message-ids)))))

(ert-deftest slack-test-room-trim-noop-when-under-limit ()
  (slack-test-setup
    (let ((msg (make-instance 'slack-message :type "message" :ts "1.0")))
      (slack-room-push-message channel msg team)
      (slack-room-trim-messages channel 100)
      (should (eq 1 (length (oref channel message-ids)))))))

;;; ---- Reaction operations ----

(ert-deftest slack-test-reaction-equalp ()
  (let ((r1 (make-instance 'slack-reaction :name "thumbsup" :count 1))
        (r2 (make-instance 'slack-reaction :name "thumbsup" :count 2))
        (r3 (make-instance 'slack-reaction :name "heart" :count 1)))
    (should (slack-reaction-equalp r1 r2))
    (should-not (slack-reaction-equalp r1 r3))))

(ert-deftest slack-test-reaction-join-same-name ()
  (let ((r1 (make-instance 'slack-reaction
                           :name "thumbsup" :count 1
                           :users '("U1")))
        (r2 (make-instance 'slack-reaction
                           :name "thumbsup" :count 1
                           :users '("U2"))))
    (let ((result (slack-reaction-join r1 r2)))
      (should result)
      (should (eq 2 (oref result count)))
      (should (equal '("U2" "U1") (oref result users))))))

(ert-deftest slack-test-reaction-join-different-name-returns-nil ()
  (let ((r1 (make-instance 'slack-reaction :name "thumbsup" :count 1))
        (r2 (make-instance 'slack-reaction :name "heart" :count 1)))
    (should (null (slack-reaction-join r1 r2)))))

(ert-deftest slack-test-reaction-remove-user ()
  (let ((r (make-instance 'slack-reaction
                          :name "thumbsup" :count 2
                          :users '("U1" "U2"))))
    (slack-reaction-remove-user r "U1")
    (should (eq 1 (oref r count)))
    (should (equal '("U2") (oref r users)))))

(ert-deftest slack-test-reaction-remove-user-not-present ()
  (let ((r (make-instance 'slack-reaction
                          :name "thumbsup" :count 1
                          :users '("U1"))))
    (slack-reaction-remove-user r "U999")
    (should (eq 1 (oref r count)))
    (should (equal '("U1") (oref r users)))))

(ert-deftest slack-test-reaction-delete ()
  (let ((target (make-instance 'slack-reaction :name "heart" :count 1))
        (reactions (list (make-instance 'slack-reaction
                                       :name "thumbsup" :count 1)
                         (make-instance 'slack-reaction
                                       :name "heart" :count 1)
                         (make-instance 'slack-reaction
                                       :name "smile" :count 1))))
    (let ((result (slack-reaction-delete target reactions)))
      (should (eq 2 (length result)))
      (should-not (cl-find-if (lambda (r)
                                (string= "heart" (oref r name)))
                              result)))))

(ert-deftest slack-test-reaction-find ()
  (let ((target (make-instance 'slack-reaction :name "heart" :count 1))
        (reactions (list (make-instance 'slack-reaction
                                       :name "thumbsup" :count 1)
                         (make-instance 'slack-reaction
                                       :name "heart" :count 3))))
    (let ((found (slack-reaction--find reactions target)))
      (should found)
      (should (eq 3 (oref found count))))))

(ert-deftest slack-test-reaction-merge ()
  (let ((old (make-instance 'slack-reaction
                            :name "thumbsup" :count 1
                            :users '("U1")))
        (new (make-instance 'slack-reaction
                            :name "thumbsup" :count 2
                            :users '("U1" "U2"))))
    (slack-merge old new)
    (should (eq 2 (oref old count)))
    (should (equal '("U1" "U2") (oref old users)))))

(ert-deftest slack-test-reaction-user-reacted-p ()
  (let ((r (make-instance 'slack-reaction
                          :name "thumbsup" :count 2
                          :users '("U1" "U2"))))
    (should (slack-reaction-user-reacted-p r "U1"))
    (should-not (slack-reaction-user-reacted-p r "U999"))))

;;; ---- Counts system ----

(ert-deftest slack-test-create-counts ()
  (let ((counts (slack-create-counts
                 '(:threads (:has_unreads t :mention_count 3)
                   :channels ((:id "C1" :has_unreads t
                               :mention_count 2 :latest "100.0")
                              (:id "C2" :has_unreads nil
                               :mention_count 0 :latest "50.0"))
                   :mpims ()
                   :ims ((:id "D1" :has_unreads t
                          :mention_count 1 :latest "200.0"))))))
    (should (eq t (oref (oref counts threads) has-unreads)))
    (should (eq 3 (oref (oref counts threads) mention-count)))
    (should (eq 2 (length (oref counts channels))))
    (should (eq 1 (length (oref counts ims))))
    (should (null (oref counts mpims)))))

(ert-deftest slack-test-counts-channel-unread-and-mention ()
  (let* ((ch (make-instance 'slack-channel :id "C1" :name "test"))
         (counts (slack-create-counts
                  '(:threads (:has_unreads nil :mention_count 0)
                    :channels ((:id "C1" :has_unreads t
                                :mention_count 5 :latest "100.0"))
                    :mpims () :ims ()))))
    (should (eq t (slack-counts-channel-unread-p counts ch)))
    (should (eq 5 (slack-counts-channel-mention-count counts ch)))))

(ert-deftest slack-test-counts-channel-unknown-returns-zero ()
  (let* ((ch (make-instance 'slack-channel :id "C999" :name "unknown"))
         (counts (slack-create-counts
                  '(:threads (:has_unreads nil :mention_count 0)
                    :channels () :mpims () :ims ()))))
    (should (eq 0 (slack-counts-channel-mention-count counts ch)))))

(ert-deftest slack-test-counts-set-mention-count ()
  (let* ((ch (make-instance 'slack-channel :id "C1" :name "test"))
         (counts (slack-create-counts
                  '(:threads (:has_unreads nil :mention_count 0)
                    :channels ((:id "C1" :has_unreads nil
                                :mention_count 0 :latest "1.0"))
                    :mpims () :ims ()))))
    (slack-counts-channel-set-mention-count counts ch 42)
    (should (eq 42 (slack-counts-channel-mention-count counts ch)))))

(ert-deftest slack-test-counts-set-has-unreads ()
  (let* ((ch (make-instance 'slack-channel :id "C1" :name "test"))
         (counts (slack-create-counts
                  '(:threads (:has_unreads nil :mention_count 0)
                    :channels ((:id "C1" :has_unreads nil
                                :mention_count 0 :latest "1.0"))
                    :mpims () :ims ()))))
    (should-not (slack-counts-channel-unread-p counts ch))
    (slack-counts-channel-set-has-unreads counts ch t)
    (should (eq t (slack-counts-channel-unread-p counts ch)))))

(ert-deftest slack-test-counts-update-latest ()
  (let* ((ch (make-instance 'slack-channel :id "C1" :name "test"))
         (counts (slack-create-counts
                  '(:threads (:has_unreads nil :mention_count 0)
                    :channels ((:id "C1" :has_unreads nil
                                :mention_count 0 :latest "100.0"))
                    :mpims () :ims ()))))
    (slack-counts-channel-update-latest counts ch "200.0")
    (should (string= "200.0" (slack-counts-channel-latest counts ch)))))

(ert-deftest slack-test-counts-update-latest-ignores-older ()
  (let* ((ch (make-instance 'slack-channel :id "C1" :name "test"))
         (counts (slack-create-counts
                  '(:threads (:has_unreads nil :mention_count 0)
                    :channels ((:id "C1" :has_unreads nil
                                :mention_count 0 :latest "200.0"))
                    :mpims () :ims ()))))
    (slack-counts-channel-update-latest counts ch "100.0")
    (should (string= "200.0" (slack-counts-channel-latest counts ch)))))

(ert-deftest slack-test-counts-im-operations ()
  (let* ((dm (make-instance 'slack-im :id "D1" :user "U1"))
         (counts (slack-create-counts
                  '(:threads (:has_unreads nil :mention_count 0)
                    :channels ()
                    :mpims ()
                    :ims ((:id "D1" :has_unreads t
                           :mention_count 3
                           :latest "1710000050.000000"))))))
    (should (eq t (slack-counts-im-unread-p counts dm)))
    (should (eq 3 (slack-counts-im-mention-count counts dm)))
    (slack-counts-im-set-mention-count counts dm 0)
    (should (eq 0 (slack-counts-im-mention-count counts dm)))
    (slack-counts-im-update-latest counts dm "1710000100.000000")
    (should (string= "1710000100.000000"
                     (slack-counts-im-latest counts dm)))))

(ert-deftest slack-test-counts-summary ()
  (let ((counts (slack-create-counts
                 '(:threads (:has_unreads t :mention_count 1)
                   :channels ((:id "C1" :has_unreads t
                               :mention_count 2 :latest "1.0")
                              (:id "C2" :has_unreads nil
                               :mention_count 0 :latest "1.0"))
                   :mpims ((:id "G1" :has_unreads t
                            :mention_count 1 :latest "1.0"))
                   :ims ((:id "D1" :has_unreads nil
                          :mention_count 0 :latest "1.0"))))))
    (let ((summary (slack-counts-summary counts)))
      (should (eq 4 (length summary)))
      (let ((thread-entry (assoc 'thread summary))
            (channel-entry (assoc 'channel summary))
            (mpim-entry (assoc 'mpim summary)))
        (should (eq t (cadr thread-entry)))
        (should (eq 1 (cddr thread-entry)))
        (should (eq t (cadr channel-entry)))
        (should (eq 2 (cddr channel-entry)))
        (should (eq t (cadr mpim-entry)))
        (should (eq 1 (cddr mpim-entry)))))))

;;; ---- Room finding ----

(ert-deftest slack-test-room-find-channel ()
  (slack-test-setup
    (should (eq channel (slack-room-find channel-id team)))))

(ert-deftest slack-test-room-find-group ()
  (slack-test-setup
    (should (eq group (slack-room-find group-id team)))))

(ert-deftest slack-test-room-find-im ()
  (slack-test-setup
    (should (eq im (slack-room-find im-id team)))))

(ert-deftest slack-test-room-find-nonexistent ()
  (slack-test-setup
    (should (null (slack-room-find "X99999" team)))))

(ert-deftest slack-test-room-find-nil-id ()
  (slack-test-setup
    (should (null (slack-room-find nil team)))))

(ert-deftest slack-test-room-equal-p ()
  (let ((r1 (make-instance 'slack-channel :id "C1" :name "a"))
        (r2 (make-instance 'slack-channel :id "C1" :name "b"))
        (r3 (make-instance 'slack-channel :id "C2" :name "a")))
    (should (slack-room-equal-p r1 r2))
    (should-not (slack-room-equal-p r1 r3))))

;;; ---- Room merge ----

(ert-deftest slack-test-room-merge-preserves-messages ()
  (slack-test-setup
    (let ((msg (make-instance 'slack-message :type "message" :ts "1.0")))
      (slack-room-push-message channel msg team)
      (let ((other (make-instance 'slack-channel
                                  :id channel-id
                                  :name "UpdatedName"
                                  :unread_count 5
                                  :last_read "99.0")))
        (slack-merge channel other)
        (should (eq msg (slack-room-find-message channel "1.0")))
        (should (eq 5 (oref channel unread-count)))
        (should (string= "99.0" (oref channel last-read)))))))

(ert-deftest slack-test-room-merge-skips-zero-last-read ()
  (slack-test-setup
    (oset channel last-read "50.0")
    (let ((other (make-instance 'slack-channel
                                :id channel-id
                                :name channel-name
                                :last_read "0")))
      (slack-merge channel other)
      (should (string= "50.0" (oref channel last-read))))))

(ert-deftest slack-test-room-merge-handles-nil-created ()
  (slack-test-setup
    (let ((other (make-instance 'slack-channel
                                :id channel-id
                                :name "Updated")))
      (slack-merge channel other)
      (should (null (oref channel created))))))

;;; ---- Team room management ----

(ert-deftest slack-test-team-set-channels-adds-new ()
  (slack-test-setup
    (let ((new-ch (make-instance 'slack-channel
                                 :id "C99999" :name "NewChan")))
      (slack-team-set-channels team (list new-ch))
      (should (eq new-ch (gethash "C99999" (oref team channels))))
      (should (eq channel (gethash channel-id (oref team channels)))))))

(ert-deftest slack-test-team-set-channels-merges-existing ()
  (slack-test-setup
    (let ((updated (make-instance 'slack-channel
                                  :id channel-id
                                  :name "Updated"
                                  :unread_count 10)))
      (slack-team-set-channels team (list updated))
      (let ((stored (gethash channel-id (oref team channels))))
        (should (eq stored channel))
        (should (eq 10 (oref stored unread-count)))))))

(ert-deftest slack-test-team-set-room-dispatches-by-class ()
  (slack-test-setup
    (let ((new-im (make-instance 'slack-im :id "D99999" :user "U999")))
      (slack-team-set-room team new-im)
      (should (eq new-im (gethash "D99999" (oref team ims)))))))

(ert-deftest slack-test-team-set-groups ()
  (slack-test-setup
    (let ((new-grp (make-instance 'slack-group
                                  :id "G99999" :name "NewGroup")))
      (slack-team-set-groups team (list new-grp))
      (should (eq new-grp (gethash "G99999" (oref team groups)))))))

;;; ---- Utility functions ----

(ert-deftest slack-test-slack-string-blankp ()
  (should (slack-string-blankp nil))
  (should (slack-string-blankp ""))
  (should (slack-string-blankp "   "))
  (should (slack-string-blankp "\t\n"))
  (should-not (slack-string-blankp "a"))
  (should-not (slack-string-blankp " a ")))

(ert-deftest slack-test-slack-linkfy ()
  (should (string= "<http://example.com|text>"
                   (slack-linkfy "text" "http://example.com")))
  (should (string= "text" (slack-linkfy "text" "")))
  (should (string= "text" (slack-linkfy "text" nil)))
  (should (string= "text" (slack-linkfy "text" "   "))))

(ert-deftest slack-test-slack-decode ()
  (should (equal '("hello" "world") (slack-decode '("hello" "world"))))
  (should (equal '(42) (slack-decode '(42))))
  (should (equal nil (slack-decode nil))))

(ert-deftest slack-test-slack-seq-to-list ()
  (should (equal '(1 2 3) (slack-seq-to-list '(1 2 3))))
  (should (equal '(1 2 3) (slack-seq-to-list [1 2 3])))
  (should (equal nil (slack-seq-to-list nil))))

(ert-deftest slack-test-slack-merge-plist ()
  (should (equal '(:a 1 :b 2)
                 (slack-merge-plist '(:a 1) '(:b 2))))
  (should (equal '(:a 2)
                 (slack-merge-plist '(:a 1) '(:a 2))))
  (should (equal '(:a 1 :b 2 :c 3)
                 (slack-merge-plist '(:a 1) '(:b 2) '(:c 3))))
  (should (equal '(:a 3)
                 (slack-merge-plist '(:a 1) '(:a 2) '(:a 3)))))

(ert-deftest slack-test-slack-format-ts ()
  (should (null (slack-format-ts nil)))
  (should (stringp (slack-format-ts "1726244896")))
  (should (stringp (slack-format-ts 1726244896))))

(ert-deftest slack-test-slack-format-message ()
  (should (string= "a\nb" (slack-format-message "a" "b")))
  (should (string= "a" (slack-format-message "a" nil "")))
  (should (string= "a" (slack-format-message nil "a" ""))))

(ert-deftest slack-test-collect-slots ()
  (let ((result (slack-collect-slots
                 'slack-channel
                 '(:id "C1" :name "test" :bogus "ignored"))))
    (should (string= "C1" (plist-get result :id)))
    (should (string= "test" (plist-get result :name)))
    (should-not (plist-member result :bogus))))

(ert-deftest slack-test-collect-slots-converts-json-false ()
  (let ((result (slack-collect-slots
                 'slack-group
                 '(:id "G1" :name "test" :is_archived :json-false))))
    (should (null (plist-get result :is_archived)))))

(ert-deftest slack-test-class-have-slot-p ()
  (should (slack-class-have-slot-p 'slack-channel :id))
  (should (slack-class-have-slot-p 'slack-channel :name))
  (should (slack-class-have-slot-p 'slack-group :is_archived))
  (should-not (slack-class-have-slot-p 'slack-channel :nonexistent_slot)))

;;; ---- Permalink round-trip ----

(ert-deftest slack-test-permalink-to-info-basic ()
  (let ((info (slack-permalink-to-info
               "https://clojurians.slack.com/archives/C099W16KZ/p1730182493679269")))
    (should (string= "clojurians" (plist-get info :team-domain)))
    (should (string= "C099W16KZ" (plist-get info :room-id)))
    (should (string= "1730182493.679269" (plist-get info :ts)))
    (should (string= "1730182493.679269" (plist-get info :thread-ts)))))

(ert-deftest slack-test-permalink-to-info-with-thread-ts ()
  (let ((info (slack-permalink-to-info
               "https://myteam.slack.com/archives/C123/p1710000000000100?thread_ts=1710000000.000000&cid=C123")))
    (should (string= "myteam" (plist-get info :team-domain)))
    (should (string= "C123" (plist-get info :room-id)))
    (should (string= "1710000000.000100" (plist-get info :ts)))
    (should (string= "1710000000.000000" (plist-get info :thread-ts)))))

(ert-deftest slack-test-permalink-round-trip ()
  (let* ((info (list :team-domain "myteam"
                     :room-id "C099W16KZ"
                     :ts "1730182493.679269"
                     :thread-ts "1730182493.679269"))
         (permalink (slack-info-to-permalink info))
         (back (slack-permalink-to-info permalink)))
    (should (string= (plist-get info :team-domain)
                     (plist-get back :team-domain)))
    (should (string= (plist-get info :room-id)
                     (plist-get back :room-id)))
    (should (string= (plist-get info :ts)
                     (plist-get back :ts)))))

(ert-deftest slack-test-permalink-no-thread ()
  (let* ((info (list :team-domain "acme"
                     :room-id "C555"
                     :ts "1700000000.000000"
                     :thread-ts nil))
         (permalink (slack-info-to-permalink info)))
    (should (string-match-p "C555" permalink))
    (should-not (string-match-p "thread_ts" permalink))))

;;; ---- Rate limiting ----

(ert-deftest slack-test-rate-limit-first-request-returns-zero ()
  (let ((slack-rate-limit-counters (make-hash-table :test 'equal)))
    (should (eq 0 (slack-rate-limit-delay 'tier-1)))))

(ert-deftest slack-test-rate-limit-under-limit-returns-zero ()
  (let ((slack-rate-limit-counters (make-hash-table :test 'equal)))
    (should (eq 0 (slack-rate-limit-delay 'tier-4)))
    (should (eq 0 (slack-rate-limit-delay 'tier-4)))
    (should (eq 0 (slack-rate-limit-delay 'tier-4)))))

(ert-deftest slack-test-rate-limit-at-limit-returns-positive ()
  (let ((slack-rate-limit-counters (make-hash-table :test 'equal)))
    (puthash 'tier-1 (cons 1 (float-time))
             slack-rate-limit-counters)
    (let ((delay (slack-rate-limit-delay 'tier-1)))
      (should (> delay 0)))))

(ert-deftest slack-test-rate-limit-expired-window-resets ()
  (let ((slack-rate-limit-counters (make-hash-table :test 'equal)))
    (puthash 'tier-1 (cons 100 (- (float-time) 120))
             slack-rate-limit-counters)
    (should (eq 0 (slack-rate-limit-delay 'tier-1)))))

(ert-deftest slack-test-rate-limit-reset ()
  (let ((slack-rate-limit-counters (make-hash-table :test 'equal)))
    (puthash 'tier-1 (cons 5 (float-time))
             slack-rate-limit-counters)
    (slack-rate-limit-reset 'tier-1)
    (should (null (gethash 'tier-1 slack-rate-limit-counters)))))

;;; ---- Room label and display ----

(ert-deftest slack-test-room-inc-unread-count ()
  (let ((room (make-instance 'slack-channel :id "C1" :name "test")))
    (should (eq 0 (oref room unread-count-display)))
    (slack-room-inc-unread-count room)
    (should (eq 1 (oref room unread-count-display)))
    (slack-room-inc-unread-count room)
    (should (eq 2 (oref room unread-count-display)))))

(ert-deftest slack-test-group-hidden-when-not-member ()
  (let ((non-member (make-instance 'slack-group
                                   :id "G1" :name "private"
                                   :is_member nil)))
    (should (slack-room-hidden-p non-member)))
  (let ((member (make-instance 'slack-group
                               :id "G2" :name "joined"
                               :is_member t)))
    (should-not (slack-room-hidden-p member))))

(ert-deftest slack-test-group-hidden-when-archived ()
  (let ((archived-member (make-instance 'slack-group
                                        :id "G1" :name "old"
                                        :is_member t
                                        :is_archived t)))
    (should (slack-room-hidden-p archived-member)))
  (let ((active-member (make-instance 'slack-group
                                      :id "G2" :name "active"
                                      :is_member t
                                      :is_archived nil)))
    (should-not (slack-room-hidden-p active-member))))

(ert-deftest slack-test-channel-hidden-when-archived ()
  (let ((archived (make-instance 'slack-channel
                                 :id "C1" :name "old"
                                 :is_archived t)))
    (should (slack-room-hidden-p archived)))
  (let ((active (make-instance 'slack-channel
                               :id "C2" :name "active"
                               :is_archived nil)))
    (should-not (slack-room-hidden-p active))))

(ert-deftest slack-test-im-open-p ()
  (let ((open (make-instance 'slack-im :id "D1" :user "U1")))
    (should (slack-room-open-p open)))
  (let ((frozen (make-instance 'slack-im :id "D2" :user "U2"
                               :is_frozen t)))
    (should-not (slack-room-open-p frozen)))
  (let ((deleted (make-instance 'slack-im :id "D3" :user "U3"
                                :is_user_deleted t)))
    (should-not (slack-room-open-p deleted))))

;;; ---- Request retry ----

(ert-deftest slack-test-request-retry-under-max ()
  (let ((req (slack-request-create "https://example.com"
                                   nil
                                   :type "GET"
                                   :success #'ignore)))
    (oset req retry-count 0)
    (should (slack-request-retry-failed-request-p
             req '(end-of-file) 'error))))

(ert-deftest slack-test-request-retry-rejects-post ()
  (let ((req (slack-request-create "https://example.com"
                                   nil
                                   :type "POST"
                                   :success #'ignore)))
    (oset req retry-count 0)
    (should-not (slack-request-retry-failed-request-p
                 req '(end-of-file) 'error))))

;;; ---- Room mention count display ----

(ert-deftest slack-test-room-mention-count-display ()
  (slack-test-setup
    (oset team counts
          (slack-create-counts
           '(:threads (:has_unreads nil :mention_count 0)
             :channels ((:id "C11111" :has_unreads nil
                         :mention_count 0 :latest "1.0"))
             :mpims () :ims ())))
    (should (string= "" (slack-room-mention-count-display channel team)))
    (slack-counts-channel-set-mention-count (oref team counts) channel 5)
    (should (string= "(5)" (slack-room-mention-count-display channel team)))))

(if noninteractive
    (ert-run-tests-batch-and-exit)
  (ert t))

;;; test-suite.el ends here
