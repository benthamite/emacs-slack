;;; run-test.el --- run emacs-slack tests  -*- lexical-binding: t; -*-

(require 'ert)
(require 'slack)
(require 'slack-team)
(require 'slack-channel)
(require 'slack-usergroup)
(require 'slack-message-formatter)
(require 'slack-block)
(require 'slack-mrkdwn)
(require 'slack-message-sender)
(require 'slack-image)
(require 'slack-message)
(require 'slack-request)
(require 'slack-star)
(require 'slack-star-event)
(require 'slack-stars-buffer)
(require 'slack-pinned-items-buffer)
(require 'slack-search)
(require 'slack-search-result-buffer)
(require 'slack-scheduled-messages-buffer)
(require 'slack-activity-feed-buffer)

(defvar slack-channel-button-keymap nil)
(setq slack-render-image-p t)

(defmacro slack-test-setup (&rest body)
  (declare (indent 0) (debug t))
  `(let* ((channel-id "C11111")
          (channel-name "TestChannel")
          (channel (make-instance 'slack-channel
                                  :id channel-id
                                  :name channel-name))
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
                                           (puthash (oref channel id)
                                                    channel
                                                    h)
                                           h)
                               :users (let ((h (make-hash-table :test 'equal)))
                                        (puthash (plist-get user :id)
                                                 user
                                                 h)
                                        h)
                               :usergroups (list usergroup))))
     ,@body))

(ert-deftest slack-test-image-path ()
  (let* ((url "http://example.com/image.jpg?crop=1:2;3:4")
         (splitted (split-string url "?"))
         (query (cadr splitted))
         (ext (file-name-extension (car splitted))))
    (should (equal (expand-file-name (concat (md5 url) "." ext)
                                     slack-image-file-directory)
                   (slack-image-path url))))
  (let* ((url "http://example.com/image.jpg")
         (ext "jpg"))
    (should (equal (expand-file-name (concat (md5 url) "." ext)
                                     slack-image-file-directory)
                   (slack-image-path url))))
  (let* ((url "https://qiita-user-contents.imgix.net/https%3A%2F%2Fcdn.qiita.com%2Fassets%2Fpublic%2Farticle-ogp-background-1150d8b18a7c15795b701a55ae908f94.png?ixlib=rb-1.2.2&w=1200&mark=https%3A%2F%2Fqiita-user-contents.imgix.net%2F~text%3Fixlib%3Drb-1.2.2%26w%3D840%26h%3D380%26txt%3DRuby%25E3%2581%25AESJIS%25E3%2581%25AFShift_JIS%25E3%2581%2598%25E3%2582%2583%25E3%2581%25AA%25E3%2581%2584%26txt-color%3D%2523333%26txt-font%3DAvenir-Black%26txt-size%3D54%26txt-clip%3Dellipsis%26txt-align%3Dcenter%252Cmiddle%26s%3D7bec43d35ef823368af1e72227127508&mark-align=center%2Cmiddle&blend=https%3A%2F%2Fqiita-user-contents.imgix.net%2F~text%3Fixlib%3Drb-1.2.2%26w%3D840%26h%3D500%26txt%3D%2540yugo-yamamoto%26txt-color%3D%2523333%26txt-font%3DAvenir-Black%26txt-size%3D45%26txt-align%3Dright%252Cbottom%26s%3D8d65eaf7282eb521ea4a47ecac74efc5&blend-align=center%2Cmiddle&blend-mode=normal&s=1b2bdc847358703b21238cb69b5f37d8")
         (splitted (split-string url "?"))
         (query (cadr splitted))
         (ext (file-name-extension (car splitted))))
    (should (equal (expand-file-name (concat (md5 url) "." ext)
                                     slack-image-file-directory)
                   (slack-image-path url)))))

(ert-deftest slack-test-unescape-&<> ()
  (should (equal "<" (slack-unescape-&<> "&lt;")))
  (should (equal ">" (slack-unescape-&<> "&gt;")))
  (should (equal "&" (slack-unescape-&<> "&amp;")))
  (should (equal "foo" (slack-unescape-&<> "foo"))))

(ert-deftest slack-test-unescape-channel ()
  (slack-test-setup
    (should (equal (format "#%s" channel-name)
                   (slack-unescape-channel
                    (format "<#%s>" channel-id)
                    team)))
    (should (equal "#Foo"
                   (slack-unescape-channel
                    (format "<#%s|Foo>" channel-id)
                    team)))
    (should (equal "#<Unknown CHANNEL>"
                   (slack-unescape-channel
                    "<#C9999999>" team)))))

(ert-deftest slack-test-unescape-@ ()
  (slack-test-setup
    (oset team full-and-display-names t)
    (should (equal (format "@%s" real-name)
                   (slack-unescape-@
                    (format "<@%s>" user-id)
                    team)))
    (should (equal (format "@%s" real-name)
                   (slack-unescape-@
                    (format "<@%s|Foo>" user-id)
                    team)))
    (should (equal "@<Unknown USER>"
                   (slack-unescape-@
                    "<@U424242>" team)))
    (oset team full-and-display-names nil)
    (should (equal (format "@%s" display-name)
                   (slack-unescape-@
                    (format "<@%s>" user-id)
                    team)))
    (should (equal (format "@%s" display-name)
                   (slack-unescape-@
                    (format "<@%s|Foo>" user-id)
                    team)))
    (should (equal "@<Unknown USER>"
                   (slack-unescape-@
                    "<@U424242>" team)))
    ))

(ert-deftest slack-test-unescape-!subteam ()
  (slack-test-setup
    (should (equal (slack-unescape-!subteam
                    (format "<!subteam^%s|@%s>"
                            usergroup-id
                            usergroup-handle))
                   (format "@%s" usergroup-handle)))))


(ert-deftest slack-test-unescape-!date ()
  (should (equal (slack-unescape-!date
                  "<!date^1392734382^Posted {date_num} {time_secs}|Posted 2014-02-18 14:39:42>"
                  0)
                 "Posted 2014-02-18 14:39:42") )
  (should (equal (slack-unescape-!date
                  "<!date^1392734382^{date} at {time}|February 18 2014 at 14:39 PST>"
                  0)
                 "February 18, 2014 at 14:39"))
  (should (equal (slack-unescape-!date
                  "<!date^1392734382^{date_short}^https://example.com/|Feb 18, 2014 PST>"
                  0)
                 "<https://example.com/|Feb 18, 2014>"))
  )


(ert-deftest slack-test-unescape-variable ()
  (should (equal "@here" (slack-unescape-variable "<!here>")))
  (should (equal "@here" (slack-unescape-variable "<!here|here>")))
  (should (equal "@channel" (slack-unescape-variable "<!channel>")))
  (should (equal "@everyone" (slack-unescape-variable "<!everyone>")))
  (should (equal "<foo>" (slack-unescape-variable "<!foo>")))
  (should (equal "<label>" (slack-unescape-variable "<!foo|label>"))))

(ert-deftest slack-test-section-layout-block ()
  (let* ((input '(:type "section" :block_id "Vgv" :text (:type "mrkdwn" :text "Take a look at this image." :verbatim :json-false) :accessory (:fallback "600x800px image" :image_url "https://api.slack.com/img/blocks/bkb_template_images/palmtree.png" :image_width 600 :image_height 800 :image_bytes 482870 :type "image" :alt_text "palm tree") :fields ((:type "mrkdwn" :text "Foo" :verbatim :json-false) (:type "mrkdwn" :text "Bar" :verbatim :json-false))))
         (out (slack-create-layout-block input))
         (text (oref out text)))
    (should (eq 'slack-section-layout-block
                (eieio-object-class-name out)))
    (should (equal "Vgv" (oref out block-id)))
    (should (eq (eieio-object-class-name text)
                'slack-text-message-composition-object))
    (should (eq 2 (length (oref out fields))))
    (mapc #'(lambda (e) (should (eq 'slack-text-message-composition-object
                                    (eieio-object-class-name e))))
          (oref out fields))
    (should (eq 'slack-image-block-element
                (eieio-object-class-name (oref out accessory))))
    ))

(ert-deftest slack-test-divider-layout-block ()
  (let* ((input '(:type "divider" :block_id "y5a"))
         (out (slack-create-layout-block input)))
    (should (eq 'slack-divider-layout-block
                (eieio-object-class-name out)))
    (should (equal (oref out block-id)
                   "y5a"))))

(ert-deftest slack-test-image-layout-block ()
  (let* ((input '(:type "image" :block_id "Tron" :image_url "https://api.slack.com/img/blocks/bkb_template_images/beagle.png" :alt_text "image1" :title (:type "plain_text" :text "image1" :emoji t) :fallback "1080x1080px image" :image_width 1080 :image_height 1080 :image_bytes 1432686))
         (out (slack-create-layout-block input)))
    (should (eq 'slack-image-layout-block
                (eieio-object-class-name out)))
    (should (eq 1080 (oref out image-width)))
    (should (eq 1080 (oref out image-height)))
    (should (equal
             "https://api.slack.com/img/blocks/bkb_template_images/beagle.png"
             (oref out image-url)))

    (should (equal "image1" (oref (oref out title) text)))
    (should (eq t (oref (oref out title) emoji)))
    ))

(ert-deftest slack-test-actions-layout-block ()
  (let* ((input '(:type
                  "actions"
                  :block_id "lzhj"
                  :elements ((:type
                              "conversations_select"
                              :action_id "r4Y"
                              :placeholder (:type
                                            "plain_text"
                                            :text "Select a conversation"
                                            :emoji t)))))
         (out (slack-create-layout-block input)))
    (should (eq 'slack-actions-layout-block
                (eieio-object-class-name out)))
    (should (eq 1 (length (oref out elements))))
    (should (equal "lzhj" (oref out block-id)))
    ))

(ert-deftest slack-text-context-layout-block ()
  (let* ((input '(:type "context" :block_id "mOfwN" :elements ((:type "plain_text" :text "Last updated: Jan 1, 2019" :emoji t) (:fallback "600x800px image" :image_url "https://api.slack.com/img/blocks/bkb_template_images/goldengate.png" :image_width 600 :image_height 800 :image_bytes 828593 :type "image" :alt_text "goldengate"))))
         (out (slack-create-layout-block input)))
    (should (eq 'slack-context-layout-block
                (eieio-object-class-name out)))
    (should (equal "mOfwN" (oref out block-id)))
    (should (eq 2 (length (oref out elements))))
    ))

(ert-deftest slack-test-button-block-element ()
  (let* ((input '(:type "section" :block_id "mL2Z" :text (:type "mrkdwn" :text "You can add a button alongside text in your message. " :verbatim :json-false) :accessory (:type "button" :text (:type "plain_text" :text "Button" :emoji t) :value "click_me_123" :action_id "4sDY")))
         (out (slack-create-layout-block input))
         (button (oref out accessory)))
    (should (eq 'slack-button-block-element
                (eieio-object-class-name button)))
    (should (equal "click_me_123"
                   (oref button value)))
    (should (equal "4sDY"
                   (oref button action-id)))
    (should (equal "Button"
                   (oref (oref button text) text)))
    ))

(ert-deftest slack-test-static-select-block-element ()
  (let* ((input '(:type "section" :block_id "1ljQm" :text (:type "mrkdwn" :text "Pick an item from the dropdown list" :verbatim :json-false) :accessory (:type "static_select" :placeholder (:type "plain_text" :text "Select an item" :emoji t) :options ((:text (:type "plain_text" :text "Choice 1" :emoji t) :value "value-0") (:text (:type "plain_text" :text "Choice 2" :emoji t) :value "value-1") (:text (:type "plain_text" :text "Choice 3" :emoji t) :value "value-2")) :action_id "glxm" :initial_option (:text (:type "plain_text" :text "Choice 2" :emoji t) :value "value-1"))))
         (out (slack-create-layout-block input))
         (select (oref out accessory)))
    (should (eq 'slack-static-select-block-element
                (eieio-object-class-name select)))
    (should (eq 3 (length (oref select options))))
    (should (eq nil (oref select option-groups)))
    (should (equal "glxm" (oref select action-id)))
    (should (eq 'slack-text-message-composition-object
                (eieio-object-class-name (oref select placeholder))))
    (should (eq 'slack-option-message-composition-object
                (eieio-object-class-name (oref select initial-option)))))

  (let* ((input '(:type "section" :block_id "2aQr" :text (:type "mrkdwn" :text "Pick an item from the dropdown list" :verbatim :json-false) :accessory (:type "static_select" :placeholder (:type "plain_text" :text "Select an item" :emoji t) :initial_option (:text (:type "plain_text" :text "Choice 1" :emoji t) :value "value-0") :option_groups ((:label (:type "plain_text" :text "Foo" :emoji t) :options ((:text (:type "plain_text" :text "Choice 1" :emoji t) :value "value-0") (:text (:type "plain_text" :text "Choice 2" :emoji t) :value "value-1") (:text (:type "plain_text" :text "Choice 3" :emoji t) :value "value-2"))) (:label (:type "plain_text" :text "Bar" :emoji t) :options ((:text (:type "plain_text" :text "Choice 1" :emoji t) :value "value-0") (:text (:type "plain_text" :text "Choice 2" :emoji t) :value "value-1") (:text (:type "plain_text" :text "Choice 3" :emoji t) :value "value-2")))) :action_id "rUen5")))
         (out (slack-create-layout-block input))
         (select (oref out accessory)))
    (should (eq 'slack-static-select-block-element
                (eieio-object-class-name select)))
    (should (eq nil (oref select options)))
    (should (eq 2 (length (oref select option-groups))))
    (should (eq 'slack-option-message-composition-object
                (eieio-object-class-name (oref select initial-option))))
    (should (equal "rUen5" (oref select action-id)))
    (should (eq 'slack-text-message-composition-object
                (eieio-object-class-name (oref select placeholder))))))

(ert-deftest slack-test-external-select-block-element ()
  (let* ((input '(:type "section" :block_id "7y4+" :text (:type "mrkdwn" :text "Pick an item from the dropdown list" :verbatim :json-false) :accessory (:type "external_select" :placeholder (:type "plain_text" :text "Select an item" :emoji t) :min_query_length 3 :action_id "03T+" :confirm (:title (:type "plain_text" :text "Title") :text (:type "plain_text" :text "Text") :confirm (:type "plain_text" :text "Yes") :deny (:type "plain_text" :text "No")))))
         (out (slack-create-layout-block input))
         (select (oref out accessory)))
    (should (eq 'slack-external-select-block-element
                (eieio-object-class-name select)))
    (should (eq 'slack-text-message-composition-object
                (eieio-object-class-name (oref select placeholder))))
    (should (eq 'slack-confirmation-dialog-message-composition-object
                (eieio-object-class-name (oref select confirm))))
    (should (eq 3 (oref select min-query-length)))
    (should (equal "03T+" (oref select action-id)))))

(ert-deftest slack-test-user-select-block-element ()
  (let* ((input '(:type "section" :block_id "DjXiH" :text (:type "mrkdwn" :text "Pick an item from the dropdown list" :verbatim :json-false) :accessory (:type "users_select" :placeholder (:type "plain_text" :text "Select an item" :emoji t) :confirm (:title (:type "plain_text" :text "Title" :emoji t) :text (:type "plain_text" :text "Text" :emoji t) :confirm (:type "plain_text" :text "Yes" :emoji t) :deny (:type "plain_text" :text "No" :emoji t)) :action_id "VtT" :initial_user "UAAAAA")))
         (out (slack-create-layout-block input))
         (select (oref out accessory)))
    (should (eq 'slack-user-select-block-element
                (eieio-object-class-name select)))
    (should (eq 'slack-text-message-composition-object
                (eieio-object-class-name (oref select placeholder))))
    (should (eq 'slack-confirmation-dialog-message-composition-object
                (eieio-object-class-name (oref select confirm))))
    (should (equal "VtT" (oref select action-id)))
    (should (equal "UAAAAA" (oref select initial-user)))))

(ert-deftest slack-test-conversation-select-block-element ()
  (let* ((input '(:type "section" :block_id "SrLD" :text (:type "mrkdwn" :text "Pick an item from the dropdown list" :verbatim :json-false) :accessory (:type "conversations_select" :placeholder (:type "plain_text" :text "Select an item" :emoji t) :confirm (:title (:type "plain_text" :text "Title" :emoji t) :text (:type "plain_text" :text "Text" :emoji t) :confirm (:type "plain_text" :text "Yes" :emoji t) :deny (:type "plain_text" :text "No" :emoji t)) :action_id "GfRre")))
         (out (slack-create-layout-block input))
         (select (oref out accessory)))
    (should (eq 'slack-conversation-select-block-element
                (eieio-object-class-name select)))
    (should (eq 'slack-text-message-composition-object
                (eieio-object-class-name (oref select placeholder))))
    (should (eq 'slack-confirmation-dialog-message-composition-object
                (eieio-object-class-name (oref select confirm))))
    (should (equal "GfRre" (oref select action-id)))))

(ert-deftest slack-test-channel-select-block-element ()
  (let* ((input '(:type "section" :block_id "fBa" :text (:type "mrkdwn" :text "Pick an item from the dropdown list" :verbatim :json-false) :accessory (:type "channels_select" :initial_channel "C0G31N06B" :placeholder (:type "plain_text" :text "Select an item" :emoji t) :action_id "zJl")))
         (out (slack-create-layout-block input))
         (select (oref out accessory)))
    (should (eq 'slack-channel-select-block-element
                (eieio-object-class-name select)))
    (should (eq 'slack-text-message-composition-object
                (eieio-object-class-name (oref select placeholder))))
    (should (equal "C0G31N06B" (oref select initial-channel)))
    (should (equal "zJl" (oref select action-id)))))

(ert-deftest slack-test-overflow-block-element ()
  (let* ((input '(:type "section" :block_id "Egl" :text (:type "mrkdwn" :text "This block has an overflow menu." :verbatim :json-false) :accessory (:type "overflow" :options ((:text (:type "plain_text" :text "Option 1" :emoji t) :value "value-0") (:text (:type "plain_text" :text "Option 2" :emoji t) :value "value-1") (:text (:type "plain_text" :text "Option 3" :emoji t) :value "value-2") (:text (:type "plain_text" :text "Option 4" :emoji t) :value "value-3")) :confirm (:title (:type "plain_text" :text "Title" :emoji t) :text (:type "plain_text" :text "Text" :emoji t) :confirm (:type "plain_text" :text "Yes" :emoji t) :deny (:type "plain_text" :text "No" :emoji t)) :action_id "ksRP")))
         (out (slack-create-layout-block input))
         (overflow (oref out accessory)))
    (should (eq 'slack-overflow-menu-block-element
                (eieio-object-class-name overflow)))
    (should (eq 'slack-confirmation-dialog-message-composition-object
                (eieio-object-class-name (oref overflow confirm))))
    (should (eq 4 (length (oref overflow options))))
    (should (equal "ksRP" (oref overflow action-id)))))

(ert-deftest slack-test-datepicker-block-element ()
  (let* ((input '(:type "section" :block_id "VvZ" :text (:type "mrkdwn" :text "Pick a date for the deadline." :verbatim :json-false) :accessory (:type "datepicker" :initial_date "1990-04-28" :placeholder (:type "plain_text" :text "Select a date" :emoji t) :action_id "G=RRF")))
         (out (slack-create-layout-block input))
         (datepicker (oref out accessory)))
    (should (eq 'slack-date-picker-block-element
                (eieio-object-class-name datepicker)))
    (should (eq 'slack-text-message-composition-object
                (eieio-object-class-name (oref datepicker placeholder))))
    (should (equal "1990-04-28" (oref datepicker initial-date)))
    (should (equal "G=RRF" (oref datepicker action-id)))))

(ert-deftest slack-test-block-to-string ()
  ;; nil
  (should (eq nil (slack-block-to-string nil)))
  ;; slack-section-layout-block
  (let* ((input '(:type "section" :text (:type "plain_text" :text "Hello") :fields ((:type "plain_text" :text "Foo") (:type "plain_text" :text "Bar"))))
         (out (slack-block-to-string
               (slack-create-layout-block input))))
    (should (equal "Hello\nFoo\nBar" out)))
  ;; slack-divider-layout-block
  (let* ((lui-fill-column 10)
         (input '(:type "divider"))
         (out (slack-block-to-string
               (slack-create-layout-block input))))
    (should (eq 10 (length out)))
    (should (equal "----------" out)))
  ;; slack-image-layout-block
  (let* ((input '(:type "image" :block_id "DxZ" :image_url "https://goldengate.png" :alt_text "Example Image" :title (:type "plain_text" :text "Example Image" :emoji t) :fallback "600x800px image" :image_width 600 :image_height 800 :image_bytes 828593))
         (out (slack-block-to-string
               (slack-create-layout-block input))))
    (should (equal "Example Image (829 kB)\n[Image]" out)))
  ;; slack-actions-layout-block
  (let* ((input '(:type "actions" :elements ((:type "button" :text (:type "plain_text" :text "Button1") :action_id "Foo") (:type "button" :text (:type "plain_text" :text "Button2") :action_id "Bar"))))
         (actions (slack-create-layout-block input))
         (out (slack-block-to-string actions)))
    (should (equal "Button1 Button2" out)))
  ;; slack-context-layout-block
  (let* ((input '(:type "context" :block_id "CmiG" :elements ((:type "plain_text" :text "For more info, contact support@acme.inc" :emoji t) (:fallback "666x1000px image" :image_url "https://1YjNtFtJlMTaC26A/o.jpg" :image_width 666 :image_height 1000 :image_bytes 107304 :type "image" :alt_text "alt text for image"))))
         (context (slack-create-layout-block input))
         (out (slack-block-to-string context)))
    (should (equal "For more info, contact support@acme.inc [Image]" out)))
  ;; slack-static-select-block-element
  (let* ((input '(:type "static_select" :placeholder (:type "plain_text" :text "Select an item" :emoji t) :initial_option (:text (:type "plain_text" :text "Choice 1" :emoji t) :value "value-0") :options ((:text (:type "plain_text" :text "Choice 1" :emoji t) :value "value-0") (:text (:type "plain_text" :text "Choice 2" :emoji t) :value "value-1") (:text (:type "plain_text" :text "Choice 3" :emoji t) :value "value-2")) :action_id "tIzc"))
         (out (slack-block-to-string
               (slack-create-block-element input ""))))
    (should (equal "Choice 1" out)))
  ;; slack-external-select-block-element
  (let* ((input '(:type "external_select" :placeholder (:type "plain_text" :text "Select an item" :emoji t) :action_id "T6JV"))
         (out (slack-block-to-string
               (slack-create-block-element input ""))))
    (should (equal "Select an item" out)))
  ;; slack-user-select-block-element
  (let* ((input '(:type "users_select" :action_id "4ji5" :initial_user "U0G2XCVQV" :placeholder (:type "plain_text" :text "Select a user" :emoji t)))
         (out (slack-block-to-string
               (slack-create-block-element input ""))))
    (should (equal "USER: U0G2XCVQV" out))
    (should (eq t (get-text-property 0 'slack-lazy-user-name out)))
    (should (equal "U0G2XCVQV" (get-text-property 0 'slack-user-id out))))
  ;; slack-conversation-select-block-element
  (let* ((input '(:type "conversations_select" :action_id "2/eZ" :initial_conversation "C0G31N06B" :placeholder (:type "plain_text" :text "Select a conversation" :emoji t)))
         (out (slack-block-to-string
               (slack-create-block-element input ""))))
    (should (equal "CONVERSATION: C0G31N06B" out))
    (should (eq t (get-text-property 0 'slack-lazy-conversation-name out)))
    (should (equal "C0G31N06B" (get-text-property 0 'slack-conversation-id out))))
  ;; slack-channel-select-block-element
  (let* ((input '(:type "channels_select" :action_id "CL5OA" :initial_channel "C0G31N06B" :placeholder (:type "plain_text" :text "Select a channel" :emoji t)))
         (out (slack-block-to-string
               (slack-create-block-element input ""))))
    (should (equal "CHANNEL: C0G31N06B" out))
    (should (eq t (get-text-property 0 'slack-lazy-conversation-name out)))
    (should (equal "C0G31N06B" (get-text-property 0 'slack-conversation-id out))))
  ;; slack-overflow-block-element
  (let* ((input '(:type "overflow" :options ((:text (:type "plain_text" :text "Option 1" :emoji t) :value "value-0") (:text (:type "plain_text" :text "Option 2" :emoji t) :value "value-1") (:text (:type "plain_text" :text "Option 3" :emoji t) :value "value-2") (:text (:type "plain_text" :text "Option 4" :emoji t) :value "value-3")) :action_id "3O/yl"))
         (out (slack-block-to-string
               (slack-create-block-element input ""))))
    (should (equal " … " out))
    )
  ;; slack-date-picker-block-element
  (let* ((input '(:type "datepicker" :initial_date "1990-04-28" :placeholder (:type "plain_text" :text "Select a date" :emoji t) :action_id "5/4"))
         (out (slack-block-to-string
               (slack-create-block-element input ""))))
    (should (equal "1990-04-28" out)))
  ;; slack-text-message-composition-object
  (let* ((input '(:type "plain_text" :text "hello\nworld" :emoji t :verbatim :json-false))
         (out (slack-block-to-string
               (slack-create-text-message-composition-object input))))
    (should (equal "hello\nworld" out))))

(ert-deftest slack-test-mrkdwn-regex-bold ()
  (let ((bold "aaa *Ace Wasabi Rock-n-Roll Sushi Bar* aaa"))
    (string-match slack-mrkdwn-regex-bold bold)
    (should (equal (match-string 3 bold)
                   "Ace Wasabi Rock-n-Roll Sushi Bar"))
    (should (eq (match-beginning 2) 4))
    (should (eq (match-beginning 4) 37)))
  (should (string-match-p slack-mrkdwn-regex-bold "*@channel お題案だしてもらったので以下から2つ選んでください*"))

  (should (not (string-match-p slack-mrkdwn-regex-bold "* bbb *")))
  (should (not (string-match-p slack-mrkdwn-regex-bold "*bbb*aaa")))
  (should (not (string-match-p slack-mrkdwn-regex-bold "aaa*bbb*")))
  (should (not (string-match-p slack-mrkdwn-regex-bold "aaa*bbb*bbb")))
  (should (not (string-match-p slack-mrkdwn-regex-bold "aaa *Ace Wasabi Rock-n-Roll\n Sushi Bar* aaa")))
  (should (string-match-p slack-mrkdwn-regex-bold "*Ace Wasabi Rock-n-Roll Sushi Bar*")))

(ert-deftest slack-test-mrkdwn-regex-italic ()
  (let ((italic "aaa _Ace Wasabi Rock-n-Roll Sushi Bar_ aaa"))
    (string-match slack-mrkdwn-regex-italic italic)
    (should (equal (match-string 3 italic)
                   "Ace Wasabi Rock-n-Roll Sushi Bar"))
    (should (equal "_" (match-string 2 italic)))
    (should (equal "_" (match-string 4 italic)))
    (should (eq (match-beginning 2) 4))
    (should (eq (match-beginning 4) 37)))
  (should (not (string-match-p slack-mrkdwn-regex-italic "_bbb_aaa")))
  (should (not (string-match-p slack-mrkdwn-regex-italic "aaa_bbb_")))
  (should (not (string-match-p slack-mrkdwn-regex-italic "aaa_bbb_aaa")))
  (should (not (string-match-p slack-mrkdwn-regex-italic "SOME_ENV_BAR")))
  (should (not (string-match-p slack-mrkdwn-regex-italic "https://example.com/foo_bar_baz.html")))
  (should (not (string-match-p slack-mrkdwn-regex-italic "aaa _Ace Wasabi Rock-n-Roll \nSushi Bar_ aaa")))
  (should (string-match-p slack-mrkdwn-regex-italic "_a a a_"))
  (should (string-match-p slack-mrkdwn-regex-italic "_ aaa _"))
  (should (string-match-p slack-mrkdwn-regex-italic "_Ace Wasabi Rock-n-Roll Sushi Bar_")))

(ert-deftest slack-test-mrkdwn-regex-strike ()
  (let ((strike "aaa ~Ace Wasabi Rock-n-Roll Sushi Bar~ aaa"))
    (string-match slack-mrkdwn-regex-strike strike)
    (should (equal (match-string 3 strike)
                   "Ace Wasabi Rock-n-Roll Sushi Bar"))
    (should (eq (match-beginning 2) 4))
    (should (eq (match-beginning 4) 37)))
  (should (not (string-match-p slack-mrkdwn-regex-strike "~ bbb ~")))
  (should (not (string-match-p slack-mrkdwn-regex-strike "~bbb~aaa")))
  (should (not (string-match-p slack-mrkdwn-regex-strike "aaa~bbb~")))
  (should (not (string-match-p slack-mrkdwn-regex-strike "aaa~bbb~aaa")))
  (should (not (string-match-p slack-mrkdwn-regex-strike "aaa ~Ace Wasabi Rock-n-Roll\n Sushi Bar~ aaa")))
  (should (string-match-p slack-mrkdwn-regex-strike "~Ace Wasabi Rock-n-Roll Sushi Bar~"))
  (should (string-match-p slack-mrkdwn-regex-strike "~*When:*  7月 31, 2020 16:30 - 17:00~")))

(ert-deftest slack-test-mrkdwn-regex-code ()
  (let ((code "aaa `Ace Wasabi Rock-n-Roll Sushi Bar` aaa"))
    (should (string-match-p slack-mrkdwn-regex-code code))
    (string-match slack-mrkdwn-regex-code code)
    (should (equal "Ace Wasabi Rock-n-Roll Sushi Bar"
                   (match-string 3 code)))
    (should (eq (match-beginning 2) 4))
    (should (eq (match-beginning 4) 37))
    )
  ;; TODO
  ;; (let ((block "   ```This is a `code` block\nAnd it's multi-line```   "))
  ;;   (should (eq nil (string-match-p slack-mrkdwn-regex-code block))))
  (should (string-match-p slack-mrkdwn-regex-code "aaa`bbb`aaa"))
  (should (string-match-p slack-mrkdwn-regex-code "aaa`bbb`"))
  (should (not (string-match-p slack-mrkdwn-regex-code "   ```This is a code block\nAnd it's multi-line```   ")))
  (should (not (string-match-p slack-mrkdwn-regex-code "aaa `Ace Wasabi \nRock-n-Roll Sushi Bar` aaa")))
  (should (string-match-p slack-mrkdwn-regex-code "`bbb`aaa"))
  (should (string-match-p slack-mrkdwn-regex-code "` bbb `")))

(ert-deftest slack-test-mrkdwn-regex-code-block ()
  (let ((block "   ```This is a code block\nAnd it's multi-line```   "))
    (string-match slack-mrkdwn-regex-code-block block)
    (should (equal "This is a code block\nAnd it's multi-line"
                   (match-string 2 block)))
    (should (eq 3 (match-beginning 1)))
    (should (eq 46 (match-beginning 4))))
  (let ((block "   ```\nThis is a code block\nAnd it's multi-line\n```   "))
    (string-match slack-mrkdwn-regex-code-block block)
    (should (equal "This is a code block\nAnd it's multi-line"
                   (match-string 2 block)))
    (should (eq 3 (match-beginning 1)))
    (should (eq 47 (match-beginning 4))))
  (should (string-match-p slack-mrkdwn-regex-code-block "```\nbbb\naaa\n```\n"))
  (should (not (string-match-p slack-mrkdwn-regex-code-block "aaa```bbb```aaa")))
  (should (not (string-match-p slack-mrkdwn-regex-code-block "aaa```bbb```")))
  (should (not (string-match-p slack-mrkdwn-regex-code-block "```bbb```aaa")))
  (should (not (string-match-p slack-mrkdwn-regex-code-block "aaa `Ace Wasabi Rock-n-Roll Sushi Bar` aaa"))))


(ert-deftest slack-test-mrkdwn-regex-blockquote ()
  (let ((blockquote " > aaa aaa"))
    (string-match slack-mrkdwn-regex-blockquote blockquote)
    (should (equal "aaa aaa"
                   (match-string 3 blockquote)))
    (should (eq 1 (match-beginning 1))))
  (should (string-match-p slack-mrkdwn-regex-blockquote ">aaa"))
  (should (string-match-p slack-mrkdwn-regex-blockquote " > aaa"))
  (should (string-match-p slack-mrkdwn-regex-blockquote " >aaa"))
  (should (not (string-match-p slack-mrkdwn-regex-blockquote ">")))
  (should (not (string-match-p slack-mrkdwn-regex-blockquote "a > a")))
  (should (not (string-match-p slack-mrkdwn-regex-blockquote "a >"))))

(ert-deftest slack-test-mrkdwn-regex-list ()
  (let ((str "- aaa\n- bbb"))
    (string-match slack-mrkdwn-regex-list str)
    (should (string= "-" (match-string 2 str)))
    (should (string= "aaa" (match-string 4 str))))
  (let ((str "* aaa\n* bbb"))
    (string-match slack-mrkdwn-regex-list str)
    (should (string= "*" (match-string 2 str)))
    (should (string= "aaa" (match-string 4 str))))
  (let ((str "  -  aaa"))
    (string-match slack-mrkdwn-regex-list str)
    (should (string= "-" (match-string 2 str)))
    (should (string= "  " (match-string 1 str)))
    (should (string= " aaa" (match-string 4 str))))
  (let ((str "  1. aaa\n  2.bbb"))
    (string-match slack-mrkdwn-regex-list str)
    (should (string= "1." (match-string 2 str)))
    (should (string= "  " (match-string 1 str)))
    (should (string= "aaa" (match-string 4 str)))))

(ert-deftest slack-test-rich-text-section ()
  (let ((payload (list :type "rich_text_section"
                       :elements (list (list :type "text" :text "Hello")
                                       (list :type "text" :text " World")))))
    (should (string= "Hello World" (slack-block-to-string
                                    (slack-create-rich-text-block-element payload))))))

(ert-deftest slack-test-rich-text-preformatted ()
  (let* ((payload (list :type "rich_text_preformatted"
                        :elements (list (list :type "text" :text "Hello")
                                        (list :type "text" :text " World"))))
         (text (slack-block-to-string
                (slack-create-rich-text-block-element payload))))
    (should (eq 'slack-mrkdwn-code-block-face
                (get-text-property 0 'face text)))))

(ert-deftest slack-test-rich-text-quote ()
  (let* ((payload (list :type "rich_text_quote"
                        :elements (list (list :type "text" :text "Hello\n")
                                        (list :type "text" :text "\nWorld"))))
         (text (slack-block-to-string (slack-create-rich-text-block-element payload))))
    (should (eq 'slack-mrkdwn-blockquote-face
                (get-text-property 0 'face text)))))

(ert-deftest slack-test-rich-text-list ()
  (let ((elements (list (list :type "rich_text_section"
                              :elements (list (list :type "text" :text "foo")))
                        (list :type "rich_text_section"
                              :elements (list (list :type "text" :text "bar")))
                        (list :type "rich_text_section"
                              :elements (list (list :type "text" :text "baz"))))))

    (let* ((payload (list :type "rich_text_list"
                          :elements elements
                          :style "bullet"
                          :indent 0))
           (text (slack-block-to-string (slack-create-rich-text-block-element payload))))
      (should (string= (concat (format "%s foo" slack-mrkdwn-list-bullet)
                               "\n"
                               (format "%s bar" slack-mrkdwn-list-bullet)
                               "\n"
                               (format "%s baz" slack-mrkdwn-list-bullet)
                               "\n")
                       text)))

    (let* ((payload (list :type "rich_text_list"
                          :elements elements
                          :style "bullet"
                          :indent 1))
           (text (slack-block-to-string (slack-create-rich-text-block-element payload))))
      (should (string= (concat (format "  %s foo" slack-mrkdwn-list-bullet)
                               "\n"
                               (format "  %s bar" slack-mrkdwn-list-bullet)
                               "\n"
                               (format "  %s baz" slack-mrkdwn-list-bullet)
                               "\n")
                       text)))

    (let* ((payload (list :type "rich_text_list"
                          :elements elements
                          :style "ordered"
                          :indent 0))
           (text (slack-block-to-string (slack-create-rich-text-block-element payload))))
      (should (string= (concat "1. foo"
                               "\n"
                               "2. bar"
                               "\n"
                               "3. baz"
                               "\n")
                       text)))

    (let* ((payload (list :type "rich_text_list"
                          :elements elements
                          :style "ordered"
                          :indent 1))
           (text (slack-block-to-string (slack-create-rich-text-block-element payload))))
      (should (string= (concat "  1. foo"
                               "\n"
                               "  2. bar"
                               "\n"
                               "  3. baz"
                               "\n")
                       text)))))

(ert-deftest slack-test-rich-text-text-element ()
  (let ((payload '(:type "text" :text "Hello this is rich text")))
    (should (string= "Hello this is rich text" (slack-block-to-string
                                                (slack-create-rich-text-element payload))))))

(ert-deftest slack-test-rich-text-element-style ()
  (let* ((bold '(:bold t))
         (italic '(:italic t))
         (strike '(:strike t))
         (code '(:code t))
         (payload '(:type "text" :text "Hello this is rich text")))
    (cl-labels ((get-face-property (style)
                                   (get-text-property 0
                                                      'face
                                                      (slack-block-to-string
                                                       (slack-create-rich-text-element
                                                        (plist-put payload :style style))))))
      (should (eq 'slack-mrkdwn-bold-face (get-face-property bold)))
      (should (eq 'slack-mrkdwn-italic-face (get-face-property italic)))
      (should (eq 'slack-mrkdwn-strike-face (get-face-property strike)))
      (should (eq 'slack-mrkdwn-code-face (get-face-property code))))))

(ert-deftest slack-test-rich-text-channel-element ()
  (slack-test-setup
    (let ((payload (list :type "channel" :channel_id channel-id)))
      (should (string= (format "#%s" channel-name)
                       (slack-block-to-string (slack-create-rich-text-element payload)
                                              (list :team team)))))))

(ert-deftest slack-test-rich-text-user-elemenmt ()
  (slack-test-setup
    (let ((payload (list :type "user" :user_id user-id)))
      (should (string= (format "@%s" display-name)
                       (slack-block-to-string (slack-create-rich-text-element payload)
                                              (list :team team)))))))

(ert-deftest slack-test-rich-text-emoji-element ()
  (let ((payload (list :type "emoji" :name "smile")))
    (should (string= ":smile:" (slack-block-to-string (slack-create-rich-text-element payload))))))

(ert-deftest slack-test-rich-text-link-element ()
  (let* ((url "https://www.gnu.org/software/emacs/")
         (payload (list :type "link" :url url :text nil)))
    (should (string= (format "<%s|%s>" url url)
                     (slack-block-to-string (slack-create-rich-text-element payload)))))
  (let* ((url "https://www.gnu.org/software/emacs/")
         (text "GNU Emacs - GNU Project")
         (payload (list :type "link" :url url :text text)))
    (should (string= (format "<%s|%s>" url text)
                     (slack-block-to-string (slack-create-rich-text-element payload))))))

(ert-deftest slack-test-rich-text-usergroup-element ()
  (slack-test-setup
    (let ((payload (list :type "usergroup" :usergroup_id usergroup-id)))
      (should (string= (format "@%s" usergroup-handle)
                       (slack-block-to-string (slack-create-rich-text-element payload)
                                              (list :team team)))))))

(ert-deftest slack-test-rich-text-date-element ()
  (let* ((time (current-time))
         (payload (list :type "date" :timestamp (format-time-string "%s") time)))
    (should (string= (format-time-string "%Y-%m-%d %H:%M:%S" time)
                     (slack-block-to-string (slack-create-rich-text-element payload))))))

(ert-deftest slack-test-rich-text-range-element ()
  (let ((payload (list :type "broadcast" :range "here")))
    (should (string= "@here" (slack-block-to-string (slack-create-rich-text-element payload))))))

(defun slack-test-parse-blocks (str)
  (let* ((json-object-type 'plist)
         (json-array-type 'list))
    (plist-get (json-read-from-string (json-encode
                                       (with-temp-buffer
                                         (insert str)
                                         (slack-create-blocks-from-buffer))))
               :blocks)))

(ert-deftest slack-test-create-blocks-from-buffer ()
  (let* ((str (string-trim "*<!here> fff*"))
         (blocks (slack-test-parse-blocks str)))
    (let ((block (car blocks)))
      (should (string= "rich_text" (plist-get block :type)))
      (let ((elements (plist-get block :elements)))
        (should (eq 1 (length elements)))
        (let ((section (car elements)))
          (let ((elements (plist-get section :elements)))
            (should (eq 2 (length elements)))
            (let ((mention (nth 0 elements))
                  (text (nth 1 elements)))
              (should (string= "broadcast" (plist-get mention :type)))
              (should (string= "here" (plist-get mention :range)))
              (should (string= "text" (plist-get text :type)))
              (should (string= " fff" (plist-get text :text)))
              (should (eq t (plist-get (plist-get text :style) :bold)))))))))
;; (:type "mrkdwn" :text "*<!channel|channel> お題案だしてもらったので以下から2つ選んでください*
;; You may vote for multiple options" :verbatim t)
  (let* ((str (string-trim "
bold *bold* bold
italic _italic_ italic
strike ~strike~ strike
code `code` code
"))
         (blocks (slack-test-parse-blocks str)))
    (should (not (null blocks)))
    (let ((block (car blocks)))
      (should (string= "rich_text" (plist-get block :type)))
      (should (eq 1 (length (plist-get block :elements))))
      (let ((section (car (plist-get block :elements))))
        (should (string= "rich_text_section" (plist-get section :type)))
        (let ((elements (plist-get section :elements)))
          (dolist (style  '(("bold" . :bold)
                            ("italic" . :italic)
                            ("strike" . :strike)
                            ("code" . :code)))
            (should (eq 1 (length (cl-remove-if #'(lambda (el) (or (null (plist-get el :style))
                                                                   (not (plist-get (plist-get el :style)
                                                                                   (cdr style)))))
                                                elements))))
            (should (string= (car style)
                             (plist-get (cl-find-if #'(lambda (el) (and (plist-get el :style)
                                                                        (plist-get (plist-get el :style)
                                                                                   (cdr style))))
                                                    elements)
                                        :text))))))))
  (let* ((str (string-trim "
```
*code*
block
:smile:
<@USERID>
https://google.com
```
"))
         (blocks (slack-test-parse-blocks str)))
    (should (not (null blocks)))
    (let ((block (car blocks)))
      (let ((elements (plist-get block :elements)))
        (should (eq 1 (length elements)))
        (let ((section (car elements)))
          (should (string= "rich_text_preformatted" (plist-get section :type)))
          (let ((elements (plist-get section :elements)))
            (should (eq 1 (length elements)))
            (let ((element (car elements)))
              (should (string= "text" (plist-get element :type)))
              (should (string= "*code*\nblock\n:smile:\n<@USERID>\nhttps://google.com" (plist-get element :text)))))))))

  (let* ((str (string-trim "
> bold *bold* bold
> quote
"))
         (blocks (slack-test-parse-blocks str)))
    (should (not (null blocks)))
    (let ((block (car blocks)))
      (let ((elements (plist-get block :elements)))
        (should (eq 1 (length elements)))
        (let ((section (car elements)))
          (should (string= "rich_text_quote" (plist-get section :type)))
          (let ((elements (plist-get section :elements)))
            (should (eq 3 (length elements)))
            (dolist (style '(("bold" . :bold)))
              (should (eq 1 (length (cl-remove-if #'(lambda (el) (or (null (plist-get el :style))
                                                                     (not (plist-get (plist-get el :style)
                                                                                     (cdr style)))))
                                                  elements))))
              (should (string= (car style)
                               (plist-get (cl-find-if #'(lambda (el) (and (plist-get el :style)
                                                                          (plist-get (plist-get el :style)
                                                                                     (cdr style))))
                                                      elements)
                                          :text)))))))))

  (let* ((str (string-trim "
1. list
2. *bold*
3. ordered
"))
         (blocks (slack-test-parse-blocks str)))
    (should (not (null blocks)))
    (let ((block (car blocks)))
      (let ((elements (plist-get block :elements)))
        (should (eq 1 (length elements)))
        (let ((section (car elements)))
          (should (string= "rich_text_list" (plist-get section :type)))
          (should (string= "ordered" (plist-get section :style)))
          (should (eq 0 (plist-get section :indent)))
          (let ((elements (plist-get section :elements)))
            (should (eq 3 (length elements)))

            (let* ((bold-section (cadr elements))
                   (elements (plist-get bold-section :elements)))
              (dolist (style '(("bold" . :bold)))
                (should (eq 1 (length (cl-remove-if #'(lambda (el) (or (null (plist-get el :style))
                                                                       (not (plist-get (plist-get el :style)
                                                                                       (cdr style)))))
                                                    elements))))
                (should (string= (car style)
                                 (plist-get (cl-find-if #'(lambda (el) (and (plist-get el :style)
                                                                            (plist-get (plist-get el :style)
                                                                                       (cdr style))))
                                                        elements)
                                            :text))))))))))
  (let* ((str (string-trim "
 - list
 - `code`
 - bullet

"))
         (blocks (slack-test-parse-blocks str)))
    (should (not (null blocks)))
    (let ((block (car blocks)))
      (let ((elements (plist-get block :elements)))
        (should (eq 1 (length elements)))
        (let ((section (car elements)))
          (should (string= "rich_text_list" (plist-get section :type)))
          (should (string= "bullet" (plist-get section :style)))
          ;; Two spaces = one indent level; a single leading space is
          ;; level 0 (upstream's slack-list-indent-level semantics).
          (should (eq 0 (plist-get section :indent)))
          (let ((elements (plist-get section :elements)))
            (should (eq 3 (length elements)))

            (let* ((bold-section (cadr elements))
                   (elements (plist-get bold-section :elements)))
              (dolist (style '(("code" . :code)))
                (should (eq 1 (length (cl-remove-if #'(lambda (el) (or (null (plist-get el :style))
                                                                       (not (plist-get (plist-get el :style)
                                                                                       (cdr style)))))
                                                    elements))))
                (should (string= (car style)
                                 (plist-get (cl-find-if #'(lambda (el) (and (plist-get el :style)
                                                                            (plist-get (plist-get el :style)
                                                                                       (cdr style))))
                                                        elements)
                                            :text))))))))))
  (let* ((str (string-trim "
<@U0G2XCVQV>
<!channel>
<#C0G31N06B>
<!subteam^USLACKBOT>
"))
         (blocks (slack-test-parse-blocks str)))
    (should (not (null blocks)))
    (let ((block (car blocks)))
      (should (string= "rich_text" (plist-get block :type)))
      (should (eq 1 (length (plist-get block :elements))))
      (let ((section (car (plist-get block :elements))))
        (should (string= "rich_text_section" (plist-get section :type)))
        (let ((elements (plist-get section :elements)))
          (dolist (mention  '(("U0G2XCVQV" . ("user" . :user_id))
                              ("channel" . ("broadcast" . :range))
                              ("C0G31N06B" . ("channel" . :channel_id))
                              ("USLACKBOT" . ("usergroup" . :usergroup_id))))
            (let ((type (cadr mention))
                  (v (car mention))
                  (key (cddr mention)))
              (should (eq 1 (length (cl-remove-if #'(lambda (el) (not (string= (plist-get el :type)
                                                                               type)))
                                                  elements))))
              (should (string= v
                               (plist-get (cl-find-if #'(lambda (el) (string= (plist-get el :type)
                                                                              type))
                                                      elements)
                                          key)))))))))
  (let* ((slack-emoji-master (slack-test-emoji-master
                              "dog2" "man-biking" "man_dancing"))
         (str (string-trim "
:dog2:
:man-biking:
:man_dancing:
"))
         (blocks (slack-test-parse-blocks str)))
    (should (not (null blocks)))
    (let ((block (car blocks)))
      (should (string= "rich_text" (plist-get block :type)))
      (should (eq 1 (length (plist-get block :elements))))
      (let ((section (car (plist-get block :elements))))
        (should (not (null section)))
        (should (string= "rich_text_section" (plist-get section :type)))
        (let ((elements (plist-get section :elements)))
          (should (equal '("dog2" "man-biking" "man_dancing")
                         (mapcar #'(lambda (el) (plist-get el :name))
                                 (cl-remove-if #'(lambda (el) (not (string= "emoji" (plist-get el :type))))
                                               elements)))))
        )))
  (let* ((str (string-trim "
https://api.slack.com/changelog/2019-09-what-they-see-is-what-you-get-and-more-and-less
"))
         (blocks (slack-test-parse-blocks str)))
    (should (not (null blocks)))
    (let ((block (car blocks)))
      (should (string= "rich_text" (plist-get block :type)))
      (should (eq 1 (length (plist-get block :elements))))
      (let ((section (car (plist-get block :elements))))
        (should (not (null section)))
        (should (string= "rich_text_section" (plist-get section :type)))
        (let ((elements (plist-get section :elements)))
          (should (eq 1 (length (cl-remove-if #'(lambda (el) (not (string= "link" (plist-get el :type))))
                                              elements))))
          (should (string= "https://api.slack.com/changelog/2019-09-what-they-see-is-what-you-get-and-more-and-less"
                           (plist-get (cl-find-if #'(lambda (el) (string= "link" (plist-get el :type)))
                                                  elements)
                                      :url))))))))

(ert-deftest slack-test-block-to-mrkdwn ()
  (let* ((payload (list :type "rich_text_preformatted"
                        :elements (list (list :type "text" :text "code\nblock"))))
         (block (slack-create-rich-text-block-element payload)))
    (should (string= "```code\nblock```\n"
                     (slack-block-to-mrkdwn block))))
  (let* ((payload (list :type "rich_text_quote"
                        :elements (list (list :type "text" :text "block\nquote"))))
         (block (slack-create-rich-text-block-element payload)))
    (should (string= "> block\n> quote\n"
                     (slack-block-to-mrkdwn block))))
  (let* ((payload (list :type "rich_text_list"
                        :style "bullet"
                        :indent 1
                        :elements (list (list :type "rich_text_section"
                                              :elements (list (list :type "text" :text "list")))
                                        (list :type "rich_text_section"
                                              :elements (list (list :type "text" :text "bullet"))))))
         (block (slack-create-rich-text-block-element payload)))
    (should (string= "  - list\n  - bullet\n"
                     (slack-block-to-mrkdwn block))))
  (let* ((payload (list :type "rich_text_list"
                        :style "ordered"
                        :indent 0
                        :elements (list (list :type "rich_text_section"
                                              :elements (list (list :type "text" :text "list")))
                                        (list :type "rich_text_section"
                                              :elements (list (list :type "text" :text "ordered"))))))
         (block (slack-create-rich-text-block-element payload)))
    (should (string= "1. list\n2. ordered\n"
                     (slack-block-to-mrkdwn block))))
  (let* ((payload (list :type "text" :text "text")))
    (dolist (s (list (cons (list :bold t) "*text*")
                     (cons (list :italic t) "_text_")
                     (cons (list :strike t) "~text~")
                     (cons (list :code t) "`text`")))
      (should (string= (cdr s)
                       (slack-block-to-mrkdwn
                        (slack-create-rich-text-element
                         (plist-put payload :style (car s))))))))
  (slack-test-setup
    (let* ((payload (list :type "channel" :channel_id channel-id))
           (block (slack-create-rich-text-element payload))
           (mrkdwn (slack-block-to-mrkdwn block (list :team team))))
      (should (string= (format "<#%s> " channel-id)
                       mrkdwn))
      (should (string= (format "#%s" channel-name)
                       (get-text-property 0 'display mrkdwn)))
      (should (eq 'slack-message-mention-face
                  (get-text-property 0 'face mrkdwn)))
      (should (not (null (get-text-property 0 'slack-mention-props mrkdwn))))
      (should (null (get-text-property 1 'slack-mention-props mrkdwn))))
    (let* ((payload (list :type "user" :user_id user-id))
           (block (slack-create-rich-text-element payload))
           (mrkdwn (slack-block-to-mrkdwn block (list :team team))))
      (should (string= (format "<@%s> " user-id)
                       mrkdwn))
      (should (string= (format "@%s" display-name)
                       (get-text-property 0 'display mrkdwn))))
    (let* ((payload (list :type "usergroup" :usergroup_id usergroup-id))
           (block (slack-create-rich-text-element payload))
           (mrkdwn (slack-block-to-mrkdwn block (list :team team))))
      (should (string= (format "<!subteam^%s> " usergroup-id)
                       mrkdwn))
      (should (string= (format "@%s" usergroup-handle)
                       (get-text-property 0 'display mrkdwn))))
    (let* ((payload (list :type "broadcast" :range "here"))
           (block (slack-create-rich-text-element payload))
           (mrkdwn (slack-block-to-mrkdwn block)))
      (should (string= "<!here> " mrkdwn))
      (should (string= "@here"
                       (get-text-property 0 'display mrkdwn))))
    (let* ((payload (list :type "emoji" :name "smile"))
           (block (slack-create-rich-text-element payload)))
      (should (string= ":smile:" (slack-block-to-mrkdwn block))))
    (let* ((payload (list :type "link" :url "https://google.com"))
           (block (slack-create-rich-text-element payload)))
      (should (string= "https://google.com"
                       (slack-block-to-mrkdwn block))))))

(ert-deftest slack-test-message-get-text-falls-back-to-plain-text ()
  (slack-test-setup
    (let ((message (make-instance 'slack-message
                                  :type "message"
                                  :ts "1.0"
                                  :text "hello &lt;world&gt;"
                                  :blocks nil)))
      (should (equal "hello <world>"
                     (slack-message-get-text message team))))))

(ert-deftest slack-test-create-star-item-from-message-event ()
  (slack-test-setup
    (let* ((payload '(:type "star_added"
                      :item (:type "message"
                             :channel "C11111"
                             :message (:ts "1710000000.000100"))))
           (event (slack-create-star-event payload))
           (item (slack-event-create-star-item event team)))
      (should (string= "C11111" (oref item item-id)))
      (should (string= "message" (oref item item-type)))
      (should (string= "1710000000.000100" (oref item ts))))))

(ert-deftest slack-test-activity-feed-request-data-keeps-fields-after-cursor ()
  (let* ((body (slack-activity-feed--request-data "xoxc-token"
                                                  "chrono_reads_and_unreads"
                                                  "CURSOR123"))
         (cursor-pos (string-match (regexp-quote "name=\"cursor\"") body))
         (reason-pos (string-match (regexp-quote "name=\"_x_reason\"") body))
         (closing-pos (string-match (regexp-quote (format "--%s--"
                                                          slack-activity-feed-multipart-boundary))
                                    body)))
    (should cursor-pos)
    (should reason-pos)
    (should closing-pos)
    (should (< cursor-pos reason-pos))
    (should (< reason-pos closing-pos))
    (should (equal 1
                   (with-temp-buffer
                     (insert body)
                     (goto-char (point-min))
                     (how-many (regexp-quote (format "--%s--"
                                                     slack-activity-feed-multipart-boundary))
                               (point-min)
                               (point-max)))))))

(ert-deftest slack-test-activity-feed-prefetch-messages-calls-back-on-error ()
  (slack-test-setup
    (let* ((activity (make-instance 'slack-activity
                                    :is-unread t
                                    :feed-ts "1"
                                    :item (make-instance 'activity-item
                                                         :type "dm"
                                                         :message (make-instance 'activity-message
                                                                                 :ts "1710000000.000100"
                                                                                 :channel channel-id
                                                                                 :is-broadcast nil
                                                                                 :thread-ts nil
                                                                                 :author-id nil)
                                                         :reaction nil)))
           (called nil)
           (unavailable nil))
      (cl-letf (((symbol-function 'slack-conversations-history)
                 (lambda (_room _team &rest args)
                   (let ((on-error (plist-get args :on-error)))
                     (when (functionp on-error)
                       (funcall on-error :error-thrown '(error . "boom")))))))
        (slack-activity-feed--prefetch-messages
         (list activity)
         team
         (lambda ()
           (setq called t))
         nil
         (lambda (unavailable-activity)
           (setq unavailable unavailable-activity))))
      (should called)
      (should (eq unavailable activity)))))

(ert-deftest slack-test-activity-feed-prefetch-messages-rejects-nonmatching-history ()
  (slack-test-setup
    (let* ((activity (make-instance 'slack-activity
                                    :is-unread t
                                    :feed-ts "1"
                                    :item (make-instance 'activity-item
                                                         :type "dm"
                                                         :message (make-instance 'activity-message
                                                                                 :ts "1710000000.000100"
                                                                                 :channel channel-id
                                                                                 :is-broadcast nil
                                                                                 :thread-ts nil
                                                                                 :author-id nil)
                                                         :reaction nil)))
           (called nil)
           (messages-called nil)
           (unavailable nil))
      (cl-letf (((symbol-function 'slack-conversations-history)
                 (lambda (room _team &rest args)
                   (funcall (plist-get args :after-success)
                            (list (make-instance 'slack-message
                                                 :type "message"
                                                 :channel (oref room id)
                                                 :ts "1710000000.000099"
                                                 :text "older"))))))
        (slack-activity-feed--prefetch-messages
         (list activity)
         team
         (lambda ()
           (setq called t))
         (lambda (&rest _)
           (setq messages-called t))
         (lambda (unavailable-activity)
           (setq unavailable unavailable-activity))))
      (should called)
      (should-not messages-called)
      (should (eq unavailable activity)))))

(ert-deftest slack-test-activity-feed-prefetch-rooms-calls-back-on-error ()
  (slack-test-setup
    (let* ((activity (make-instance 'slack-activity
                                    :is-unread t
                                    :feed-ts "1"
                                    :item (make-instance 'activity-item
                                                         :type "dm"
                                                         :message (make-instance 'activity-message
                                                                                 :ts "1710000000.000100"
                                                                                 :channel "C99999"
                                                                                 :is-broadcast nil
                                                                                 :thread-ts nil
                                                                                 :author-id nil)
                                                         :reaction nil)))
           (called nil))
      (cl-letf (((symbol-function 'slack-conversations-info)
                 (lambda (_channel-id _team _after-success on-error)
                   (when (functionp on-error)
                     (funcall on-error "boom")))))
        (slack-activity-feed--prefetch-rooms
         (list activity)
         team
         (lambda ()
           (setq called t))))
      (should called))))

(ert-deftest slack-test-activity-feed-display-hydrates-missing-messages ()
  (slack-test-setup
    (let ((displayed nil)
          (room-prefetch-started nil)
          (hydration-started nil)
          (prefetched-replaced nil)
          (activity
           (make-instance
            'slack-activity
            :is-unread nil
            :feed-ts "1710000000.000100"
            :item (make-instance
                   'activity-item
                   :type "channel_message"
                   :message (make-instance
                             'activity-message
                             :ts "1710000000.000100"
                             :channel channel-id
                             :is-broadcast nil
                             :thread-ts nil
                             :author-id nil)
                   :reaction nil))))
      (cl-letf (((symbol-function 'slack-activity-feed--prefetch-rooms)
                 (lambda (_activities _team callback)
                   (setq room-prefetch-started t)
                   (funcall callback)))
                ((symbol-function 'slack-activity-feed--prefetch-messages)
                 (lambda (_activities _team callback &optional messages-callback
                                     _unavailable-callback)
                   (setq hydration-started t)
                   (when messages-callback
                     (funcall messages-callback
                              channel
                              (list (make-instance
                                     'slack-message
                                     :type "message"
                                     :channel channel-id
                                     :ts "1710000000.000100"
                                     :text "hello"))))
                   (funcall callback)))
                ((symbol-function
                  'slack-activity-feed--replace-prefetched-messages)
                 (lambda (_team _messages)
                   (setq prefetched-replaced t)))
                ((symbol-function 'slack-create-activity-feed-buffer)
                 (lambda (_activity-feed _team)
                   'buffer))
                ((symbol-function 'slack-buffer-display)
                 (lambda (_buffer)
                   (setq displayed t))))
        (slack-activity-feed--display-activities
         (list activity)
         team
         nil))
      (should displayed)
      (should room-prefetch-started)
      (should hydration-started)
      (should prefetched-replaced))))

(ert-deftest slack-test-activity-feed-hydration-does-not-redisplay-buffer ()
  (slack-test-setup
    (let ((display-count 0)
          (replaced-prefetched-messages nil)
          (activity
           (make-instance
            'slack-activity
            :is-unread t
            :feed-ts "1710000000.000100"
            :item (make-instance
                   'activity-item
                   :type "channel_message"
                   :message (make-instance
                             'activity-message
                             :ts "1710000000.000100"
                             :channel channel-id
                             :is-broadcast nil
                             :thread-ts nil
                             :author-id nil)
                   :reaction nil))))
      (cl-letf (((symbol-function 'slack-create-activity-feed-buffer)
                 (lambda (_activity-feed _team)
                   'buffer))
                ((symbol-function 'slack-buffer-display)
                 (lambda (_buffer)
                   (cl-incf display-count)))
                ((symbol-function 'slack-activity-feed--prefetch-rooms)
                 (lambda (_activities _team callback)
                   (funcall callback)))
                ((symbol-function 'slack-activity-feed--prefetch-messages)
                 (lambda (_activities _team callback &optional messages-callback
                                     _unavailable-callback)
                   (when messages-callback
                     (funcall messages-callback
                              channel
                              (list (make-instance
                                     'slack-message
                                     :type "message"
                                     :channel channel-id
                                     :ts "1710000000.000100"
                                     :text "hello"))))
                   (funcall callback)))
                ((symbol-function
                  'slack-activity-feed--replace-prefetched-messages)
                 (lambda (&rest _)
                   (setq replaced-prefetched-messages t))))
        (slack-activity-feed--display-activities
         (list activity)
         team
         nil))
      (should (= 1 display-count))
      (should replaced-prefetched-messages))))

(ert-deftest slack-test-activity-feed-watched-open-marks-channel-read ()
  (slack-test-setup
    (let* ((slack-has-unreads t)
           (slack-unread-count 2)
           (message-ts "1710000001.000100")
           (activity
            (make-instance
             'slack-activity
             :is-unread t
             :feed-ts message-ts
             :item (make-instance
                    'activity-item
                    :type "channel_message"
                    :message (make-instance
                              'activity-message
                              :ts message-ts
                              :channel channel-id
                              :is-broadcast nil
                              :thread-ts nil
                              :author-id nil)
                    :reaction nil)))
           (feed-buffer
            (make-instance 'slack-activity-feed-buffer
                           :team-id (oref team id)
                           :room-id "__activity-feed__"
                           :cached-team team
                           :activity-feed
                           (make-instance 'slack-activity-feed
                                          :activities (list activity))))
           (source-buffer (generate-new-buffer " *slack-test-activity-feed*"))
           (marked-channel nil)
           (marked-ts nil)
           (activity-mark-requested nil)
           (counts-updated nil))
      (unwind-protect
          (with-current-buffer source-buffer
            (slack-activity-feed-buffer-mode)
            (slack-buffer-set-current-buffer feed-buffer)
            (oset feed-buffer buf source-buffer)
            (slack-buffer-insert feed-buffer activity)
            (goto-char (point-min))
            (re-search-forward "Loading message...")
            (cl-letf (((symbol-function 'slack-conversations-mark)
                       (lambda (room _team ts after-success &optional _after-error)
                         (setq marked-channel (oref room id)
                               marked-ts ts)
                         (funcall after-success)))
                      ((symbol-function 'slack-request)
                       (lambda (&rest _)
                         (setq activity-mark-requested t)))
                      ((symbol-function 'slack-counts-update)
                       (lambda (_team)
                         (setq counts-updated t))))
              (slack-activity-feed--mark-read team))
            (should (equal channel-id marked-channel))
            (should (equal message-ts marked-ts))
            (should-not activity-mark-requested)
            (should counts-updated)
            (should-not (oref activity is-unread))
            (should (= 1 slack-unread-count))
            (should slack-has-unreads))
        (kill-buffer source-buffer)))))

(ert-deftest slack-test-activity-feed-display-thread-opens-thread-entry ()
  (slack-test-setup
    (let* ((reply-ts "1710000001.000100")
           (thread-ts "1710000000.000000")
           (activity
            (make-instance
             'slack-activity
             :is-unread t
             :feed-ts reply-ts
             :item (make-instance
                    'activity-item
                    :type "thread_v2"
                    :message (make-instance
                              'activity-message
                              :ts reply-ts
                              :channel channel-id
                              :is-broadcast nil
                              :thread-ts thread-ts
                              :author-id nil)
                    :reaction nil)))
           (feed-buffer
            (make-instance 'slack-activity-feed-buffer
                           :team-id (oref team id)
                           :room-id "__activity-feed__"
                           :cached-team team
                           :activity-feed
                           (make-instance 'slack-activity-feed
                                          :activities (list activity))))
           opened
           marked-team)
      (cl-letf (((symbol-function 'slack-open-message)
                 (lambda (&rest args)
                   (setq opened args)))
                ((symbol-function 'slack-activity-feed--mark-read)
                 (lambda (team)
                   (setq marked-team team)))
                ((symbol-function 'slack-team-ensure-registered)
                 #'ignore))
        (slack-buffer-display-thread feed-buffer reply-ts))
      (should (eq marked-team team))
      (should (equal opened
                     (list team channel thread-ts thread-ts reply-ts))))))

(ert-deftest slack-test-stars-buffer-display-thread-opens-thread ()
  "Pressing RET on a saved item's thread-status link opens its thread."
  (slack-test-setup
    (let* ((thread-ts "1710000000.000000")
           (parent (make-instance 'slack-message
                                  :type "message"
                                  :channel channel-id
                                  :ts thread-ts
                                  :thread_ts thread-ts
                                  :reply_count 2))
           (buf (make-instance 'slack-stars-buffer
                               :team-id (oref team id)
                               :room-id "__saved-items__"))
           shown)
      (slack-buffer-cache-team buf team)
      (slack-room-set-messages channel (list parent) team)
      (cl-letf (((symbol-function 'slack-buffer-room)
                 (lambda (_this) channel))
                ((symbol-function 'slack-thread-show-messages)
                 (lambda (message room thread-team &rest _)
                   (setq shown (list message room thread-team)))))
        (slack-buffer-display-thread buf thread-ts))
      (should (equal shown (list parent channel team))))))

(ert-deftest slack-test-pinned-items-buffer-display-thread-opens-thread ()
  "Pressing RET on a pinned message's thread-status link opens its thread."
  (slack-test-setup
    (let* ((thread-ts "1710000000.000000")
           (parent (make-instance 'slack-message
                                  :type "message"
                                  :channel channel-id
                                  :ts thread-ts
                                  :thread_ts thread-ts
                                  :reply_count 2))
           (buf (make-instance 'slack-pinned-items-buffer
                               :team-id (oref team id)
                               :room-id channel-id
                               :items (list (make-instance 'slack-pinned-item
                                                           :message parent))))
           shown)
      (slack-buffer-cache-team buf team)
      (cl-letf (((symbol-function 'slack-buffer-room)
                 (lambda (_this) channel))
                ((symbol-function 'slack-thread-show-messages)
                 (lambda (message room thread-team &rest _)
                   (setq shown (list message room thread-team)))))
        (slack-buffer-display-thread buf thread-ts))
      (should (equal shown (list parent channel team))))))

(ert-deftest slack-test-search-result-buffer-display-thread-opens-thread ()
  "Pressing RET on a search match's thread-status link opens its thread."
  (slack-test-setup
    (let* ((thread-ts "1710000000.000000")
           (parent (make-instance 'slack-message
                                  :type "message"
                                  :channel channel-id
                                  :ts thread-ts
                                  :thread_ts thread-ts
                                  :reply_count 2))
           (match (make-instance 'slack-search-message
                                 :message parent
                                 :channel (make-instance
                                           'slack-search-message-channel
                                           :id channel-id
                                           :name channel-name)
                                 :user user-id
                                 :username user-name
                                 :permalink "https://example.com/p"))
           (search-result (make-instance 'slack-search-result
                                         :matches (list match)))
           (buf (make-instance 'slack-search-result-buffer
                               :team-id (oref team id)
                               :search-result search-result))
           shown)
      (slack-buffer-cache-team buf team)
      (cl-letf (((symbol-function 'slack-thread-show-messages)
                 (lambda (message room thread-team &rest _)
                   (setq shown (list message room thread-team)))))
        (slack-buffer-display-thread buf thread-ts))
      (should (equal shown (list parent channel team))))))

(ert-deftest slack-test-activity-feed-open-loaded-parent-keeps-thread-ts ()
  (slack-test-setup
    (let* ((reply-ts "1710000001.000100")
           (thread-ts "1710000000.000000")
           (parent (make-instance 'slack-message
                                  :type "message"
                                  :channel channel-id
                                  :ts thread-ts
                                  :text "Parent message"
                                  :reactions nil))
           (reply (make-instance 'slack-message
                                 :type "message"
                                 :channel channel-id
                                 :ts reply-ts
                                 :thread_ts thread-ts))
           (activity
            (make-instance
             'slack-activity
             :is-unread t
             :feed-ts reply-ts
             :feed-key "feed-key"
             :item (make-instance
                    'activity-item
                    :type "thread_v2"
                    :message (make-instance
                              'activity-message
                              :ts reply-ts
                              :channel channel-id
                              :is-broadcast nil
                              :thread-ts thread-ts
                              :author-id nil)
                    :reaction nil)))
           (feed-buffer
            (make-instance 'slack-activity-feed-buffer
                           :team-id (oref team id)
                           :room-id "__activity-feed__"
                           :cached-team team
                           :activity-feed
                           (make-instance 'slack-activity-feed
                                          :activities (list activity))))
           (source-buffer (generate-new-buffer " *slack-test-activity-feed*"))
           requested-thread-ts)
      (unwind-protect
          (progn
            (slack-room-set-messages channel (list parent reply) team)
            (with-current-buffer source-buffer
              (slack-activity-feed-buffer-mode)
              (slack-buffer-set-current-buffer feed-buffer)
              (oset feed-buffer buf source-buffer)
              (let ((inhibit-read-only t))
                (insert (propertize "Activity entry"
                                    'ts reply-ts
                                    'room-id channel-id
                                    'thread-ts thread-ts)))
              (goto-char (point-min))
              (cl-letf (((symbol-function 'slack-conversations-replies)
                         (lambda (_room ts _team &rest _args)
                           (setq requested-thread-ts ts)))
                        ((symbol-function 'slack-activity-feed--mark-read)
                         #'ignore))
                (slack-feed-open-at-point)))
            (should (equal thread-ts requested-thread-ts)))
        (kill-buffer source-buffer)))))

(ert-deftest slack-test-activity-feed-mark-read-updates-cache ()
  (slack-test-setup
    (let* ((slack-activity-feed--cache (make-hash-table :test 'equal))
           (slack-has-unreads t)
           (slack-unread-count 2)
           (message-ts "1710000001.000100")
           (cached-activity
            (make-instance
             'slack-activity
             :is-unread t
             :feed-ts message-ts
             :item (make-instance
                    'activity-item
                    :type "channel_message"
                    :message (make-instance
                              'activity-message
                              :ts message-ts
                              :channel channel-id
                              :is-broadcast nil
                              :thread-ts nil
                              :author-id nil)
                    :reaction nil)))
           (visible-activity
            (make-instance
             'slack-activity
             :is-unread t
             :feed-ts message-ts
             :item (make-instance
                    'activity-item
                    :type "channel_message"
                    :message (make-instance
                              'activity-message
                              :ts message-ts
                              :channel channel-id
                              :is-broadcast nil
                              :thread-ts nil
                              :author-id nil)
                    :reaction nil)))
           (feed-buffer
            (make-instance 'slack-activity-feed-buffer
                           :team-id (oref team id)
                           :room-id "__activity-feed__"
                           :cached-team team
                           :activity-feed
                           (make-instance 'slack-activity-feed
                                          :activities
                                          (list visible-activity))))
           (source-buffer (generate-new-buffer " *slack-test-activity-feed*")))
      (unwind-protect
          (progn
            (slack-activity-feed--cache-put team (list cached-activity) nil)
            (with-current-buffer source-buffer
              (insert "\u25cf #TestChannel\nTODO\n")
              (goto-char (point-max))
              (cl-letf (((symbol-function 'force-mode-line-update)
                         (lambda (&rest _) nil)))
                (slack-activity-feed--on-marked-read
                 feed-buffer source-buffer (point) message-ts)))
            (should-not (oref visible-activity is-unread))
            (should-not (oref cached-activity is-unread))
            (should (= 1 slack-unread-count))
            (should slack-has-unreads))
        (kill-buffer source-buffer)))))

(ert-deftest slack-test-activity-feed-mark-read-removes-unread-cache-entry ()
  (slack-test-setup
    (let* ((slack-activity-feed--cache (make-hash-table :test 'equal))
           (message-ts "1710000001.000100")
           (cached-activity
            (make-instance
             'slack-activity
             :is-unread t
             :feed-ts message-ts
             :item (make-instance
                    'activity-item
                    :type "channel_message"
                    :message (make-instance
                              'activity-message
                              :ts message-ts
                              :channel channel-id
                              :is-broadcast nil
                              :thread-ts nil
                              :author-id nil)
                    :reaction nil)))
           (visible-activity
            (make-instance
             'slack-activity
             :is-unread t
             :feed-ts message-ts
             :item (make-instance
                    'activity-item
                    :type "channel_message"
                    :message (make-instance
                              'activity-message
                              :ts message-ts
                              :channel channel-id
                              :is-broadcast nil
                              :thread-ts nil
                              :author-id nil)
                    :reaction nil)))
           (feed-buffer
            (make-instance 'slack-activity-feed-buffer
                           :team-id (oref team id)
                           :room-id "__activity-feed__"
                           :cached-team team
                           :activity-feed
                           (make-instance 'slack-activity-feed
                                          :activities
                                          (list visible-activity))))
           (source-buffer (generate-new-buffer " *slack-test-activity-feed*")))
      (unwind-protect
          (let ((slack-activity-feed-mode-show-only-unread t))
            (slack-activity-feed--cache-put team (list cached-activity) nil)
            (with-current-buffer source-buffer
              (insert "\u25cf #TestChannel\nTODO\n")
              (goto-char (point-max))
              (cl-letf (((symbol-function 'force-mode-line-update)
                         (lambda (&rest _) nil)))
                (slack-activity-feed--on-marked-read
                 feed-buffer source-buffer (point) message-ts)))
            (should-not
             (plist-get (slack-activity-feed--cache-get team) :activities)))
        (kill-buffer source-buffer)))))

(ert-deftest slack-test-activity-feed-displays-before-room-prefetch-finishes ()
  (slack-test-setup
    (let ((displayed nil)
          (room-prefetch-started nil)
          (hydration-started nil)
          (activity
           (make-instance
            'slack-activity
            :is-unread nil
            :feed-ts "1710000000.000100"
            :item (make-instance
                   'activity-item
                   :type "channel_message"
                   :message (make-instance
                             'activity-message
                             :ts "1710000000.000100"
                             :channel "C99999"
                             :is-broadcast nil
                             :thread-ts nil
                             :author-id nil)
                   :reaction nil))))
      (cl-letf (((symbol-function 'slack-activity-feed--prefetch-rooms)
                 (lambda (_activities _team _callback)
                   (setq room-prefetch-started t)))
                ((symbol-function 'slack-activity-feed--prefetch-messages)
                 (lambda (&rest _)
                   (setq hydration-started t)))
                ((symbol-function 'slack-create-activity-feed-buffer)
                 (lambda (_activity-feed _team)
                   'buffer))
                ((symbol-function 'slack-buffer-display)
                 (lambda (_buffer)
                   (setq displayed t))))
        (slack-activity-feed--display-activities
         (list activity)
         team
         nil))
      (should displayed)
      (should room-prefetch-started)
      (should-not hydration-started))))

(ert-deftest slack-test-activity-feed-shows-before-watched-fetch-finishes ()
  (slack-test-setup
    (let ((displayed-counts nil)
          (watched-fetch-started nil)
          (data (list :items (list (list :feed_ts "1710000000.000100"
                                         :item (list :type "channel_message"
                                                     :message (list :ts "1710000000.000100"
                                                                    :channel channel-id))))
                      :response_metadata nil)))
      (cl-letf (((symbol-function 'slack-activity-feed--display-activities)
                 (lambda (activities _team _pagination)
                   (push (length activities) displayed-counts)))
                ((symbol-function 'slack-activity-feed--fetch-watched-activities)
                 (lambda (_team _callback)
                   (setq watched-fetch-started t))))
        (slack-activity-feed--show-data data team))
      (should (equal '(1) displayed-counts))
      (should watched-fetch-started))))

(ert-deftest slack-test-activity-feed-watched-fetch-updates-cache-only ()
  (slack-test-setup
    (let* ((slack-activity-feed--cache (make-hash-table :test 'equal))
           (displayed-counts nil)
           (extra (make-instance
                   'slack-activity
                   :is-unread t
                   :feed-ts "1710000001.000100"
                   :item (make-instance
                          'activity-item
                          :type "channel_message"
                          :message (make-instance
                                    'activity-message
                                    :ts "1710000001.000100"
                                    :channel channel-id
                                    :is-broadcast nil
                                    :thread-ts nil
                                    :author-id nil)
                          :reaction nil)))
           (data (list :items (list (list :feed_ts "1710000000.000100"
                                          :item (list :type "channel_message"
                                                      :message
                                                      (list :ts "1710000000.000100"
                                                            :channel channel-id))))
                       :response_metadata nil)))
      (cl-letf (((symbol-function 'slack-activity-feed--display-activities)
                 (lambda (activities _team _pagination)
                   (push (length activities) displayed-counts)))
                ((symbol-function 'slack-activity-feed--fetch-watched-activities)
                 (lambda (_team callback)
                   (funcall callback (list extra))))
                ((symbol-function 'slack-activity-feed--visible-p)
                 (lambda (_team) nil)))
        (slack-activity-feed--show-data data team))
      (should (equal '(1) displayed-counts))
      (should (= 2 (length (plist-get (slack-activity-feed--cache-get team)
                                      :activities)))))))

(ert-deftest slack-test-activity-feed-show-renders-cache-before-refresh ()
  (slack-test-setup
    (let* ((slack-activity-feed--cache (make-hash-table :test 'equal))
           (slack-current-team team)
           (cached-activity (make-instance
                             'slack-activity
                             :is-unread nil
                             :feed-ts "1710000000.000100"
                             :item (make-instance
                                    'activity-item
                                    :type "channel_message"
                                    :message (make-instance
                                              'activity-message
                                              :ts "1710000000.000100"
                                              :channel channel-id
                                              :is-broadcast nil
                                              :thread-ts nil
                                              :author-id nil)
                                    :reaction nil)))
           (fresh-activity (make-instance
                            'slack-activity
                            :is-unread nil
                            :feed-ts "1710000001.000100"
                            :item (make-instance
                                   'activity-item
                                   :type "channel_message"
                                   :message (make-instance
                                             'activity-message
                                             :ts "1710000001.000100"
                                             :channel channel-id
                                             :is-broadcast nil
                                             :thread-ts nil
                                             :author-id nil)
                                   :reaction nil)))
           (displayed nil)
           (refresh-started nil))
      (slack-activity-feed--cache-put team (list cached-activity) nil)
      (cl-letf (((symbol-function 'slack-activity-feed--display-snapshot)
                 (lambda (snapshot _team)
                   (push (plist-get snapshot :activities) displayed)))
                ((symbol-function 'slack-activity-feed--refresh-cache)
                 (lambda (team after-refresh &optional _quiet)
                   (setq refresh-started t)
                   (let ((old-snapshot (slack-activity-feed--cache-get team))
                         (new-snapshot (slack-activity-feed--cache-put
                                        team (list fresh-activity) nil)))
                     (funcall after-refresh old-snapshot new-snapshot))))
                ((symbol-function 'run-at-time)
                 (lambda (&rest _args)
                   (error "activity feed show must not start a timer")))
                ((symbol-function 'slack-activity-feed-request)
                 (lambda (&rest _args)
                   (error "show should use cache before requesting"))))
        (slack-activity-feed-show))
      (should (equal (list (list fresh-activity) (list cached-activity))
                     displayed))
      (should refresh-started))))

(ert-deftest slack-test-activity-feed-watched-message-updates-existing-cache ()
  (slack-test-setup
    (let* ((slack-activity-feed--cache (make-hash-table :test 'equal))
           (slack-activity-feed-watch-channels (list channel-name))
           (old-activity (make-instance
                          'slack-activity
                          :is-unread t
                          :feed-ts "1710000000.000100"
                          :item (make-instance
                                 'activity-item
                                 :type "channel_message"
                                 :message (make-instance
                                           'activity-message
                                           :ts "1710000000.000100"
                                           :channel channel-id
                                           :is-broadcast nil
                                           :thread-ts nil
                                           :author-id nil)
                                 :reaction nil)))
           (displayed nil))
      (oset channel last-read "1710000000.000000")
      (slack-activity-feed--cache-put team (list old-activity) nil)
      (cl-letf (((symbol-function 'force-mode-line-update)
                 (lambda (&rest _) nil))
                ((symbol-function 'slack-activity-feed--display-activities)
                 (lambda (&rest _)
                   (setq displayed t))))
        (slack-activity-feed-watch-channel-message
         (make-instance 'slack-message
                        :type "message"
                        :channel channel-id
                        :ts "1710000001.000000"
                        :text "new")
         channel
         team))
      (let ((activities (plist-get (slack-activity-feed--cache-get team)
                                   :activities)))
        (should (= 2 (length activities)))
        (should (equal "1710000001.000000" (oref (car activities) feed-ts)))
        (should (equal "1710000000.000100" (oref (cadr activities) feed-ts))))
      (should-not displayed))))

(ert-deftest slack-test-activity-feed-watched-room-resolves-name-or-id ()
  (slack-test-setup
    (should (eq channel
                (slack-activity-feed--watched-room channel-id team)))
    (should (eq channel
                (slack-activity-feed--watched-room channel-name team)))))

(ert-deftest slack-test-activity-feed-watch-channel-limit-accepts-string ()
  (let ((slack-activity-feed-watch-channel-limit "50"))
    (should (equal "50"
                   (slack-activity-feed--watch-channel-limit)))))

(ert-deftest slack-test-activity-feed-fetches-watched-channel-messages ()
  (slack-test-setup
    (let ((slack-activity-feed-watch-channels (list channel-name))
          (called-channel nil)
          (result nil))
      (cl-letf (((symbol-function 'slack-conversations-history)
                 (lambda (room _team &rest args)
                   (setq called-channel (oref room id))
                   (funcall (plist-get args :after-success)
                            (list (make-instance 'slack-message
                                                 :type "message"
                                                 :channel (oref room id)
                                                 :ts "1710000000.000100"
                                                 :text "hello")))))
                ((symbol-function 'slack-conversations-info)
                 (lambda (_channel-id _team after-success &optional _on-error)
                   (funcall after-success))))
        (slack-activity-feed--fetch-watched-activities
         team
         (lambda (activities)
           (setq result activities))))
      (should (equal channel-id called-channel))
      (should (= 1 (length result)))
      (should (equal "channel_message"
                     (oref (oref (car result) item) type)))
      (should (oref (car result) is-unread))
      (should (equal channel-id
                     (oref (oref (oref (car result) item) message)
                           channel))))))

(ert-deftest slack-test-activity-feed-refreshes-watched-channel-last-read ()
  (slack-test-setup
    (let ((slack-activity-feed-watch-channels (list channel-name))
          (result nil))
      (oset channel last-read "0")
      (cl-letf (((symbol-function 'slack-conversations-info)
                 (lambda (channel-id _team after-success &optional _on-error)
                   (should (equal channel-id (oref channel id)))
                   (oset channel last-read "1710000001.000100")
                   (funcall after-success)))
                ((symbol-function 'slack-conversations-history)
                 (lambda (room _team &rest args)
                   (funcall (plist-get args :after-success)
                            (list (make-instance 'slack-message
                                                 :type "message"
                                                 :channel (oref room id)
                                                 :ts "1710000000.000100"
                                                 :text "read"))))))
        (slack-activity-feed--fetch-watched-activities
         team
         (lambda (activities)
           (setq result activities))))
      (should (= 1 (length result)))
      (should-not (oref (car result) is-unread)))))

(ert-deftest slack-test-activity-feed-unread-mode-filters-read-watched-messages ()
  (slack-test-setup
    (let* ((slack-activity-feed-mode-show-only-unread t)
           (slack-activity-feed--cache (make-hash-table :test 'equal))
           (displayed nil)
           (read-watched
            (make-instance
             'slack-activity
             :is-unread nil
             :feed-ts "1710000000.000100"
             :item (make-instance
                    'activity-item
                    :type "channel_message"
                    :message (make-instance
                              'activity-message
                              :ts "1710000000.000100"
                              :channel channel-id
                              :is-broadcast nil
                              :thread-ts nil
                              :author-id nil)
                    :reaction nil)))
           (data (list :items nil :response_metadata nil)))
      (cl-letf (((symbol-function 'slack-activity-feed--display-activities)
                 (lambda (activities _team _pagination)
                   (setq displayed activities)))
                ((symbol-function 'slack-activity-feed--fetch-watched-activities)
                 (lambda (_team callback)
                   (funcall callback (list read-watched))))
                ((symbol-function 'slack-activity-feed--visible-p)
                 (lambda (_team) nil)))
        (slack-activity-feed--show-data data team))
      (should-not displayed)
      (should-not (plist-get (slack-activity-feed--cache-get team)
                             :activities)))))

(ert-deftest slack-test-activity-feed-watched-channel-respects-last-read ()
  (slack-test-setup
    (oset channel last-read "1710000001.000100")
    (let ((activity
           (slack-activity-feed--message-activity
            (make-instance 'slack-message
                           :type "message"
                           :channel channel-id
                           :ts "1710000000.000100"
                           :text "old")
            channel)))
      (should-not (oref activity is-unread)))))

(ert-deftest slack-test-activity-feed-unread-count-includes-watched-channels ()
  (slack-test-setup
    (let ((slack-activity-feed-watch-channels (list channel-name))
          (result nil))
      (cl-letf (((symbol-function 'slack-activity-feed-request)
                 (lambda (_team after-success &optional _cursor _on-error)
                   (funcall after-success (list :items nil))))
                ((symbol-function 'slack-conversations-info)
                 (lambda (_channel-id _team after-success &optional _on-error)
                   (funcall after-success)))
                ((symbol-function 'slack-conversations-history)
                 (lambda (room _team &rest args)
                   (funcall (plist-get args :after-success)
                            (list (make-instance 'slack-message
                                                 :type "message"
                                                 :channel (oref room id)
                                                 :ts "1710000000.000100"
                                                 :text "hello"))))))
        (slack-activity-feed--fetch-unread-count
         team
         (lambda (count)
           (setq result count))))
      (should (= 1 result)))))

(ert-deftest slack-test-activity-feed-unread-count-refreshes-activity-cache ()
  (slack-test-setup
    (let* ((slack-activity-feed--cache (make-hash-table :test 'equal))
           (old-activity (make-instance
                          'slack-activity
                          :is-unread nil
                          :feed-ts "1710000000.000100"
                          :item (make-instance
                                 'activity-item
                                 :type "at_user"
                                 :message (make-instance
                                           'activity-message
                                           :ts "1710000000.000100"
                                           :channel channel-id
                                           :is-broadcast nil
                                           :thread-ts nil
                                           :author-id nil)
                                 :reaction nil)))
           (new-item (list :is_unread t
                           :feed_ts "1710000001.000100"
                           :item (list :type "at_user"
                                       :message
                                       (list :ts "1710000001.000100"
                                             :channel channel-id))))
           (result nil))
      (let ((slack-activity-feed-mode-show-only-unread nil))
        (slack-activity-feed--cache-put team (list old-activity) nil))
      (cl-letf (((symbol-function 'slack-activity-feed-request)
                 (lambda (_team after-success &optional _cursor _on-error)
                   (funcall after-success
                            (list :items (list new-item)
                                  :response_metadata
                                  (list :next_cursor "next")))))
                ((symbol-function 'slack-activity-feed--fetch-watched-activities)
                 (lambda (_team callback)
                   (funcall callback nil)))
                ((symbol-function 'slack-activity-feed--visible-p)
                 (lambda (&rest _)
                   nil)))
        (slack-activity-feed--fetch-unread-count
         team
         (lambda (count)
           (setq result count))))
      (let ((slack-activity-feed-mode-show-only-unread nil))
        (let ((activities (plist-get (slack-activity-feed--cache-get team)
                                     :activities)))
          (should (= 2 (length activities)))
          (should (equal "1710000001.000100" (oref (car activities) feed-ts)))
          (should (equal "1710000000.000100"
                         (oref (cadr activities) feed-ts)))))
      (let ((slack-activity-feed-mode-show-only-unread t))
        (let ((snapshot (slack-activity-feed--cache-get team)))
          (should (equal "next" (plist-get snapshot :pagination)))
          (should (= 1 (length (plist-get snapshot :activities))))
          (should (equal "1710000001.000100"
                         (oref (car (plist-get snapshot :activities))
                               feed-ts)))))
      (should (= 1 result)))))

(ert-deftest slack-test-activity-feed-watched-channel-message-updates-unread-summary ()
  (slack-test-setup
    (let ((slack-activity-feed-watch-channels (list channel-name))
          (slack-has-unreads nil)
          (slack-unread-count 0))
      (oset channel last-read "1710000000.000000")
      (cl-letf (((symbol-function 'force-mode-line-update)
                 (lambda (&rest _) nil)))
        (slack-activity-feed-watch-channel-message
         (make-instance 'slack-message
                        :type "message"
                        :channel channel-id
                        :ts "1710000001.000000"
                        :text "hello")
         channel
         team))
      (should slack-has-unreads)
      (should (= 1 slack-unread-count)))))

(ert-deftest slack-test-activity-feed-merges-watched-activities-newest-first ()
  (let* ((old (make-instance 'slack-activity
                             :is-unread t
                             :feed-ts "1710000000.000100"
                             :item (make-instance
                                    'activity-item
                                    :type "at_user"
                                    :message (make-instance 'activity-message
                                                            :ts "1710000000.000100"
                                                            :channel "C11111"
                                                            :is-broadcast nil
                                                            :thread-ts nil
                                                            :author-id nil)
                                    :reaction nil)))
         (new (make-instance 'slack-activity
                             :is-unread nil
                             :feed-ts "1710000001.000100"
                             :item (make-instance
                                    'activity-item
                                    :type "channel_message"
                                    :message (make-instance 'activity-message
                                                            :ts "1710000001.000100"
                                                            :channel "C11111"
                                                            :is-broadcast nil
                                                            :thread-ts nil
                                                            :author-id nil)
                                    :reaction nil)))
         (duplicate (make-instance 'slack-activity
                                   :is-unread nil
                                   :feed-ts "1710000000.000100"
                                   :item (make-instance
                                          'activity-item
                                          :type "channel_message"
                                          :message (make-instance 'activity-message
                                                                  :ts "1710000000.000100"
                                                                  :channel "C11111"
                                                                  :is-broadcast nil
                                                                  :thread-ts nil
                                                                  :author-id nil)
                                          :reaction nil)))
         (merged (slack-activity-feed--merge-activities
                  (list old)
                  (list new duplicate))))
    (should (= 2 (length merged)))
    (should (eq new (car merged)))
    (should (eq old (cadr merged)))))

(ert-deftest slack-test-activity-feed-merge-preserves-distinct-feed-items ()
  (let* ((mention (make-instance 'slack-activity
                                 :is-unread t
                                 :feed-ts "1710000002.000100"
                                 :feed-key "mention-key"
                                 :item (make-instance
                                        'activity-item
                                        :type "at_user"
                                        :message (make-instance 'activity-message
                                                                :ts "1710000000.000100"
                                                                :channel "C11111"
                                                                :is-broadcast nil
                                                                :thread-ts nil
                                                                :author-id nil)
                                        :reaction nil)))
         (reaction (make-instance 'slack-activity
                                  :is-unread t
                                  :feed-ts "1710000001.000100"
                                  :feed-key "reaction-key"
                                  :item (make-instance
                                         'activity-item
                                         :type "message_reaction"
                                         :message (make-instance 'activity-message
                                                                 :ts "1710000000.000100"
                                                                 :channel "C11111"
                                                                 :is-broadcast nil
                                                                 :thread-ts nil
                                                                 :author-id nil)
                                         :reaction (make-instance 'activity-reaction
                                                                  :user "U11111"
                                                                  :name "thumbsup"))))
         (watched-duplicate (make-instance 'slack-activity
                                           :is-unread t
                                           :feed-ts "1710000000.000100"
                                           :item (make-instance
                                                  'activity-item
                                                  :type "channel_message"
                                                  :message (make-instance 'activity-message
                                                                          :ts "1710000000.000100"
                                                                          :channel "C11111"
                                                                          :is-broadcast nil
                                                                          :thread-ts nil
                                                                          :author-id nil)
                                                  :reaction nil)))
         (merged (slack-activity-feed--merge-activities
                  (list mention reaction)
                  (list watched-duplicate))))
    (should (equal (list mention reaction) merged))))

(ert-deftest slack-test-stars-prefetch-messages-calls-back-on-error ()
  (slack-test-setup
    (let ((called nil)
          (item (slack-create-star-item
                 (list :item_id channel-id
                       :item_type "message"
                       :ts "1710000000.000100"))))
      (cl-letf (((symbol-function 'slack-conversations-history)
                 (lambda (_room _team &rest args)
                   (let ((on-error (plist-get args :on-error)))
                     (when (functionp on-error)
                       (funcall on-error :error-thrown '(error . "boom")))))))
        (slack-stars--prefetch-messages
         (list item)
         team
         (lambda ()
           (setq called t))))
      (should called))))

(ert-deftest slack-test-scheduled-message-blocks-json-escapes-text ()
  (let* ((text "hello \"team\"\npath\\value")
         (payload (json-parse-string
                   (slack-scheduled-messages--draft-blocks-json text)
                   :object-type 'plist
                   :array-type 'list)))
    (should (string= text
                     (plist-get
                      (car (plist-get
                            (car (plist-get
                                  (car payload)
                                  :elements))
                            :elements))
                      :text)))))

(ert-deftest slack-test-scheduled-messages-show-keeps-request-team ()
  (let* ((team1 (slack-create-team '(:id "T1" :name "One" :token "xoxb-one")))
         (team2 (slack-create-team '(:id "T2" :name "Two" :token "xoxb-two")))
         (slack-current-team team1)
         captured-buffer)
    (cl-letf (((symbol-function 'slack-list-scheduled-messages-request)
               (lambda (_team after-success)
                 (setq slack-current-team team2)
                 (funcall after-success
                          :data
                          '(:drafts ((:id "D1"
                                     :date_scheduled 1710000000
                                     :last_updated_ts "1710000000.001"
                                     :blocks ((:elements ((:elements ((:text "hello"))))))
                                     :destinations ((:channel_id "C11111"))))))))
              ((symbol-function 'slack-buffer-display)
               (lambda (buffer)
                 (setq captured-buffer buffer))))
      (slack-scheduled-messages-show team1))
    (should (string= "T1" (oref captured-buffer team-id)))))

(ert-deftest slack-test-request-retry-respects-max-retries ()
  (let ((req (slack-request-create "https://example.com"
                                   nil
                                   :type "GET"
                                   :success #'ignore)))
    (oset req retry-count slack-request-max-retry)
    (should-not (slack-request-retry-failed-request-p req '(end-of-file) 'error))))

(defun slack-test-emoji-master (&rest names)
  (let ((h (make-hash-table :test 'equal :size 16)))
    (dolist (name names)
      (puthash (format ":%s:" name) "x" h))
    h))

(defun slack-test-section-elements (str)
  (let* ((blocks (slack-test-parse-blocks str))
         (section (car (plist-get (car blocks) :elements))))
    (plist-get section :elements)))

(ert-deftest slack-test-blocks-emoji-in-bold-no-duplication ()
  (let* ((slack-emoji-master (slack-test-emoji-master "smile"))
         (elements (slack-test-section-elements "*bold :smile: end*")))
    (should (equal '(("text" . "bold ") ("emoji" . "smile") ("text" . " end"))
                   (mapcar (lambda (el)
                             (cons (plist-get el :type)
                                   (or (plist-get el :text)
                                       (plist-get el :name))))
                           elements)))
    (should (eq t (plist-get (plist-get (nth 0 elements) :style) :bold)))
    (should (eq t (plist-get (plist-get (nth 2 elements) :style) :bold)))))

(ert-deftest slack-test-blocks-colon-times-are-not-emoji ()
  (let* ((slack-emoji-master (slack-test-emoji-master "smile"))
         (elements (slack-test-section-elements "meeting at 12:30:45")))
    (should (eq 1 (length elements)))
    (should (string= "meeting at 12:30:45"
                     (plist-get (car elements) :text)))))

(ert-deftest slack-test-blocks-plus-one-is-emoji ()
  (let* ((slack-emoji-master (slack-test-emoji-master "+1"))
         (elements (slack-test-section-elements "nice :+1:")))
    (should (equal '(("text" . "nice ") ("emoji" . "+1"))
                   (mapcar (lambda (el)
                             (cons (plist-get el :type)
                                   (or (plist-get el :text)
                                       (plist-get el :name))))
                           elements)))))

(ert-deftest slack-test-blocks-mention-in-bold-is-user-element ()
  (let* ((slack-emoji-master (slack-test-emoji-master))
         (elements (slack-test-section-elements "*hi <@U11111> bye*")))
    (should (equal '(("text" . "hi ") ("user" . "U11111") ("text" . " bye"))
                   (mapcar (lambda (el)
                             (cons (plist-get el :type)
                                   (or (plist-get el :text)
                                       (plist-get el :user_id))))
                           elements)))
    (should (eq t (plist-get (plist-get (nth 0 elements) :style) :bold)))
    (should (eq t (plist-get (plist-get (nth 2 elements) :style) :bold)))))

(ert-deftest slack-test-blocks-link-in-bold-no-duplication ()
  (let* ((slack-emoji-master (slack-test-emoji-master))
         (elements (slack-test-section-elements
                    "*see https://example.com now*")))
    (should (equal '(("text" . "see ")
                     ("link" . "https://example.com")
                     ("text" . " now"))
                   (mapcar (lambda (el)
                             (cons (plist-get el :type)
                                   (or (plist-get el :text)
                                       (plist-get el :url))))
                           elements)))))

(ert-deftest slack-test-blocks-code-in-bold-no-duplication ()
  (let* ((slack-emoji-master (slack-test-emoji-master))
         (elements (slack-test-section-elements "*a `b` c*")))
    (should (equal '("a " "b" " c")
                   (mapcar (lambda (el) (plist-get el :text)) elements)))
    (should (eq t (plist-get (plist-get (nth 1 elements) :style) :code)))))

(ert-deftest slack-test-sync-request-invokes-success-handler ()
  (slack-test-setup
    (let ((captured nil)
          (success-data nil))
      (cl-letf (((symbol-function 'request)
                 (lambda (url &rest args)
                   (setq captured (cons url args))
                   nil)))
        (slack-request
         (slack-request-create
          "https://slack.com/api/test" team
          :sync t
          :without-auth t
          :success (cl-function
                    (lambda (&key data &allow-other-keys)
                      (setq success-data data))))))
      (let ((success-fn (plist-get (cdr captured) :success)))
        (should (functionp success-fn))
        (funcall success-fn :data '(:ok t))
        (should (equal '(:ok t) success-data))))))

(ert-deftest slack-test-reconnect-url-cleared-after-use ()
  (slack-test-setup
    (let ((ws (make-instance 'slack-team-ws))
          (opened-url nil))
      (oset team ws ws)
      (oset ws reconnect-url "wss://stale.example/reconnect")
      (cl-letf (((symbol-function 'slack-team-find)
                 (lambda (_id) team))
                ((symbol-function 'slack-ws-open)
                 (cl-function
                  (lambda (_ws _team &key ws-url &allow-other-keys)
                    (setq opened-url ws-url))))
                ((symbol-function 'slack-log) #'ignore))
        (slack-ws-reconnect-with-reconnect-url "T111"))
      (should (equal "wss://stale.example/reconnect" opened-url))
      (should (equal "" (oref ws reconnect-url))))))

(ert-deftest slack-test-user-typing-cancels-previous-timer ()
  (slack-test-setup
    (let* ((channel2 (make-instance 'slack-channel
                                    :id "C22222"
                                    :name "OtherChannel"))
           (old-timer nil))
      (puthash (oref channel2 id) channel2 (oref team channels))
      (unwind-protect
          (cl-letf (((symbol-function 'slack-buffer-find)
                     (lambda (&rest _) t))
                    ((symbol-function 'slack-buffer-name)
                     (lambda (&rest _) " *slack-test-typing*"))
                    ((symbol-function 'slack-buffer-show-typing-p)
                     (lambda (&rest _) t))
                    ((symbol-function 'slack-user-name)
                     (lambda (&rest _) "TestUser")))
            (slack-ws-handle-user-typing
             (list :user user-id :channel channel-id) team)
            (setq old-timer (oref team typing-timer))
            (should (timerp old-timer))
            (slack-ws-handle-user-typing
             (list :user user-id :channel (oref channel2 id)) team)
            (should-not (memq old-timer timer-list))
            (should (timerp (oref team typing-timer))))
        (when (timerp (oref team typing-timer))
          (cancel-timer (oref team typing-timer)))
        (when (timerp old-timer)
          (cancel-timer old-timer))))))

(ert-deftest slack-test-ws-url-redacted ()
  (should (equal "wss://wss-primary.slack.com/?token=REDACTED&sync_desync=1"
                 (slack-ws--redact-url
                  "wss://wss-primary.slack.com/?token=xoxc-secret-123&sync_desync=1")))
  (should (equal "wss://example.com/socket"
                 (slack-ws--redact-url "wss://example.com/socket"))))

(ert-deftest slack-test-authorize-with-dangling-request-proceeds ()
  (slack-test-setup
    (let ((requested nil))
      (oset team authorize-request
            (slack-request-create "https://slack.com/api/rtm.connect" team
                                  :success #'ignore))
      (cl-letf (((symbol-function 'slack-request)
                 (lambda (&rest _) (setq requested t) nil))
                ((symbol-function 'slack-log) #'ignore))
        (slack-authorize team))
      (should requested))))

(ert-deftest slack-test-cookie-store-tolerates-nil-cookie ()
  (slack-test-setup
    (should-not (slack-team-cookie team))
    (slack-url-cookie-store team)))

(ert-deftest slack-test-disconnected-teams-uses-connection-state ()
  (let ((slack-teams-by-token (make-hash-table :test 'equal))
        (team (make-instance 'slack-team :self-id "U0" :token "tok")))
    (oset team ws (make-instance 'slack-team-ws))
    (puthash "tok" team slack-teams-by-token)
    (should (equal (list team) (slack-disconnected-teams)))
    (oset (oref team ws) connected t)
    (should-not (slack-disconnected-teams))))

(ert-deftest slack-test-ws-close-cancels-connect-timeout-timer ()
  (slack-test-setup
    (let ((ws (make-instance 'slack-team-ws)))
      (oset team ws ws)
      (cl-letf (((symbol-function 'slack-log) #'ignore))
        (slack-ws-set-connect-timeout-timer ws #'ignore)
        (let ((timer (oref ws connect-timeout-timer)))
          (should (timerp timer))
          (unwind-protect
              (progn
                (slack-ws--close ws team)
                (should-not (memq timer timer-list)))
            (when (timerp timer)
              (cancel-timer timer))))))))

(ert-deftest slack-test-ws-on-timeout-tolerates-missing-team ()
  (cl-letf (((symbol-function 'slack-team-find) (lambda (_id) nil)))
    (slack-ws-on-timeout "T-GONE")))

(ert-deftest slack-test-dnd-team-info-stores-string-keys ()
  (slack-test-setup
    (let ((im (make-instance 'slack-im :id "D11111" :user user-id))
          (captured-success nil))
      (cl-letf (((symbol-function 'slack-team-ims)
                 (lambda (_team) (list im)))
                ((symbol-function 'slack-room-open-p)
                 (lambda (_room) t))
                ((symbol-function 'slack-request)
                 (lambda (req &rest _) (setq captured-success (oref req success)))))
        (slack-dnd-status-team-info team))
      (should (functionp captured-success))
      (funcall captured-success
               :data (list :ok t
                           :users (list (intern (concat ":" user-id))
                                        (list :dnd_enabled t
                                              :next_dnd_start_ts 0
                                              :next_dnd_end_ts 9999999999))))
      (let ((statuses (oref team dnd-status)))
        (should (hash-table-p statuses))
        (should (gethash user-id statuses))))))

(ert-deftest slack-test-user-local-time-uses-tz-offset ()
  (let* ((before (format-time-string "%I:%M %p" nil 7200))
         (actual (slack-user-local-time (list :tz_offset 7200)))
         (after (format-time-string "%I:%M %p" nil 7200)))
    (should (member actual (list before after))))
  (let* ((before (format-time-string "%I:%M %p" nil -10800))
         (actual (slack-user-local-time (list :tz_offset -10800)))
         (after (format-time-string "%I:%M %p" nil -10800)))
    (should (member actual (list before after)))))

(ert-deftest slack-test-user-local-time-nil-without-offset ()
  (should-not (slack-user-local-time (list :name "bot")))
  (should-not (slack-user-local-time nil))
  (should-not (slack-user-timezone (list :name "bot"))))

(ert-deftest slack-test-conversations-replies-params-clean ()
  (slack-test-setup
    (let ((captured-params nil))
      (cl-letf (((symbol-function 'slack-request)
                 (lambda (req &rest _) (setq captured-params (oref req params)))))
        (slack-conversations-replies channel "1710000000.000000" team)
        (should-not (assoc "oldest" captured-params))
        (should-not (memq nil captured-params))
        (slack-conversations-replies channel "1710000000.000000" team
                                     :cursor "cur" :oldest "1709.1")
        (should (equal "cur" (cdr (assoc "cursor" captured-params))))
        (should (eq 1 (cl-count "oldest" captured-params
                                :key #'car-safe :test #'equal)))
        (slack-conversations-replies channel "1710000000.000000" team
                                     :oldest "1709.1")
        (should (equal "1709.1" (cdr (assoc "oldest" captured-params))))
        (should (eq 1 (cl-count "oldest" captured-params
                                :key #'car-safe :test #'equal)))))))

(ert-deftest slack-test-conversations-history-commits-primary-before-users ()
  (slack-test-setup
    (let (request hydration-success primary-args ready-args events)
      (cl-letf (((symbol-function 'slack-team-missing-user-ids)
                 (lambda (&rest _) '("U-MISSING")))
                ((symbol-function 'slack-user-info-request)
                 (lambda (_ids _team &rest args)
                   (push 'hydration-request events)
                   (setq hydration-success
                         (plist-get args :after-success))))
                ((symbol-function 'slack-request)
                 (lambda (req &rest _)
                   (setq request req))))
        (slack-conversations-history
         channel team
         :on-primary-page
         (lambda (messages cursor)
           (setq primary-args (list messages cursor))
           (push 'primary events))
         :after-success
         (lambda (messages cursor)
           (setq ready-args (list messages cursor))
           (push 'ready events)))
        (funcall
         (oref request success)
         :data '(:ok t
                 :messages ((:type "message" :ts "1.0"
                             :user "U-MISSING" :text "body"))
                 :response_metadata (:next_cursor "CURSOR")))
        (should (equal '(hydration-request primary) events))
        (should (functionp hydration-success))
        (should (equal "CURSOR" (cadr primary-args)))
        (funcall hydration-success)
        (should (equal '(ready hydration-request primary) events))
        (should (eq (car primary-args) (car ready-args)))
        (should (equal (cdr primary-args) (cdr ready-args)))))))

(ert-deftest slack-test-conversations-replies-commits-primary-before-users ()
  (slack-test-setup
    (let (request hydration-success primary-args ready-args events)
      (cl-letf (((symbol-function 'slack-team-missing-user-ids)
                 (lambda (&rest _) '("U-MISSING")))
                ((symbol-function 'slack-users-info-request)
                 (lambda (_ids _team &rest args)
                   (push 'hydration-request events)
                   (setq hydration-success
                         (plist-get args :after-success))))
                ((symbol-function 'slack-request)
                 (lambda (req &rest _)
                   (setq request req))))
        (slack-conversations-replies
         channel "1.0" team
         :on-primary-page
         (lambda (messages cursor has-more)
           (setq primary-args (list messages cursor has-more))
           (push 'primary events))
         :after-success
         (lambda (messages cursor has-more)
           (setq ready-args (list messages cursor has-more))
           (push 'ready events)))
        (funcall
         (oref request success)
         :data '(:ok t :has_more t
                 :messages ((:type "message" :ts "1.0"
                             :user "U-MISSING" :text "body"))
                 :response_metadata (:next_cursor "REPLIES-CURSOR")))
        (should (equal '(hydration-request primary) events))
        (should (functionp hydration-success))
        (should (equal "REPLIES-CURSOR" (cadr primary-args)))
        (should (eq t (nth 2 primary-args)))
        (funcall hydration-success)
        (should (equal '(ready hydration-request primary) events))
        (should (eq (car primary-args) (car ready-args)))
        (should (equal (cdr primary-args) (cdr ready-args)))))))

(ert-deftest slack-test-conversations-failures-skip-success ()
  (slack-test-setup
    (let ((starters
           (list
            (lambda (primary ready error)
              (slack-conversations-history
               channel team
               :on-primary-page primary
               :after-success ready
               :on-error error))
            (lambda (primary ready error)
              (slack-conversations-replies
               channel "1.0" team
               :on-primary-page primary
               :after-success ready
               :on-error error)))))
      (dolist (start starters)
        (dolist (failure '(api transport))
          (let (request reported-errors primary-called ready-called
                       (expected-error
                        (if (eq failure 'api)
                            '("invalid_auth")
                          '(:error-thrown (error "timeout")
                            :symbol-status timeout :response nil :data nil))))
            (cl-letf (((symbol-function 'slack-request)
                       (lambda (req &rest _)
                         (setq request req))))
              (funcall
               start
               (lambda (&rest _) (setq primary-called t))
               (lambda (&rest _) (setq ready-called t))
               (lambda (&rest args) (push args reported-errors)))
              (if (eq failure 'api)
                  (funcall (oref request success)
                           :data '(:ok :json-false :error "invalid_auth"))
                (funcall (oref request error)
                         :error-thrown '(error "timeout")
                         :symbol-status 'timeout
                         :response nil
                         :data nil))
              (should (equal (list expected-error) reported-errors))
              (should-not primary-called)
              (should-not ready-called))))))))

(ert-deftest slack-test-set-replies-merges-partial-fetches ()
  (slack-test-setup
    (let* ((parent (make-instance 'slack-message :type "message"
                                  :channel channel-id
                                  :ts "1710000000.000100"))
           (r1 (make-instance 'slack-message :type "message"
                              :channel channel-id
                              :ts "1710000000.000101"
                              :thread_ts "1710000000.000100"))
           (r2 (make-instance 'slack-message :type "message"
                              :channel channel-id
                              :ts "1710000000.000102"
                              :thread_ts "1710000000.000100")))
      (slack-room-set-messages channel (list parent r1 r2) team)
      (slack-message-set-replies channel "1710000000.000100" (list r1))
      (slack-message-set-replies channel "1710000000.000100" (list r2))
      (should (equal '("1710000000.000101" "1710000000.000102")
                     (sort (copy-sequence (oref parent replies))
                           #'string<))))))

(ert-deftest slack-test-set-replies-tolerates-uncached-parent ()
  (slack-test-setup
    (slack-message-set-replies channel "1710000000.000999" nil)))

(ert-deftest slack-test-get-or-fetch-anchors-reply-fetch-at-ts ()
  (slack-test-setup
    (let ((captured-args nil))
      (cl-letf (((symbol-function 'slack-conversations-replies)
                 (cl-function
                  (lambda (_room _ts _team &rest args &key &allow-other-keys)
                    (setq captured-args args)
                    nil))))
        (slack-message-get-or-fetch "1710000000.000200" channel-id team
                                    "1710000000.000100"))
      (should (equal "1710000000.000200"
                     (plist-get captured-args :oldest))))))

(ert-deftest slack-test-get-or-fetch-tolerates-unknown-room ()
  (slack-test-setup
    (should-not (slack-message-get-or-fetch "1710000000.000200"
                                            "C-UNKNOWN" team))))

(defclass slack-test--plain-buffer (slack-buffer) ())

(ert-deftest slack-test-load-missing-messages-skips-killed-buffer ()
  (slack-test-setup
    (let ((buf-obj (make-instance 'slack-message-buffer
                                  :room-id channel-id
                                  :team-id (oref team id)))
          (buffer (generate-new-buffer " *slack-test-missing*")))
      (slack-buffer-cache-team buf-obj team)
      (oset buf-obj buf buffer)
      (kill-buffer buffer)
      (let ((buffers-after-kill (buffer-list)))
        (cl-letf (((symbol-function 'slack-conversations-history)
                   (cl-function
                    (lambda (_room _team &key after-success &allow-other-keys)
                      (funcall after-success nil nil)))))
          (slack-buffer-load-missing-messages buf-obj))
        (should (equal buffers-after-kill (buffer-list)))))))

(ert-deftest slack-test-load-more-resets-flag-on-error ()
  (slack-test-setup
    (let* ((buf-obj (make-instance 'slack-test--plain-buffer
                                   :team-id (oref team id)))
           (buffer (generate-new-buffer " *slack-test-load-more*"))
           (captured-error nil))
      (oset buf-obj buf buffer)
      (unwind-protect
          (cl-letf (((symbol-function 'slack-buffer-has-next-page-p)
                     (lambda (_this) t))
                    ((symbol-function 'slack-buffer-request-history)
                     (lambda (_this _success &optional on-error)
                       (setq captured-error on-error))))
            (with-current-buffer buffer
              (slack-buffer-load-more buf-obj)
              (should slack-buffer--loading-more-p))
            (should (functionp captured-error))
            (funcall captured-error "rate_limited")
            (with-current-buffer buffer
              (should-not slack-buffer--loading-more-p)))
        (kill-buffer buffer)))))

(ert-deftest slack-test-all-threads-history-restores-point-in-feed-buffer ()
  (slack-test-setup
    (let ((buf-obj (make-instance 'slack-all-threads-buffer
                                  :team-id (oref team id)
                                  :current-ts "1710000000.000000"
                                  :threads nil))
          (feed-buffer (generate-new-buffer " *slack-test-threads*"))
          (other-buffer (generate-new-buffer " *slack-test-other*"))
          (captured-success nil))
      (slack-buffer-cache-team buf-obj team)
      (oset buf-obj buf feed-buffer)
      (unwind-protect
          (progn
            (with-current-buffer feed-buffer
              (insert "0123456789")
              (goto-char 5))
            (with-current-buffer other-buffer
              (insert "abcdefghij")
              (goto-char 9))
            (cl-letf (((symbol-function 'slack-subscriptions-thread-get-view)
                       (lambda (_team _ts success)
                         (setq captured-success success))))
              (with-current-buffer feed-buffer
                (slack-buffer-request-history buf-obj #'ignore)))
            (with-current-buffer feed-buffer
              (goto-char (point-max)))
            (with-current-buffer other-buffer
              (funcall captured-success 0 0 nil nil)
              (should (eq 9 (point))))
            (with-current-buffer feed-buffer
              (should (eq 5 (point)))))
        (kill-buffer feed-buffer)
        (kill-buffer other-buffer)))))

(ert-deftest slack-test-marker-overlay-uses-own-buffer ()
  (slack-test-setup
    (let* ((buf-obj (make-instance 'slack-message-buffer
                                   :room-id channel-id
                                   :team-id (oref team id)))
           (own (generate-new-buffer " *slack-test-own*"))
           (decoy nil))
      (slack-buffer-cache-team buf-obj team)
      (oset buf-obj buf own)
      (oset channel last-read "100")
      (unwind-protect
          (progn
            (setq decoy (generate-new-buffer (slack-buffer-name buf-obj)))
            (dolist (b (list own decoy))
              (with-current-buffer b
                (insert (propertize "a\n" 'ts "100"))
                (insert (propertize "b\n" 'ts "200"))))
            (slack-buffer-update-marker-overlay buf-obj)
            (should (eq own (overlay-buffer (oref buf-obj marker-overlay)))))
        (kill-buffer own)
        (when decoy (kill-buffer decoy))))))

(ert-deftest slack-test-image-layout-block-without-size-metadata ()
  (let ((block (slack-create-image-layout-block
                (list :type "image"
                      :image_url "https://example.com/x.png"
                      :alt_text "alt"))))
    (should block)
    (should (stringp (slack-block-to-string block)))
    (should (string-prefix-p "alt" (slack-block-to-string block)))))

(ert-deftest slack-test-dialog-create-tolerates-json-nulls ()
  (let ((dialog (slack-dialog-create
                 (list :title "T"
                       :callback_id "cb"
                       :submit_label nil
                       :state nil
                       :elements (list (list :type "text"
                                             :name "n"
                                             :label "L"
                                             :max_length nil
                                             :subtype nil))))))
    (should dialog)
    (should (equal "Submit" (oref dialog submit-label)))
    (should (eq 150 (oref (car (oref dialog elements)) max-length)))))

(ert-deftest slack-test-dialog-option-groups-create-option-objects ()
  (let ((element (slack-dialog-select-element-create
                  (list :name "s" :label "Sel" :type "select"
                        :data_source "static"
                        :option_groups
                        (list (list :label "G"
                                    :options (list (list :label "A"
                                                         :value "a"))))))))
    (let* ((group (car (oref element option-groups)))
           (option (car (oref group options))))
      (should (cl-typep option 'slack-dialog-select-option))
      (should (equal "A" (slack-selectable-text option))))))

(ert-deftest slack-test-dialog-edit-element-init-returns-buffer ()
  (slack-test-setup
    (oset team name "TestTeam")
    (let* ((dialog (make-instance 'slack-dialog
                                  :title "T" :callback_id "cb" :elements nil))
           (dialog-buffer (make-instance 'slack-dialog-buffer
                                         :team-id (oref team id)
                                         :dialog-id "D1" :dialog dialog))
           (element (make-instance 'slack-dialog-text-element
                                   :name "n" :label "L" :type "text"))
           (buf-obj (make-instance 'slack-dialog-edit-element-buffer
                                   :team-id (oref team id)
                                   :dialog-buffer dialog-buffer
                                   :element element)))
      (slack-buffer-cache-team buf-obj team)
      (let ((buf (slack-buffer-init-buffer buf-obj)))
        (unwind-protect
            (should (bufferp buf))
          (when (buffer-live-p (oref buf-obj buf))
            (kill-buffer (oref buf-obj buf))))))))

(ert-deftest slack-test-file-update-passes-file-id ()
  (slack-test-setup
    (let* ((file (slack-file-create (list :id "F11111")))
           (buf-obj (make-instance 'slack-file-info-buffer
                                   :team-id (oref team id)
                                   :file file))
           (captured-id nil))
      (slack-buffer-cache-team buf-obj team)
      (cl-letf (((symbol-function 'slack-file-request-info)
                 (lambda (file-id _page _team &optional _after-success)
                   (setq captured-id file-id))))
        (let ((slack-current-buffer buf-obj))
          (slack-file-update)))
      (should (equal "F11111" captured-id)))))

(ert-deftest slack-test-scheduled-messages-buffer-is-findable ()
  (slack-test-setup
    (should (equal "scheduled-messages"
                   (slack-buffer-key 'slack-scheduled-messages-buffer)))
    (let ((buf-obj (make-instance 'slack-scheduled-messages-buffer
                                  :team-id (oref team id)
                                  :messages nil)))
      (slack-buffer-cache-team buf-obj team)
      (slack-team-set-buffer buf-obj)
      (should (eq buf-obj
                  (slack-buffer-find 'slack-scheduled-messages-buffer team))))))

(ert-deftest slack-test-message-replied-does-not-recount-mentions ()
  (slack-test-setup
    (let ((event (make-instance 'slack-message-replied-event
                                :payload nil))
          (parent (make-instance 'slack-message
                                 :type "message"
                                 :channel channel-id
                                 :ts "1710000000.000100"
                                 :text "parent <@U38383838>"))
          (mention-count-set nil))
      (cl-letf (((symbol-function 'slack-room-set-mention-count)
                 (lambda (&rest _) (setq mention-count-set t)))
                ((symbol-function 'slack-message-mentioned-p)
                 (lambda (&rest _) t))
                ((symbol-function 'slack-message-visible-p)
                 (lambda (&rest _) t))
                ((symbol-function 'slack-update-modeline) #'ignore))
        (slack-message-event-update-modeline event parent team))
      (should-not mention-count-set))))

(ert-deftest slack-test-scheduled-messages-tolerate-unscheduled-drafts ()
  (slack-test-setup
    (let ((displayed nil))
      (cl-letf (((symbol-function 'slack-list-scheduled-messages-request)
                 (lambda (_team cb)
                   (funcall cb :data
                            (list :ok t
                                  :drafts (list (list :id "D1"
                                                      :last_updated_ts "1")
                                                (list :id "D2"
                                                      :date_scheduled 9999999999
                                                      :last_updated_ts "2"))))))
                ((symbol-function 'slack-buffer-display)
                 (lambda (buf) (setq displayed buf)))
                ((symbol-function 'slack-scheduled-messages--team)
                 (lambda (&optional _t) team)))
        (slack-scheduled-messages-show team))
      (should displayed)
      (should (eq 1 (length (oref displayed messages)))))))

(ert-deftest slack-test-scheduled-messages-surface-api-errors ()
  (slack-test-setup
    (let ((displayed nil))
      (cl-letf (((symbol-function 'slack-list-scheduled-messages-request)
                 (lambda (_team cb)
                   (funcall cb :data (list :ok :json-false
                                           :error "invalid_auth"))))
                ((symbol-function 'slack-buffer-display)
                 (lambda (buf) (setq displayed buf)))
                ((symbol-function 'slack-scheduled-messages--team)
                 (lambda (&optional _t) team)))
        (slack-scheduled-messages-show team))
      (should-not displayed))))

(ert-deftest slack-test-file-without-timestamp-reads-nil ()
  (let ((file (slack-file-create (list :id "F11111"))))
    (should-not (oref file timestamp))))

(ert-deftest slack-test-channel-rename-without-normalized-name ()
  (slack-test-setup
    (let ((event (make-instance 'slack-room-rename-event
                                :payload (list :channel
                                               (list :id channel-id
                                                     :name "renamed")))))
      (oset channel name-normalized "TestChannel")
      (slack-event-update-name event channel team)
      (should (equal "renamed" (oref channel name)))
      (should (equal "renamed" (oref channel name-normalized))))))

(ert-deftest slack-test-selectable-empty-group-input-returns-nil ()
  (let ((element (slack-dialog-select-element-create
                  (list :name "s" :label "Sel" :type "select"
                        :data_source "static"
                        :option_groups
                        (list (list :label "G"
                                    :options (list (list :label "A"
                                                         :value "a")))))))
        (slack-completing-read-function (lambda (&rest _) "")))
    (should-not (slack-selectable-select-from-static-data-source element))))

(ert-deftest slack-test-inline-action-label-with-backslash ()
  (with-temp-buffer
    (insert "<slack-action://B1/payload|run C:\\tool>")
    (slack-display-inline-action)
    (should (string= "run C:\\tool" (buffer-string)))))

(ert-deftest slack-test-thread-sync-inserts-gap-replies-in-order ()
  (slack-test-setup
    (let* ((parent (make-instance 'slack-message :type "message"
                                  :channel channel-id
                                  :ts "100.000100"))
           (r1 (make-instance 'slack-message :type "message"
                              :channel channel-id :ts "100.000200"
                              :thread_ts "100.000100"))
           (r2 (make-instance 'slack-message :type "message"
                              :channel channel-id :ts "100.000300"
                              :thread_ts "100.000100"))
           (r3 (make-instance 'slack-message :type "message"
                              :channel channel-id :ts "100.000400"
                              :thread_ts "100.000100"))
           (buf-obj (make-instance 'slack-thread-message-buffer
                                   :room-id channel-id
                                   :team-id (oref team id)
                                   :thread-ts "100.000100"
                                   :has-more nil))
           (buffer (generate-new-buffer " *slack-test-thread-sync*")))
      (slack-buffer-cache-team buf-obj team)
      (oset buf-obj buf buffer)
      (slack-room-set-messages channel (list parent r1 r2 r3) team)
      (slack-message-set-replies channel "100.000100" (list r1 r2 r3))
      (unwind-protect
          (progn
            (with-current-buffer buffer
              (setq-local lui-output-marker (point-max-marker))
              (insert (propertize "100.000200\n" 'ts "100.000200"))
              (insert (propertize "100.000400\n" 'ts "100.000400"))
              (set-marker lui-output-marker (point-max)))
            (cl-letf (((symbol-function 'slack-buffer-insert)
                       (lambda (_buf m &optional _nt)
                         (save-excursion
                           (goto-char lui-output-marker)
                           (insert-before-markers
                            (propertize (concat (slack-ts m) "\n")
                                        'ts (slack-ts m))))))
                      ((symbol-function 'lui-recover-output-marker)
                       #'ignore)
                      ((symbol-function 'slack-buffer-update-mark)
                       #'ignore)
                      ((symbol-function 'slack-thread-mark)
                       #'ignore))
              (slack-thread--sync-buffer buf-obj parent channel))
            (with-current-buffer buffer
              (should (equal "100.000200\n100.000300\n100.000400\n"
                             (buffer-substring-no-properties
                              (point-min) (point-max))))))
        (kill-buffer buffer)))))

(ert-deftest slack-test-message-buffer-load-more-needs-cursor ()
  (slack-test-setup
    (let ((buf-obj (make-instance 'slack-message-buffer
                                  :room-id channel-id
                                  :team-id (oref team id)
                                  :cursor ""))
          (requested nil))
      (slack-buffer-cache-team buf-obj team)
      (cl-letf (((symbol-function 'slack-conversations-history)
                 (lambda (&rest _) (setq requested t))))
        (with-temp-buffer
          (slack-buffer-load-more buf-obj)))
      (should-not requested))))

(ert-deftest slack-test-rate-limit-safe-list-calls-back-once ()
  (slack-test-setup
    (let ((calls 0)
          (received nil))
      (cl-letf (((symbol-function 'slack-conversations-list)
                 (lambda (_team callback types)
                   (funcall callback
                            (when (equal types (list "public_channel"))
                              (list 'public))
                            (when (equal types (list "private_channel"))
                              (list 'private))
                            (when (equal types (list "im"))
                              (list 'im)))))
                ((symbol-function 'slack-log) #'ignore))
        (slack-conversations-list--safe-for-rate-limiting
         team
         (lambda (channels groups ims)
           (cl-incf calls)
           (setq received (list channels groups ims)))))
      (should (eq 1 calls))
      (should (equal (list (list 'public) (list 'private) (list 'im))
                     received)))))

(ert-deftest slack-test-feed-render-stops-when-buffer-killed ()
  (slack-test-setup
    (let ((buf-obj (make-instance 'slack-activity-feed-buffer
                                  :team-id (oref team id)
                                  :room-id "__activity-feed__"
                                  :activity-feed (make-instance
                                                  'slack-activity-feed
                                                  :activities nil
                                                  :pagination nil
                                                  :last nil)))
          (buffer (generate-new-buffer " *slack-test-feed-render*"))
          (slack-activity-feed-render-batch-size 1)
          (inserted 0))
      (slack-buffer-cache-team buf-obj team)
      (oset buf-obj buf buffer)
      (with-current-buffer buffer
        (setq-local lui-output-marker (point-min-marker)))
      (cl-letf (((symbol-function 'slack-buffer-insert)
                 (lambda (&rest _) (cl-incf inserted))))
        (slack-activity-feed--render-activities buf-obj (list 'a 'b 'c))
        (should (eq 1 inserted))
        (kill-buffer buffer)
        (let ((timer (oref buf-obj render-timer)))
          (should (timerp timer))
          (timer-event-handler timer))
        (should (eq 1 inserted))
        (should-not (oref buf-obj render-timer))
        (should-not (buffer-live-p buffer))))))

(ert-deftest slack-test-event-cache-refresh-is-debounced ()
  (slack-test-setup
    (let ((slack-activity-feed--cache (make-hash-table :test 'equal))
          (slack-activity-feed--event-refresh-time 0)
          (slack-activity-refresh-debounce 30)
          (refreshes 0))
      (puthash (list (oref team id) nil)
               (list :activities nil :pagination nil)
               slack-activity-feed--cache)
      (cl-letf (((symbol-function 'slack-activity-feed--refresh-cache)
                 (lambda (_team &optional _after _quiet) (cl-incf refreshes))))
        (slack-activity-feed-refresh-cache-from-event team)
        (slack-activity-feed-refresh-cache-from-event team)
        (slack-activity-feed-refresh-cache-from-event team))
      (should (eq 1 refreshes)))))

(ert-deftest slack-test-unread-count-fetch-reports-zero-on-error ()
  (slack-test-setup
    (let ((reported 'unset))
      (cl-letf (((symbol-function 'slack-activity-feed-request)
                 (lambda (_team _success &optional _cursor on-error)
                   (funcall on-error "request failed"))))
        (slack-activity-feed--fetch-unread-count
         team (lambda (count) (setq reported count))))
      (should (eq 0 reported)))))

(ert-deftest slack-test-stars-load-more-resets-flag-on-error ()
  (slack-test-setup
    (let ((buf-obj (make-instance 'slack-stars-buffer
                                  :team-id (oref team id)))
          (buffer (generate-new-buffer " *slack-test-stars-lm*"))
          (captured-error nil))
      (slack-buffer-cache-team buf-obj team)
      (oset buf-obj buf buffer)
      (oset team star (slack-create-star
                       (list :saved_items nil
                             :response_metadata (list :next_cursor "cur"))))
      (unwind-protect
          (progn
            (cl-letf (((symbol-function 'slack-stars-list-request)
                       (lambda (_team _cursor _success &optional on-error)
                         (setq captured-error on-error))))
              (with-current-buffer buffer
                (slack-buffer-load-more buf-obj)
                (should slack-buffer--loading-more-p)))
            (should (functionp captured-error))
            (funcall captured-error "rate_limited")
            (with-current-buffer buffer
              (should-not slack-buffer--loading-more-p)))
        (kill-buffer buffer)))))

(ert-deftest slack-test-users-info-error-keeps-continuation ()
  (slack-test-setup
    (let ((captured-error nil)
          (continued nil))
      (cl-letf (((symbol-function 'slack-request)
                 (lambda (req &rest _) (setq captured-error (oref req error))))
                ((symbol-function 'slack-log) #'ignore))
        (slack-users-info-request (list "U99999") team
                                  :after-success (lambda () (setq continued t)))
        (should (functionp captured-error))
        (funcall captured-error
                 :error-thrown '(error "timeout")
                 :symbol-status 'timeout
                 :response nil :data nil))
      (should continued))))

(ert-deftest slack-test-counts-tolerate-missing-fields ()
  (let ((counts (slack-create-counts
                 (list :threads nil
                       :channels (list (list :id "C1" :has_unreads t))
                       :mpims nil
                       :ims nil))))
    (should counts)
    (let ((summary (slack-counts-summary counts)))
      (should (eq 0 (cdr (cdr (assoc 'channel summary))))))))

(ert-deftest slack-test-worker-queue-keeps-distinct-requests ()
  (slack-test-setup
    (let ((worker (make-instance 'slack-request-worker))
          (req1 (slack-request-create "https://slack.com/api/x" team
                                      :type "GET" :success #'ignore))
          (req2 (slack-request-create "https://slack.com/api/x" team
                                      :type "GET" :success #'ignore)))
      (slack-request-worker-push worker req1)
      (slack-request-worker-push worker req2)
      (should (eq 2 (length (oref worker queue))))
      (slack-request-worker-push worker req1)
      (should (eq 2 (length (oref worker queue)))))))

(ert-deftest slack-test-rate-limit-retry-honors-no-retry ()
  (slack-test-setup
    (let ((req (slack-request-create "https://slack.com/api/x" team
                                     :sync t
                                     :without-auth t
                                     :no-retry t
                                     :success #'ignore
                                     :error (cl-function
                                             (lambda (&key &allow-other-keys) nil))))
          (requeued nil)
          (captured-error nil))
      (cl-letf (((symbol-function 'request)
                 (lambda (_url &rest args) (setq captured-error (plist-get args :error)) nil))
                ((symbol-function 'slack-request-worker-push)
                 (lambda (_req) (setq requeued t)))
                ((symbol-function 'request-response-header)
                 (lambda (_resp _header) "3")))
        (slack-request req)
        (funcall captured-error
                 :error-thrown '(error http 429)
                 :symbol-status 'error
                 :response nil
                 :data nil))
      (should-not requeued))))

(ert-deftest slack-test-share-omits-blocks-for-blank-comment ()
  (slack-test-setup
    (let ((captured-params nil)
          (slack-completing-read-function (lambda (&rest _) "chan")))
      (cl-letf (((symbol-function 'slack-request)
                 (lambda (req &rest _) (setq captured-params (oref req params))))
                ((symbol-function 'slack-message-room-list)
                 (lambda (_team) (list (cons "chan" channel)))))
        (slack-message-share--send team channel "1710000000.000100" "")
        (should captured-params)
        (should-not (assoc "blocks" captured-params))
        (slack-message-share--send team channel "1710000000.000100" "hi")
        (should (assoc "blocks" captured-params))))))

(ert-deftest slack-test-reaction-remove-updates-locally ()
  (slack-test-setup
    (let* ((ts "1710000000.000100")
           (m (make-instance 'slack-message
                             :type "message"
                             :channel channel-id
                             :ts ts
                             :reactions (list (slack-reaction
                                               :name "smile"
                                               :count 1
                                               :users (list "U38383838")))))
           (captured-success nil)
           (replaced nil))
      (slack-room-set-messages channel (list m) team)
      (cl-letf (((symbol-function 'slack-request)
                 (lambda (req &rest _) (setq captured-success (oref req success))))
                ((symbol-function 'slack-message-replace-buffer)
                 (lambda (&rest _) (setq replaced t))))
        (slack-message-reaction-remove "smile" ts channel team)
        (funcall captured-success :data '(:ok t)))
      (should replaced)
      (should-not (oref m reactions)))))

(ert-deftest slack-test-send-internal-allows-attachment-only ()
  (slack-test-setup
    (let ((uploaded nil))
      (oset channel is-member t)
      (cl-letf (((symbol-function 'slack-message-upload-files)
                 (cl-function
                  (lambda (_team files &key &allow-other-keys)
                    (setq uploaded files)))))
        (slack-message-send-internal "" channel team
                                     :files (list "report.pdf")))
      (should (equal (list "report.pdf") uploaded))
      (should-error (slack-message-send-internal "" channel team)
                    :type 'user-error))))

(ert-deftest slack-test-paginate-after-anchors-at-page-newest ()
  (let ((page (list (make-instance 'slack-message :type "message"
                                   :channel "C11111"
                                   :ts "1710000000.000300")
                    (make-instance 'slack-message :type "message"
                                   :channel "C11111"
                                   :ts "1710000000.000100")
                    (make-instance 'slack-message :type "message"
                                   :channel "C11111"
                                   :ts "1710000000.000200"))))
    (should (equal "1710000000.000300"
                   (slack-messages-paginate--anchor-ts
                    'after page "1710000000.000050")))
    (should (equal "1710000000.000100"
                   (slack-messages-paginate--anchor-ts
                    'before page "1710000000.000400")))
    (should (equal "1710000000.000050"
                   (slack-messages-paginate--anchor-ts
                    'after nil "1710000000.000050")))))

(ert-deftest slack-test-thread-replies-publishes-primary-before-hydrated ()
  (slack-test-setup
    (let* ((thread-ts "1710000000.000100")
           (reply-ts "1710000000.000200")
           (parent (make-instance 'slack-message
                                  :type "message"
                                  :channel channel-id
                                  :ts thread-ts
                                  :thread_ts thread-ts))
           (reply (make-instance 'slack-message
                                 :type "message"
                                 :channel channel-id
                                 :ts reply-ts
                                 :thread_ts thread-ts))
           primary
           hydrated
           events)
      (slack-room-set-messages channel (list parent) team)
      (cl-letf (((symbol-function 'slack-conversations-replies)
                 (lambda (_room _ts _team &rest args)
                   (setq primary (plist-get args :on-primary-page)
                         hydrated (plist-get args :after-success)))))
        (slack-thread-replies
         parent channel team
         :on-primary-page
         (lambda (&rest _) (push 'primary events))
         :after-success
         (lambda (&rest _) (push 'hydrated events))))
      (should (functionp primary))
      (funcall primary (list parent reply) "cursor-1" t)
      (should (equal '(primary) events))
      (should (eq reply (slack-room-find-message channel reply-ts)))
      (should (equal (list reply-ts) (oref parent replies)))
      (funcall hydrated (list parent reply) "cursor-1" t)
      (should (equal '(hydrated primary) events)))))

(ert-deftest slack-test-thread-show-displays-and-caches-before-hydration ()
  (slack-test-setup
    (let* ((thread-ts "1710000000.000100")
           (reply-ts "1710000000.000200")
           (parent (make-instance 'slack-message
                                  :type "message"
                                  :channel channel-id
                                  :ts thread-ts
                                  :thread_ts thread-ts
                                  :text "parent"
                                  :reactions nil))
           (reply (make-instance 'slack-message
                                 :type "message"
                                 :channel channel-id
                                 :ts reply-ts
                                 :thread_ts thread-ts
                                 :text "reply"
                                 :reactions nil))
           (live-reply (make-instance 'slack-message
                                      :type "message"
                                      :channel channel-id
                                      :ts "1710000000.000300"
                                      :thread_ts thread-ts
                                      :text "live reply"
                                      :reactions nil))
           primary
           hydrated
           events
           object
           buffer)
      (slack-room-set-messages channel (list parent) team)
      (unwind-protect
          (cl-letf (((symbol-function 'slack-buffer-display)
                     (lambda (shown)
                       (setq object shown
                             buffer (oref shown buf))
                       (push 'display events)))
                    ((symbol-function 'slack-thread-replies)
                     (lambda (_message _room _team &rest args)
                       (setq primary (plist-get args :on-primary-page)
                             hydrated (plist-get args :after-success))
                       (push 'request events))))
            (slack-thread-show-messages
             parent channel team (lambda () (push 'ready events)))
            (should (equal '(request display) events))
            ;; A WebSocket reply beyond the HTTP page must remain visible but
            ;; must not become the anchor for the next older-replies request.
            (slack-room-set-messages channel (list live-reply) team)
            (slack-message-set-replies channel thread-ts (list live-reply))
            ;; `slack-thread-replies' invokes its primary callback only after
            ;; storing both the room messages and the thread reply relation.
            (slack-room-set-messages channel (list parent reply) team)
            (slack-message-set-replies channel thread-ts (list parent reply))
            (funcall primary (list parent reply) "cursor-1" t)
            (should (slack-page-state-loaded-p
                     (slack-thread-page-state channel team thread-ts)))
            (should (eq 'loading
                        (slack-page-state-status
                         (slack-thread-page-state channel team thread-ts))))
            (should (equal reply-ts (oref object last-read)))
            (should (with-current-buffer buffer
                      (save-excursion
                        (goto-char (point-min))
                        (search-forward "reply" nil t))))
            (should-not (memq 'ready events))
            (funcall hydrated "cursor-1" t)
            (should (eq 'ready
                        (slack-page-state-status
                         (slack-thread-page-state channel team thread-ts))))
            (should (eq 'ready (car events))))
        (when (buffer-live-p buffer) (kill-buffer buffer))))))

(ert-deftest slack-test-thread-show-coalesces-two-opens ()
  (slack-test-setup
    (let* ((thread-ts "1710000000.000100")
           (parent (make-instance 'slack-message
                                  :type "message"
                                  :channel channel-id
                                  :ts thread-ts
                                  :thread_ts thread-ts
                                  :reactions nil))
           (requests 0)
           object)
      (slack-room-set-messages channel (list parent) team)
      (unwind-protect
          (cl-letf (((symbol-function 'slack-buffer-display)
                     (lambda (shown) (setq object shown)))
                    ((symbol-function 'slack-thread-replies)
                     (lambda (&rest _) (cl-incf requests))))
            (slack-thread-show-messages parent channel team)
            (slack-thread-show-messages parent channel team)
            (should (= 1 requests)))
        (when (and object (slot-boundp object 'buf)
                   (buffer-live-p (oref object buf)))
          (kill-buffer (oref object buf)))))))

(ert-deftest slack-test-thread-show-coalescing-preserves-both-callbacks ()
  (slack-test-setup
    (let* ((thread-ts "1710000000.000100")
           (parent (make-instance 'slack-message
                                  :type "message"
                                  :channel channel-id
                                  :ts thread-ts
                                  :thread_ts thread-ts
                                  :reactions nil))
           primary
           hydrated
           callbacks
           object)
      (slack-room-set-messages channel (list parent) team)
      (unwind-protect
          (cl-letf (((symbol-function 'slack-buffer-display)
                     (lambda (shown) (setq object shown)))
                    ((symbol-function 'slack-thread-replies)
                     (lambda (_message _room _team &rest args)
                       (setq primary (plist-get args :on-primary-page)
                             hydrated (plist-get args :after-success)))))
            (slack-thread-show-messages
             parent channel team (lambda () (push 'first callbacks)))
            (slack-thread-show-messages
             parent channel team (lambda () (push 'second callbacks)))
            (funcall primary (list parent) "" nil)
            (funcall hydrated "" nil)
            (should (equal '(first second) callbacks)))
        (when (and object (slot-boundp object 'buf)
                   (buffer-live-p (oref object buf)))
          (kill-buffer (oref object buf)))))))

(ert-deftest slack-test-thread-primary-does-not-resurrect-killed-buffer ()
  (slack-test-setup
    (let* ((thread-ts "1710000000.000100")
           (parent (make-instance 'slack-message
                                  :type "message"
                                  :channel channel-id
                                  :ts thread-ts
                                  :thread_ts thread-ts
                                  :reactions nil))
           primary
           hydrated
           object
           killed)
      (slack-room-set-messages channel (list parent) team)
      (cl-letf (((symbol-function 'slack-buffer-display)
                 (lambda (shown)
                   (setq object shown
                         killed (oref shown buf))))
                ((symbol-function 'slack-thread-replies)
                 (lambda (_message _room _team &rest args)
                   (setq primary (plist-get args :on-primary-page)
                         hydrated (plist-get args :after-success)))))
        (slack-thread-show-messages parent channel team)
        (kill-buffer killed)
        (funcall primary (list parent) "" nil)
        (funcall hydrated "" nil)
        (should-not (buffer-live-p killed))
        (should (eq killed (oref object buf)))))))

(ert-deftest slack-test-thread-reopen-renders-page-without-running-old-callback ()
  (slack-test-setup
    (let* ((thread-ts "1710000000.000100")
           (reply-ts "1710000000.000200")
           (parent (make-instance 'slack-message
                                  :type "message"
                                  :channel channel-id
                                  :ts thread-ts
                                  :thread_ts thread-ts
                                  :text "parent"
                                  :reactions nil))
           (reply (make-instance 'slack-message
                                 :type "message"
                                 :channel channel-id
                                 :ts reply-ts
                                 :thread_ts thread-ts
                                 :text "new reply"
                                 :reactions nil))
           primary
           hydrated
           first-object
           first-buffer
           second-object
           second-buffer
           callbacks)
      (slack-room-set-messages channel (list parent) team)
      (unwind-protect
          (cl-letf (((symbol-function 'slack-buffer-display)
                     (lambda (shown)
                       (if first-object
                           (setq second-object shown
                                 second-buffer (oref shown buf))
                         (setq first-object shown
                               first-buffer (oref shown buf)))))
                    ((symbol-function 'slack-thread-replies)
                     (lambda (_message _room _team &rest args)
                       (setq primary (plist-get args :on-primary-page)
                             hydrated (plist-get args :after-success)))))
            (slack-thread-show-messages
             parent channel team (lambda () (push 'old callbacks)))
            (kill-buffer first-buffer)
            (slack-thread-show-messages
             parent channel team (lambda () (push 'new callbacks)))
            (should (not (eq first-object second-object)))
            (slack-room-set-messages channel (list parent reply) team)
            (slack-message-set-replies channel thread-ts (list parent reply))
            (funcall primary (list parent reply) "" nil)
            (funcall hydrated "" nil)
            (should (equal '(new) callbacks))
            (should (with-current-buffer second-buffer
                      (save-excursion
                        (goto-char (point-min))
                        (search-forward "new reply" nil t)))))
        (when (buffer-live-p second-buffer) (kill-buffer second-buffer))))))

(ert-deftest slack-test-thread-primary-preserves-concurrent-message-edit ()
  (slack-test-setup
    (let* ((thread-ts "1710000000.000100")
           (reply-ts "1710000000.000200")
           (parent (make-instance 'slack-message
                                  :type "message"
                                  :channel channel-id
                                  :ts thread-ts
                                  :thread_ts thread-ts
                                  :reactions nil))
           (cached (make-instance 'slack-message
                                  :type "message"
                                  :channel channel-id
                                  :ts reply-ts
                                  :thread_ts thread-ts
                                  :text "cached"))
           (concurrent (make-instance 'slack-message
                                      :type "message"
                                      :channel channel-id
                                      :ts reply-ts
                                      :thread_ts thread-ts
                                      :text "edited"))
           (http (make-instance 'slack-message
                                :type "message"
                                :channel channel-id
                                :ts reply-ts
                                :thread_ts thread-ts
                                :text "stale"))
           primary)
      (slack-room-set-messages channel (list parent cached) team)
      (cl-letf (((symbol-function 'slack-conversations-replies)
                 (lambda (_room _ts _team &rest args)
                   (setq primary (plist-get args :on-primary-page)))))
        (slack-thread-replies parent channel team)
        (slack-room-set-messages channel (list concurrent) team)
        (funcall primary (list parent http) "" nil))
      (should (eq concurrent
                  (slack-room-find-message channel reply-ts))))))

(ert-deftest slack-test-thread-failure-renders-and-retries ()
  (slack-test-setup
    (let* ((thread-ts "1710000000.000100")
           (parent (make-instance 'slack-message
                                  :type "message"
                                  :channel channel-id
                                  :ts thread-ts
                                  :thread_ts thread-ts
                                  :reactions nil))
           (requests 0)
           fail
           object
           buffer)
      (slack-room-set-messages channel (list parent) team)
      (unwind-protect
          (cl-letf (((symbol-function 'slack-buffer-display)
                     (lambda (shown)
                       (setq object shown
                             buffer (oref shown buf))))
                    ((symbol-function 'slack-thread-replies)
                     (lambda (_message _room _team &rest args)
                       (cl-incf requests)
                       (setq fail (plist-get args :on-error)))))
            (slack-thread-show-messages parent channel team)
            (funcall fail "offline")
            (should (eq 'failed
                        (slack-page-state-status
                         (slack-thread-page-state channel team thread-ts))))
            (should (with-current-buffer buffer
                      (save-excursion
                        (goto-char (point-min))
                        (search-forward "Slack request failed" nil t))))
            (with-current-buffer buffer (slack-buffer-page-retry))
            (should (= 2 requests)))
        (when (buffer-live-p buffer) (kill-buffer buffer))))))

(ert-deftest slack-test-open-channel-displays-then-fetches-range-sequentially ()
  (slack-test-setup
    (let ((target-ts "1710000000.000500")
          ready
          before-success
          after-success
          navigated-ts
          events)
      (cl-letf (((symbol-function 'slack-room-display)
                 (lambda (_room _team &optional callback)
                   (push 'display events)
                   (setq ready callback)))
                ((symbol-function 'slack-messages-before)
                 (lambda (_ts _room _team &optional callback _error)
                   (push 'before events)
                   (setq before-success callback)))
                ((symbol-function 'slack-messages-after)
                 (lambda (_ts _room _team &optional callback _error)
                   (push 'after events)
                   (setq after-success callback)))
                ((symbol-function 'slack-buffer-goto)
                 (lambda (ts) (setq navigated-ts ts) t)))
        (slack-open-message team channel target-ts nil)
        (should (equal '(display) events))
        (should-not navigated-ts)
        (funcall ready)
        (should (equal '(before display) events))
        (funcall before-success)
        (should (equal '(after before display) events))
        (should-not navigated-ts)
        (funcall after-success)
        (should (equal target-ts navigated-ts))
        (should (= 1 (cl-count 'display events)))))))

(ert-deftest slack-test-open-channel-range-error-reaches-fallback ()
  (slack-test-setup
    (let ((target-ts "1710000000.000500")
          ready
          range-error
          (fallbacks 0))
      (cl-letf (((symbol-function 'slack-room-display)
                 (lambda (_room _team &optional callback)
                   (setq ready callback)))
                ((symbol-function 'slack-messages-before)
                 (lambda (_ts _room _team &optional _callback error)
                   (setq range-error error)))
                ((symbol-function 'slack-buffer-goto) (lambda (_ts) nil))
                ((symbol-function 'slack-open-message--browser-fallback)
                 (lambda (&rest _) (cl-incf fallbacks))))
        (slack-open-message team channel target-ts nil nil t)
        (funcall ready)
        (should (functionp range-error))
        (funcall range-error "offline")
        (should (= 1 fallbacks))))))

(ert-deftest slack-test-open-channel-cached-target-navigates-after-ready ()
  (slack-test-setup
    (let* ((target-ts "1710000000.000500")
           (target (make-instance 'slack-message
                                  :type "message"
                                  :channel channel-id
                                  :ts target-ts
                                  :reactions nil))
           ready
           navigated-ts
           (ranges 0))
      (slack-room-set-messages channel (list target) team)
      (cl-letf (((symbol-function 'slack-room-display)
                 (lambda (_room _team &optional callback)
                   (setq ready callback)))
                ((symbol-function 'slack-messages-before)
                 (lambda (&rest _) (cl-incf ranges)))
                ((symbol-function 'slack-messages-after)
                 (lambda (&rest _) (cl-incf ranges)))
                ((symbol-function 'slack-buffer-goto)
                 (lambda (ts) (setq navigated-ts ts) t)))
        (slack-open-message team channel target-ts nil)
        (should-not navigated-ts)
        (funcall ready)
        (should (equal target-ts navigated-ts))
        (should (= 0 ranges))))))

(ert-deftest slack-test-open-thread-displays-identified-destination-before-fetch ()
  (slack-test-setup
    (let ((thread-ts "1710000000.000100")
          (reply-ts "1710000000.000200")
          ready
          navigated-ts
          events)
      (cl-letf (((symbol-function 'slack-thread-show-messages)
                 (lambda (parent _room _team &optional callback _error)
                   (should (equal thread-ts (slack-ts parent)))
                   (push 'display events)
                   (setq ready callback)))
                ((symbol-function 'slack-conversations-replies)
                 (lambda (&rest _) (push 'request events)))
                ((symbol-function 'slack-messages-before)
                 (lambda (&rest _) (ert-fail "thread link fetched channel range")))
                ((symbol-function 'slack-messages-after)
                 (lambda (&rest _) (ert-fail "thread link fetched channel range")))
                ((symbol-function 'slack-buffer-goto)
                 (lambda (ts)
                   (when (equal ts thread-ts)
                     (setq navigated-ts ts)
                     t))))
        (slack-open-message team channel reply-ts thread-ts reply-ts)
        (should (equal '(display) events))
        (should-not navigated-ts)
        (funcall ready)
        (should (equal thread-ts navigated-ts))))))

(ert-deftest slack-test-open-thread-ready-miss-uses-browser-without-range ()
  (slack-test-setup
    (let* ((thread-ts "1710000000.000100")
           (reply-ts "1710000000.000200")
           (parent (make-instance 'slack-message
                                  :type "message"
                                  :channel channel-id
                                  :ts thread-ts
                                  :thread_ts thread-ts))
           ready
           (fallbacks 0)
           (ranges 0))
      (slack-room-set-messages channel (list parent) team)
      (cl-letf (((symbol-function 'slack-thread-show-messages)
                 (lambda (_parent _room _team &optional callback _error)
                   (setq ready callback)))
                ((symbol-function 'slack-messages-before)
                 (lambda (&rest _) (cl-incf ranges)))
                ((symbol-function 'slack-messages-after)
                 (lambda (&rest _) (cl-incf ranges)))
                ((symbol-function 'slack-buffer-goto) (lambda (_ts) nil))
                ((symbol-function 'slack-open-message--browser-fallback)
                 (lambda (&rest _) (cl-incf fallbacks))))
        (slack-open-message team channel reply-ts thread-ts reply-ts t)
        (should (= 0 fallbacks))
        (funcall ready)
        (should (= 1 fallbacks))
        (should (= 0 ranges))))))

(ert-deftest slack-test-open-thread-error-uses-browser-fallback ()
  (slack-test-setup
    (let ((thread-ts "1710000000.000100")
          (reply-ts "1710000000.000200")
          fail
          (fallbacks 0))
      (cl-letf (((symbol-function 'slack-thread-show-messages)
                 (lambda (_parent _room _team &optional _ready error)
                   (setq fail error)))
                ((symbol-function 'slack-buffer-goto) (lambda (_ts) nil))
                ((symbol-function 'slack-open-message--browser-fallback)
                 (lambda (&rest _) (cl-incf fallbacks))))
        (slack-open-message team channel reply-ts thread-ts reply-ts t)
        (should (functionp fail))
        (funcall fail 'failed "offline")
        (should (= 1 fallbacks))))))

(ert-deftest slack-test-messages-paginate-keeps-replacement-buffer-untouched ()
  (slack-test-setup
    (let* ((object (slack-create-message-buffer channel "" team))
           (old-buffer (slack-buffer-buffer object))
           success
           replacement
           replacement-buffer
           updated-object)
      (cl-letf (((symbol-function 'slack-conversations-history)
                 (lambda (_room _team &rest args)
                   (setq success (plist-get args :after-success))))
                ((symbol-function 'slack-buffer-insert-messages)
                 (lambda (target &rest _) (setq updated-object target))))
        (with-current-buffer old-buffer
          (slack-messages-paginate 'before "1710000000.000500" channel team))
        (kill-buffer old-buffer)
        (setq replacement (slack-create-message-buffer channel "" team)
              replacement-buffer (slack-buffer-buffer replacement)
              updated-object nil)
        (funcall success
                 (list (make-instance 'slack-message
                                      :type "message"
                                      :channel channel-id
                                      :ts "1710000000.000400"))
                 "")
        (should-not updated-object)
        (should (buffer-live-p replacement-buffer)))
      (when (buffer-live-p replacement-buffer)
        (kill-buffer replacement-buffer)))))

(ert-deftest slack-test-messages-paginate-propagates-error ()
  (slack-test-setup
    (let (request-error received)
      (cl-letf (((symbol-function 'slack-conversations-history)
                 (lambda (_room _team &rest args)
                   (setq request-error (plist-get args :on-error)))))
        (slack-messages-paginate
         'before "1710000000.000500" channel team nil
         (lambda (&rest errors) (setq received errors))))
      (should (functionp request-error))
      (funcall request-error "offline")
      (should (equal '("offline") received)))))

(ert-deftest slack-test-messages-paginate-propagates-synchronous-error ()
  (slack-test-setup
    (let (received)
      (cl-letf (((symbol-function 'slack-conversations-history)
                 (lambda (&rest _) (error "offline"))))
        (slack-messages-paginate
         'before "1710000000.000500" channel team nil
         (lambda (&rest errors) (setq received errors))))
      (should (equal '((error "offline")) received)))))

(ert-deftest slack-test-messages-paginate-after-does-not-filter-newer-page ()
  (slack-test-setup
    (let* ((object (slack-create-message-buffer channel "" team))
           (buffer (slack-buffer-buffer object))
           success
           filter-by-oldest)
      (unwind-protect
          (cl-letf (((symbol-function 'slack-conversations-history)
                     (lambda (_room _team &rest args)
                       (setq success (plist-get args :after-success))))
                    ((symbol-function 'slack-buffer-insert-messages)
                     (lambda (_object _messages filter &optional _not-tracked)
                       (setq filter-by-oldest filter))))
            (with-current-buffer buffer
              (slack-messages-paginate
               'after "1710000000.000500" channel team))
            (funcall success
                     (list (make-instance 'slack-message
                                          :type "message"
                                          :channel channel-id
                                          :ts "1710000000.000600"))
                     "")
            (should-not filter-by-oldest))
        (when (buffer-live-p buffer) (kill-buffer buffer))))))

(ert-deftest slack-test-open-channel-serializes-before-and-after ()
  (slack-test-setup
    (let ((order nil)
          (slack-test-out-load-older-messages-p t))
      (cl-letf (((symbol-function 'slack-messages-before)
                 (lambda (_ts _room _team &optional cb _on-error)
                   (push 'before-requested order)
                   (funcall cb)))
                ((symbol-function 'slack-messages-after)
                 (lambda (_ts _room _team &optional _cb _on-error)
                   (push 'after-requested order)))
                ((symbol-function 'slack-room-display)
                 (lambda (_room _team &optional callback)
                   (funcall callback))))
        (slack-open-message--open-channel "1710000000.000100"
                                          channel team #'ignore #'ignore))
      (should (equal '(before-requested after-requested)
                     (nreverse order))))))

(ert-deftest slack-test-thread-update-keeps-last-read-while-has-more ()
  (slack-test-setup
    (let ((buf-obj (make-instance 'slack-thread-message-buffer
                                  :room-id channel-id
                                  :team-id (oref team id)
                                  :thread-ts "1710000000.000100"
                                  :has-more t))
          (buffer (generate-new-buffer " *slack-test-thread*"))
          (reply (make-instance 'slack-message
                                :type "message"
                                :channel channel-id
                                :ts "1710000000.000900"
                                :thread_ts "1710000000.000100"))
          (marked nil))
      (slack-buffer-cache-team buf-obj team)
      (oset buf-obj buf buffer)
      (oset buf-obj last-read "1710000000.000200")
      (unwind-protect
          (cl-letf (((symbol-function 'slack-buffer-insert)
                     (lambda (&rest _) nil))
                    ((symbol-function 'slack-buffer-message-exists-p)
                     (lambda (&rest _) nil))
                    ((symbol-function 'slack-thread-mark)
                     (lambda (&rest _) (setq marked t))))
            (slack-buffer-update buf-obj reply)
            (should (equal "1710000000.000200" (oref buf-obj last-read)))
            (should-not marked)
            (oset buf-obj has-more nil)
            (slack-buffer-update buf-obj
                                 (make-instance 'slack-message
                                                :type "message"
                                                :channel channel-id
                                                :ts "1710000000.000901"
                                                :thread_ts "1710000000.000100"))
            (should (equal "1710000000.000901" (oref buf-obj last-read))))
        (kill-buffer buffer)))))

(ert-deftest slack-test-unread-count-fetch-uses-unread-cache-key ()
  (slack-test-setup
    (let ((slack-activity-feed--cache (make-hash-table :test 'equal))
          (slack-activity-feed-mode-show-only-unread nil)
          (captured-success nil))
      (puthash (list (oref team id) nil)
               (list :activities 'all-mode-snapshot :pagination nil)
               slack-activity-feed--cache)
      (cl-letf (((symbol-function 'slack-activity-feed-request)
                 (lambda (_team &optional after-success _cursor _on-error)
                   (setq captured-success after-success)))
                ((symbol-function 'slack-activity-feed--with-watched-activities)
                 (lambda (activities _team cb) (funcall cb activities)))
                ((symbol-function 'slack-activity-feed--cache-merge-activities)
                 #'ignore))
        (slack-activity-feed--fetch-unread-count team #'ignore)
        ;; The callback fires after the let-binding of the mode var
        ;; has unwound, i.e. with the global (all) mode in effect.
        (funcall captured-success (list :items nil :response_metadata nil)))
      (should (eq 'all-mode-snapshot
                  (plist-get (gethash (list (oref team id) nil)
                                      slack-activity-feed--cache)
                             :activities)))
      (should (gethash (list (oref team id) t)
                       slack-activity-feed--cache)))))

(ert-deftest slack-test-activity-mark-read-never-regresses-cursor ()
  (slack-test-setup
    (let ((marked nil))
      (oset channel last-read "1710000000.000200")
      (cl-letf (((symbol-function 'slack-conversations-mark)
                 (lambda (_room _team ts &optional _cb) (setq marked ts)))
                ((symbol-function 'slack-activity-feed--on-marked-read)
                 #'ignore))
        (with-temp-buffer
          (insert (propertize "old entry"
                              'ts "1710000000.000100"
                              'room-id channel-id))
          (goto-char (point-min))
          (slack-activity-feed--mark-read team))
        (should-not marked)
        (with-temp-buffer
          (insert (propertize "new entry"
                              'ts "1710000000.000300"
                              'room-id channel-id))
          (goto-char (point-min))
          (slack-activity-feed--mark-read team))
        (should (equal "1710000000.000300" marked))))))

(ert-deftest slack-test-custom-notification-predicates-lifecycle ()
  (slack-test-setup
    (let ((m (make-instance 'slack-message
                            :type "message"
                            :channel channel-id
                            :ts "1710000000.000100"
                            :text "hello")))
      (cl-letf (((symbol-function 'slack-message-minep)
                 (lambda (&rest _) nil)))
        (let ((slack-custom-notification-predicates
               (list (lambda (&rest _) 'slack-notify-keep))))
          (should (slack-message-notify-p m channel team))
          (should (eq 1 (length slack-custom-notification-predicates)))
          (should (slack-message-notify-p m channel team)))
        (let ((slack-custom-notification-predicates
               (list (lambda (&rest _) t))))
          (should (slack-message-notify-p m channel team))
          (should (eq 0 (length slack-custom-notification-predicates)))))
      (cl-letf (((symbol-function 'slack-message-minep)
                 (lambda (&rest _) t)))
        (let ((slack-custom-notification-predicates
               (list (lambda (&rest _) t))))
          (should-not (slack-message-notify-p m channel team))
          (should (eq 1 (length slack-custom-notification-predicates))))))))

(ert-deftest slack-test-reaction-fetch-rejects-ts-mismatch ()
  (slack-test-setup
    (let ((req (slack-request-create "https://example.com" team
                                     :success #'ignore)))
      (oset req response
            (make-request-response
             :data (list :messages
                         (list (list :type "message"
                                     :user "U11111"
                                     :channel "C11111"
                                     :ts "1710000000.000050"
                                     :text "older channel message")))))
      (cl-letf (((symbol-function 'slack-conversations-history)
                 (cl-function (lambda (&rest _args &key &allow-other-keys) req))))
        (should-not (slack-reaction-event--fetch-and-cache-message
                     channel "1710000000.000200" team))
        (should-not (slack-room-find-message channel "1710000000.000050"))))))

(ert-deftest slack-test-notify-alert-tolerates-empty-message ()
  (slack-test-setup
    (let ((m (make-instance 'slack-message
                            :type "message"
                            :channel channel-id
                            :ts "1710000000.000100"
                            :text ""))
          (alerted nil))
      (oset team name "TestTeam")
      (cl-letf (((symbol-function 'slack-message-notify-p)
                 (lambda (&rest _) t))
                ((symbol-function 'alert)
                 (lambda (&rest _) (setq alerted t))))
        (slack-message-notify-alert m channel team))
      (should alerted))))

(ert-deftest slack-test-pins-list-skips-unknown-item-types ()
  (slack-test-setup
    (let ((captured-success nil)
          (received-items 'unset))
      (cl-letf (((symbol-function 'slack-request)
                 (lambda (req &rest _) (setq captured-success (oref req success)))))
        (slack-pins-list channel team
                         (lambda (items) (setq received-items items))))
      (funcall captured-success
               :data (list :ok t
                           :items (list
                                   (list :type "file_comment"
                                         :comment (list :comment "legacy"))
                                   (list :type "message"
                                         :message (list :type "message"
                                                        :user "U11111"
                                                        :ts "1710000000.000100"
                                                        :text "pinned")))))
      (should (eq 1 (length received-items)))
      (should (cl-typep (car received-items) 'slack-pinned-item)))))

(ert-deftest slack-test-star-empty-cursor-means-no-next-page ()
  (let ((star (slack-create-star
               (list :saved_items nil
                     :response_metadata (list :next_cursor "")))))
    (should-not (slack-star-has-next-page-p star)))
  (let ((star (slack-create-star
               (list :saved_items nil
                     :response_metadata (list :next_cursor "cur")))))
    (should (slack-star-has-next-page-p star))))

(ert-deftest slack-test-saved-items-starts-uninitialized-team-before-request ()
  (let ((team (make-instance 'slack-team
                             :name "Uninitialized"
                             :token "token"))
        (slack-team--conversations-loaded (make-hash-table :test 'equal))
        (started nil)
        (buffer-lookup nil)
        (requested nil))
    (cl-letf (((symbol-function 'slack-team-select)
               (lambda (&optional _) team))
              ((symbol-function 'slack-start)
               (lambda (&optional selected-team)
                 (setq started selected-team)))
              ((symbol-function 'slack-buffer-find)
               (lambda (&rest _)
                 (setq buffer-lookup t)))
              ((symbol-function 'slack-stars-list-request)
               (lambda (&rest _) (setq requested t))))
      (should-error (slack-saved-items) :type 'user-error)
      (should (eq team started))
      (should-not buffer-lookup)
      (should-not requested))))

(ert-deftest slack-test-feed-goto-prev-lands-on-entry-starts ()
  (with-temp-buffer
    (insert (propertize "entry one\n" 'ts "1"))
    (insert "\n")
    (insert (propertize "entry two\n" 'ts "2"))
    (insert "\n")
    (insert (propertize "entry three\n" 'ts "3"))
    (goto-char (1- (point-max)))
    (slack-feed-goto-prev)
    (should (equal "2" (get-text-property (point) 'ts)))
    (slack-feed-goto-prev)
    (should (equal "1" (get-text-property (point) 'ts)))
    (should (eq (point) (point-min)))
    (slack-feed-goto-prev)
    (should (eq (point) (point-min)))))

(ert-deftest slack-test-feed-goto-prev-from-gap ()
  (with-temp-buffer
    (insert (propertize "entry one\n" 'ts "1"))
    (insert "\n")
    (insert (propertize "entry two\n" 'ts "2"))
    (goto-char (+ 10 (point-min)))
    (should-not (get-text-property (point) 'ts))
    (slack-feed-goto-prev)
    (should (equal "1" (get-text-property (point) 'ts)))
    (should (eq (point) (point-min)))))

(ert-deftest slack-test-reaction-add-works-for-uncached-message ()
  (slack-test-setup
    (let ((captured-params nil))
      (cl-letf (((symbol-function 'slack-request)
                 (lambda (req &rest _) (setq captured-params (oref req params)))))
        (slack-message-reaction-add "smile" "1710000000.000000" channel team))
      (should captured-params)
      (should (equal "1710000000.000000"
                     (cdr (assoc "timestamp" captured-params))))
      (should (equal "smile" (cdr (assoc "name" captured-params)))))))

(ert-deftest slack-test-reaction-remove-works-for-uncached-message ()
  (slack-test-setup
    (let ((captured-params nil))
      (cl-letf (((symbol-function 'slack-request)
                 (lambda (req &rest _) (setq captured-params (oref req params)))))
        (slack-message-reaction-remove "smile" "1710000000.000000" channel team))
      (should captured-params)
      (should (equal "1710000000.000000"
                     (cdr (assoc "timestamp" captured-params)))))))

(ert-deftest slack-test-upload-files-failure-stops-timer ()
  (slack-test-setup
    (cl-letf (((symbol-function 'slack-upload-file)
               (lambda (_file _team cb) (funcall cb nil)))
              ((symbol-function 'slack-log) #'ignore))
      (let ((before (copy-sequence timer-list))
            (errors 0))
        (slack-message-upload-files team (list "f.txt")
                                    :on-error (lambda (&rest _)
                                                (cl-incf errors)))
        (let ((new-timers (cl-set-difference timer-list before)))
          (should (eq 1 (length new-timers)))
          (unwind-protect
              (progn
                (timer-event-handler (car new-timers))
                (should (eq 1 errors))
                (should-not (memq (car new-timers) timer-list)))
            (cancel-timer (car new-timers))))))))

(ert-deftest slack-test-upload-files-failure-without-on-error-stops-timer ()
  (slack-test-setup
    (cl-letf (((symbol-function 'slack-upload-file)
               (lambda (_file _team cb) (funcall cb nil)))
              ((symbol-function 'slack-log) #'ignore))
      (let ((before (copy-sequence timer-list)))
        (slack-message-upload-files team (list "f.txt"))
        (let ((new-timers (cl-set-difference timer-list before)))
          (should (eq 1 (length new-timers)))
          (unwind-protect
              (progn
                (timer-event-handler (car new-timers))
                (should-not (memq (car new-timers) timer-list)))
            (cancel-timer (car new-timers))))))))

(ert-deftest slack-test-curl-downloader-failure-keeps-existing-file ()
  (let* ((dir (make-temp-file "slack-test-dl" t))
         (target (expand-file-name "existing.bin" dir))
         (errored nil))
    (unwind-protect
        (progn
          (with-temp-file target (insert "precious"))
          (slack-curl-downloader "http://127.0.0.1:1/nonexistent" target nil
                                 :error (lambda (&rest _) (setq errored t)))
          (with-timeout (10 (error "curl did not finish"))
            (while (not errored) (sleep-for 0.05)))
          (should (file-exists-p target))
          (should (equal "precious"
                         (with-temp-buffer
                           (insert-file-contents target)
                           (buffer-string)))))
      (delete-directory dir t))))

(ert-deftest slack-test-curl-downloader-renames-temp-on-success ()
  (let* ((dir (make-temp-file "slack-test-dl" t))
         (source (expand-file-name "source.bin" dir))
         (target (expand-file-name "downloaded.bin" dir))
         (done nil))
    (unwind-protect
        (progn
          (with-temp-file source (insert "payload"))
          (slack-curl-downloader (concat "file://" source) target nil
                                 :success (lambda () (setq done t)))
          (with-timeout (10 (error "curl did not finish"))
            (while (not done) (sleep-for 0.05)))
          (should (equal "payload"
                         (with-temp-buffer
                           (insert-file-contents target)
                           (buffer-string))))
          (should (equal (list "downloaded.bin" "source.bin")
                         (sort (directory-files dir nil "^[^.]") #'string<))))
      (delete-directory dir t))))

(ert-deftest slack-test-file-download-confirms-overwrite ()
  (slack-test-setup
    (let* ((dir (make-temp-file "slack-test-dl" t))
           (target (expand-file-name "report.pdf" dir))
           (file (slack-file-create
                  (list :id "F11111"
                        :url_private_download "https://example.com/report.pdf")))
           (download-started nil))
      (unwind-protect
          (progn
            (with-temp-file target (insert "precious"))
            (let ((slack-file-download-confirm t))
              (cl-letf (((symbol-function 'read-file-name)
                         (lambda (&rest _) target))
                        ((symbol-function 'yes-or-no-p)
                         (lambda (&rest _) nil))
                        ((symbol-function 'slack-url-copy-file)
                         (cl-function
                          (lambda (&rest _args &key &allow-other-keys)
                            (setq download-started t)))))
                (should-error (slack-file-download file team)
                              :type 'user-error)
                (should-not download-started)
                (cl-letf (((symbol-function 'yes-or-no-p)
                           (lambda (&rest _) t)))
                  (slack-file-download file team)
                  (should download-started)))))
        (delete-directory dir t)))))

(ert-deftest slack-test-merge-and-equalp-on-strings ()
  (should (equal "abc" (slack-merge "abc" "xyz")))
  (should (slack-equalp "abc" "abc"))
  (should-not (slack-equalp "abc" "xyz")))

(ert-deftest slack-test-file-merge-with-channel-lists ()
  (let ((old-file (slack-file-create
                   (list :id "F11111" :channels (list "C11111"))))
        (new-file (slack-file-create
                   (list :id "F11111" :channels (list "C11111" "C22222")))))
    (slack-merge old-file new-file)
    (should (equal '("C11111" "C22222")
                   (sort (copy-sequence (oref old-file channels))
                         #'string<)))))

(ert-deftest slack-test-room-compose-send-kills-buffer-only-on-success ()
  (slack-test-setup
    (let* ((buf-obj (slack-create-room-message-compose-buffer channel team))
           (buffer (slack-buffer-buffer buf-obj))
           (captured-success nil)
           (captured-error nil))
      (unwind-protect
          (progn
            (cl-letf (((symbol-function 'slack-message-send-internal)
                       (cl-function
                        (lambda (_message _room _team
                                          &key on-success on-error
                                          &allow-other-keys)
                          (setq captured-success on-success
                                captured-error on-error)))))
              (slack-buffer-send-message buf-obj "hello"))
            (should (buffer-live-p buffer))
            (should (functionp captured-error))
            (funcall captured-error "is_archived")
            (should (buffer-live-p buffer))
            (should (functionp captured-success))
            (funcall captured-success)
            (should-not (buffer-live-p buffer)))
        (when (buffer-live-p buffer)
          (kill-buffer buffer))))))

(ert-deftest slack-test-thread-compose-send-kills-buffer-only-on-success ()
  (slack-test-setup
    (let* ((slack-thread-also-send-to-room nil)
           (buf-obj (slack-create-thread-message-compose-buffer
                     channel "1710000000.000000" team))
           (buffer (slack-buffer-buffer buf-obj))
           (captured-success nil)
           (captured-error nil))
      (unwind-protect
          (progn
            (cl-letf (((symbol-function 'slack-message-send-internal)
                       (cl-function
                        (lambda (_message _room _team
                                          &key on-success on-error
                                          &allow-other-keys)
                          (setq captured-success on-success
                                captured-error on-error)))))
              (slack-buffer-send-message buf-obj "hello thread"))
            (should (buffer-live-p buffer))
            (should (functionp captured-error))
            (funcall captured-error "is_archived")
            (should (buffer-live-p buffer))
            (should (functionp captured-success))
            (funcall captured-success)
            (should-not (buffer-live-p buffer)))
        (when (buffer-live-p buffer)
          (kill-buffer buffer))))))

(ert-deftest slack-test-edit-buffer-send-kills-buffer-only-on-success ()
  (slack-test-setup
    (let* ((ts "1710000000.000000")
           (m (make-instance 'slack-message
                             :type "message"
                             :channel channel-id
                             :ts ts
                             :text "original"))
           (buf-obj nil)
           (buffer nil)
           (captured-success nil)
           (captured-error nil))
      (slack-room-set-messages channel (list m) team)
      (setq buf-obj (slack-create-edit-message-buffer channel team ts))
      (setq buffer (slack-buffer-buffer buf-obj))
      (unwind-protect
          (progn
            (cl-letf (((symbol-function 'slack-message--edit)
                       (cl-function
                        (lambda (_channel _team _ts _text
                                          &key on-success on-error)
                          (setq captured-success on-success
                                captured-error on-error)))))
              (slack-buffer-send-message buf-obj "edited"))
            (should (buffer-live-p buffer))
            (should (functionp captured-error))
            (funcall captured-error "cant_update_message")
            (should (buffer-live-p buffer))
            (should (functionp captured-success))
            (funcall captured-success)
            (should-not (buffer-live-p buffer)))
        (when (buffer-live-p buffer)
          (kill-buffer buffer))))))

(ert-deftest slack-test-edit-buffer-send-works-when-message-uncached ()
  (slack-test-setup
    (let* ((ts "1710000000.000000")
           (m (make-instance 'slack-message
                             :type "message"
                             :channel channel-id
                             :ts ts
                             :text "original"))
           (buf-obj nil)
           (buffer nil)
           (edit-sent nil))
      (slack-room-set-messages channel (list m) team)
      (setq buf-obj (slack-create-edit-message-buffer channel team ts))
      (setq buffer (slack-buffer-buffer buf-obj))
      (unwind-protect
          (progn
            (slack-room-set-messages channel nil team)
            (cl-letf (((symbol-function 'slack-message--edit)
                       (cl-function
                        (lambda (&rest _args &key &allow-other-keys)
                          (setq edit-sent t)))))
              (slack-buffer-send-message buf-obj "edited"))
            (should edit-sent))
        (when (buffer-live-p buffer)
          (kill-buffer buffer))))))

(ert-deftest slack-test-share-buffer-send-kills-buffer-only-on-success ()
  (slack-test-setup
    (let* ((ts "1710000000.000000")
           (buf-obj (slack-create-message-share-buffer channel team ts))
           (buffer (slack-buffer-buffer buf-obj))
           (captured-success nil)
           (captured-error nil))
      (unwind-protect
          (progn
            (cl-letf (((symbol-function 'slack-message-share--send)
                       (cl-function
                        (lambda (_team _room _ts _msg
                                       &key on-success on-error)
                          (setq captured-success on-success
                                captured-error on-error)))))
              (slack-buffer-send-message buf-obj "share comment"))
            (should (buffer-live-p buffer))
            (should (functionp captured-error))
            (funcall captured-error "channel_not_found")
            (should (buffer-live-p buffer))
            (should (functionp captured-success))
            (funcall captured-success)
            (should-not (buffer-live-p buffer)))
        (when (buffer-live-p buffer)
          (kill-buffer buffer))))))

(ert-deftest slack-test-chat-post-message-network-error-calls-on-error ()
  (slack-test-setup
    (let ((captured-req nil)
          (reported-error nil))
      (cl-letf (((symbol-function 'slack-request)
                 (lambda (req &rest _args) (setq captured-req req))))
        (slack-chat-post-message team
                                 (list (cons "channel" channel-id))
                                 :on-error (lambda (err &rest _)
                                             (setq reported-error err))))
      (should (functionp (oref captured-req error)))
      (funcall (oref captured-req error)
               :error-thrown '(error "Connection refused")
               :symbol-status 'error
               :response nil
               :data nil)
      (should reported-error))))

;;; Room history lifecycle

(defun slack-test-room-message (ts text)
  "Return a room message at TS containing TEXT."
  (make-instance 'slack-user-message
                 :type "message"
                 :ts ts
                 :text text
                 :user "U11111"
                 :channel "C11111"
                 :reactions nil))

(defun slack-test-kill-room-buffer (room team)
  "Kill ROOM's message buffer for TEAM when it is live."
  (when-let* ((object (slack-buffer-find 'slack-message-buffer team room))
              (buffer (and (slot-boundp object 'buf) (oref object buf)))
              ((buffer-live-p buffer)))
    (kill-buffer buffer)))

(ert-deftest slack-test-room-display-displays-before-history-request ()
  "A cold room shell is displayed before history starts."
  (slack-test-setup
    (oset team mark-as-read-immediately nil)
    (let (events)
      (unwind-protect
          (cl-letf (((symbol-function 'slack-buffer-display)
                     (lambda (_object) (setq events (append events '(display)))))
                    ((symbol-function 'slack-conversations-history)
                     (lambda (&rest _args)
                       (setq events (append events '(request))))))
            (slack-room-display channel team)
            (should (equal '(display request) events)))
        (slack-test-kill-room-buffer channel team)))))

(ert-deftest slack-test-room-display-renders-retained-message-before-refresh ()
  "Reopening a killed room renders retained history before its refresh page."
  (slack-test-setup
    (oset team mark-as-read-immediately nil)
    (let ((message (slack-test-room-message "1.000" "retained text"))
          displayed-text
          primary)
      (slack-room-set-messages channel (list message) team)
      (let ((old-object (slack-create-message-buffer channel "" team)))
        (slack-buffer-buffer old-object)
        (kill-buffer (oref old-object buf)))
      (unwind-protect
          (cl-letf (((symbol-function 'slack-buffer-display)
                     (lambda (object)
                       (setq displayed-text
                             (with-current-buffer (oref object buf)
                               (buffer-substring-no-properties
                                (point-min) (point-max))))))
                    ((symbol-function 'slack-conversations-history)
                     (lambda (_room _team &rest args)
                       (setq primary (plist-get args :on-primary-page)))))
            (slack-room-display channel team)
            (should (string-match-p "retained text" displayed-text))
            (should (functionp primary)))
        (slack-test-kill-room-buffer channel team)))))

(ert-deftest slack-test-room-display-coalesces-duplicate-refresh ()
  "Two displays during one refresh issue one history request."
  (slack-test-setup
    (oset team mark-as-read-immediately nil)
    (let ((request-count 0))
      (unwind-protect
          (cl-letf (((symbol-function 'slack-buffer-display) #'ignore)
                    ((symbol-function 'slack-conversations-history)
                     (lambda (&rest _args) (cl-incf request-count))))
            (slack-room-display channel team)
            (slack-room-display channel team)
            (should (= 1 request-count)))
        (slack-test-kill-room-buffer channel team)))))

(ert-deftest slack-test-room-display-empty-page-is-loaded-and-keeps-cursor ()
  "An empty primary page is loaded data and retains its next cursor."
  (slack-test-setup
    (oset team mark-as-read-immediately nil)
    (let (primary hydrated)
      (unwind-protect
          (cl-letf (((symbol-function 'slack-buffer-display) #'ignore)
                    ((symbol-function 'slack-conversations-history)
                     (lambda (_room _team &rest args)
                       (setq primary (plist-get args :on-primary-page)
                             hydrated (plist-get args :after-success)))))
            (slack-room-display channel team)
            (funcall primary nil "cursor-1")
            (let ((state (oref channel history-state)))
              (should (slack-page-state-loaded-p state))
              (should (equal "cursor-1"
                             (slack-page-state-continuation state)))
              (should (slack-page-state-has-more state))
              (should (eq 'loading (slack-page-state-status state))))
            (funcall hydrated nil "cursor-1")
            (should (eq 'ready
                        (slack-page-state-status
                         (oref channel history-state)))))
        (slack-test-kill-room-buffer channel team)))))

(ert-deftest slack-test-room-display-late-page-does-not-recreate-killed-buffer ()
  "A history callback after buffer death leaves the exact buffer dead."
  (slack-test-setup
    (oset team mark-as-read-immediately nil)
    (let (primary hydrated killed-buffer)
      (cl-letf (((symbol-function 'slack-buffer-display) #'ignore)
                ((symbol-function 'slack-conversations-history)
                 (lambda (_room _team &rest args)
                   (setq primary (plist-get args :on-primary-page)
                         hydrated (plist-get args :after-success)))))
        (slack-room-display channel team)
        (setq killed-buffer
              (oref (slack-buffer-find 'slack-message-buffer team channel) buf))
        (kill-buffer killed-buffer)
        (funcall primary (list (slack-test-room-message "2.000" "late")) "")
        (funcall hydrated nil "")
        (should-not (buffer-live-p killed-buffer))
        (should-not (slack-buffer-find 'slack-message-buffer team channel))))))

(ert-deftest slack-test-room-display-stale-page-does-not-mutate-room ()
  "A superseded room generation ignores its late primary page."
  (slack-test-setup
    (oset team mark-as-read-immediately nil)
    (let (primary)
      (unwind-protect
          (cl-letf (((symbol-function 'slack-buffer-display) #'ignore)
                    ((symbol-function 'slack-conversations-history)
                     (lambda (_room _team &rest args)
                       (setq primary (plist-get args :on-primary-page)))))
            (slack-room-display channel team)
            (slack-page-state-restart (oref channel history-state))
            (funcall primary
                     (list (slack-test-room-message "9.000" "stale"))
                     "stale-cursor")
            (should-not (slack-room-find-message channel "9.000"))
            (should-not (slack-page-state-loaded-p
                         (oref channel history-state))))
        (slack-test-kill-room-buffer channel team)))))

(ert-deftest slack-test-room-history-merge-keeps-concurrent-new-message ()
  "A WebSocket message arriving during history survives its HTTP page."
  (slack-test-setup
    (let* ((http-message (slack-test-room-message "1.000" "http"))
           (websocket-message (slack-test-room-message "2.000" "websocket"))
           (snapshot (slack-room-history-start-snapshot channel)))
      (slack-room-push-message channel websocket-message team)
      (slack-room-merge-history-page channel (list http-message) team snapshot)
      (should (eq websocket-message
                  (slack-room-find-message channel "2.000"))))))

(ert-deftest slack-test-room-history-merge-keeps-concurrent-replacement ()
  "A newer WebSocket version wins over an older HTTP version of one message."
  (slack-test-setup
    (let* ((original (slack-test-room-message "1.000" "original"))
           (http-message (slack-test-room-message "1.000" "stale http"))
           (websocket-message (slack-test-room-message "1.000" "new websocket")))
      (slack-room-set-messages channel (list original) team)
      (slack-page-state-store (oref channel history-state) (list original) "" nil)
      (let ((snapshot (slack-room-history-start-snapshot channel)))
        (slack-room-push-message channel websocket-message team)
        (slack-room-merge-history-page channel (list http-message) team snapshot))
      (should (eq websocket-message
                  (slack-room-find-message channel "1.000"))))))

(ert-deftest slack-test-room-history-merge-honors-concurrent-delete-tombstone ()
  "A WebSocket deletion prevents an older HTTP page resurrecting the message."
  (slack-test-setup
    (let* ((original (slack-test-room-message "1.000" "original"))
           (http-message (slack-test-room-message "1.000" "stale http")))
      (slack-room-set-messages channel (list original) team)
      (slack-page-state-store (oref channel history-state) (list original) "" nil)
      (let ((snapshot (slack-room-history-start-snapshot channel)))
        (slack-room-delete-message channel "1.000")
        (slack-room-merge-history-page channel (list http-message) team snapshot))
      (should-not (slack-room-find-message channel "1.000"))
      (should (gethash "1.000" (oref channel message-revisions))))))

(ert-deftest slack-test-room-history-merge-removes-unchanged-stale-page-message ()
  "A message absent from a newer HTTP page is removed when locally unchanged."
  (slack-test-setup
    (let ((old (slack-test-room-message "1.000" "old")))
      (slack-room-set-messages channel (list old) team)
      (slack-page-state-store (oref channel history-state) (list old) "" nil)
      (let ((snapshot (slack-room-history-start-snapshot channel)))
        (slack-room-merge-history-page channel nil team snapshot))
      (should-not (slack-room-find-message channel "1.000")))))

(ert-deftest slack-test-room-clear-messages-retains-revision-tombstones ()
  "Clearing a room leaves a newer tombstone for every removed timestamp."
  (slack-test-setup
    (let ((first (slack-test-room-message "1.000" "first"))
          (second (slack-test-room-message "2.000" "second")))
      (slack-room-set-messages channel (list first second) team)
      (let ((first-revision
             (gethash "1.000" (oref channel message-revisions)))
            (second-revision
             (gethash "2.000" (oref channel message-revisions))))
        (slack-room-clear-messages channel)
        (should (= 0 (hash-table-count (oref channel messages))))
        (should (< first-revision
                   (gethash "1.000" (oref channel message-revisions))))
        (should (< second-revision
                   (gethash "2.000" (oref channel message-revisions))))))))

(ert-deftest slack-test-room-history-ready-follows-primary-and-hydration ()
  "The legacy room success callback runs after primary render and hydration."
  (slack-test-setup
    (oset team mark-as-read-immediately nil)
    (let (events primary hydrated page-recorded)
      (unwind-protect
          (cl-letf (((symbol-function 'slack-buffer-display)
                     (lambda (_object)
                       (setq events (append events '(display)))))
                    ((symbol-function 'slack-message-buffer-render-history-state)
                     (lambda (_object state)
                       (when (and (slack-page-state-loaded-p state)
                                  (not page-recorded))
                         (setq page-recorded t
                               events (append events '(page))))))
                    ((symbol-function 'slack-conversations-history)
                     (lambda (_room _team &rest args)
                       (setq events (append events '(request))
                             primary (plist-get args :on-primary-page)
                             hydrated (plist-get args :after-success)))))
            (slack-room-display
             channel team
             (lambda () (setq events (append events '(ready)))))
            (should (= 1 (length (oref channel history-snapshots))))
            (funcall primary (list (slack-test-room-message "1.000" "page")) "")
            (should-not (oref channel history-snapshots))
            (setq events (append events '(hydrated)))
            (funcall hydrated nil "")
            (should (equal '(display request page hydrated ready) events)))
        (slack-test-kill-room-buffer channel team)))))

(ert-deftest slack-test-room-history-stale-primary-releases-snapshot-once ()
  "A stale interactive primary releases its history snapshot exactly once."
  (slack-test-setup
    (oset team mark-as-read-immediately nil)
    (let ((state (oref channel history-state))
          (release-function
           (symbol-function 'slack-room-history-release-snapshot))
          (release-count 0)
          primary
          hydrated)
      (unwind-protect
          (cl-letf (((symbol-function 'slack-buffer-display) #'ignore)
                    ((symbol-function 'slack-room-history-release-snapshot)
                     (lambda (room snapshot)
                       (cl-incf release-count)
                       (funcall release-function room snapshot)))
                    ((symbol-function 'slack-conversations-history)
                     (lambda (_room _team &rest args)
                       (setq primary (plist-get args :on-primary-page)
                             hydrated (plist-get args :after-success)))))
            (slack-room-display channel team)
            (should (= 1 (length (oref channel history-snapshots))))
            (let ((replacement-generation (slack-page-state-restart state)))
              (funcall primary
                       (list (slack-test-room-message "1.000" "stale"))
                       "stale-cursor")
              (funcall hydrated nil "stale-cursor")
              (should (= 1 release-count))
              (should-not (oref channel history-snapshots))
              (should (= replacement-generation
                         (slack-page-state-generation state)))
              (should (eq 'loading (slack-page-state-status state)))
              (should-not (slack-page-state-loaded-p state))
              (should-not (slack-page-state-value state))))
        (slack-test-kill-room-buffer channel team)))))

(ert-deftest slack-test-room-history-sync-error-releases-snapshot-once ()
  "A synchronous interactive history error releases its snapshot once."
  (slack-test-setup
    (oset team mark-as-read-immediately nil)
    (let ((state (oref channel history-state))
          (release-function
           (symbol-function 'slack-room-history-release-snapshot))
          (release-count 0))
      (unwind-protect
          (cl-letf (((symbol-function 'slack-buffer-display) #'ignore)
                    ((symbol-function 'slack-room-history-release-snapshot)
                     (lambda (room snapshot)
                       (cl-incf release-count)
                       (funcall release-function room snapshot)))
                    ((symbol-function 'slack-conversations-history)
                     (lambda (&rest _args)
                       (error "synchronous history failure"))))
            (slack-room-display channel team)
            (should (= 1 release-count))
            (should-not (oref channel history-snapshots))
            (should (eq 'failed (slack-page-state-status state)))
            (should (equal '(error "synchronous history failure")
                           (slack-page-state-error state))))
        (slack-test-kill-room-buffer channel team)))))

(ert-deftest slack-test-room-display-dormant-im-displays-before-open ()
  "A dormant direct-message shell appears before conversations.open starts."
  (slack-test-setup
    (let* ((im (make-instance 'slack-im
                              :id "D11111"
                              :user user-id
                              :properties '(:is_dormant t)))
           (events nil)
           open-success)
      (oset team mark-as-read-immediately nil)
      (puthash (oref im id) im (oref team ims))
      (unwind-protect
          (cl-letf (((symbol-function 'slack-buffer-display)
                     (lambda (_object)
                       (setq events (append events '(display)))))
                    ((symbol-function 'slack-conversations-open)
                     (lambda (_team &rest args)
                       (setq events (append events '(open))
                             open-success (plist-get args :on-success))))
                    ((symbol-function 'slack-conversations-history)
                     (lambda (&rest _args)
                       (setq events (append events '(history))))))
            (slack-room-display im team)
            (should (equal '(display open) events))
            (funcall open-success nil)
            (should (equal '(display open history) events)))
        (slack-test-kill-room-buffer im team)))))

(ert-deftest slack-test-room-display-open-and-history-errors-fail-state ()
  "Both dormant-IM open errors and history errors fail their room state."
  (slack-test-setup
    (let ((im (make-instance 'slack-im
                             :id "D11111"
                             :user user-id
                             :properties '(:is_dormant t)))
          open-error
          history-error)
      (oset team mark-as-read-immediately nil)
      (puthash (oref im id) im (oref team ims))
      (unwind-protect
          (cl-letf (((symbol-function 'slack-buffer-display) #'ignore)
                    ((symbol-function 'slack-conversations-open)
                     (lambda (_team &rest args)
                       (setq open-error (plist-get args :on-error))))
                    ((symbol-function 'slack-conversations-history)
                     (lambda (_room _team &rest args)
                       (setq history-error (plist-get args :on-error)))))
            (slack-room-display im team)
            (funcall open-error "cannot_open")
            (should (eq 'failed
                        (slack-page-state-status (oref im history-state))))
            (slack-room-display channel team)
            (funcall history-error "channel_not_found")
            (should (eq 'failed
                        (slack-page-state-status
                         (oref channel history-state)))))
        (slack-test-kill-room-buffer im team)
        (slack-test-kill-room-buffer channel team)))))

(ert-deftest slack-test-conversations-open-routes-api-and-transport-errors ()
  "Opening a conversation reports both API and transport failures."
  (slack-test-setup
    (dolist (failure '(api transport))
      (let (request errors success-called)
        (cl-letf (((symbol-function 'slack-request)
                   (lambda (candidate &rest _ignored)
                     (setq request candidate))))
          (slack-conversations-open
           team
           :room channel
           :on-success (lambda (&rest _ignored) (setq success-called t))
           :on-error (lambda (&rest arguments) (push arguments errors))))
        (pcase failure
          ('api
           (funcall (oref request success)
                    :data '(:ok :json-false :error "channel_not_found")))
          ('transport
           (funcall (oref request error)
                    :error-thrown '(error "offline")
                    :symbol-status 'error)))
        (should (= 1 (length errors)))
        (should-not success-called)))))

(ert-deftest slack-test-prefetch-and-display-share-room-history-state ()
  "Unread prefetch and room display coalesce and publish through one state."
  (slack-test-setup
    (oset team counts t)
    (oset team mark-as-read-immediately nil)
    (let ((request-count 0)
          primary
          hydrated
          displayed-object)
      (unwind-protect
          (cl-letf (((symbol-function 'slack-room-has-unread-p)
                     (lambda (&rest _args) t))
                    ((symbol-function 'slack-room-muted-p)
                     (lambda (&rest _args) nil))
                    ((symbol-function 'slack-room--update-latest) #'ignore)
                    ((symbol-function 'run-at-time)
                     (lambda (_delay _repeat function &rest args)
                       (apply function args)))
                    ((symbol-function 'slack-conversations-history)
                     (lambda (_room _team &rest args)
                       (cl-incf request-count)
                       (setq primary (plist-get args :on-primary-page)
                             hydrated (plist-get args :after-success))))
                    ((symbol-function 'slack-buffer-display)
                     (lambda (object) (setq displayed-object object))))
            (slack-prefetch-unread-channels team)
            (should (slack-page-state-in-flight-p
                     (oref channel history-state)))
            (slack-room-display channel team)
            (should (= 1 request-count))
            (funcall primary
                     (list (slack-test-room-message "3.000" "prefetched"))
                     "cursor-p")
            (should (equal "cursor-p"
                           (slack-page-state-continuation
                            (oref channel history-state))))
            (should (with-current-buffer (oref displayed-object buf)
                      (save-excursion
                        (goto-char (point-min))
                        (search-forward "prefetched" nil t))))
            (funcall hydrated nil "cursor-p")
            (should (eq 'ready
                        (slack-page-state-status
                         (oref channel history-state)))))
        (slack-test-kill-room-buffer channel team)))))

(ert-deftest slack-test-room-display-takes-over-delayed-prefetch ()
  "Opening a room cancels its delayed prefetch and starts history immediately."
  (slack-test-setup
    (oset team counts t)
    (oset team mark-as-read-immediately nil)
    (let ((timer (timer-create))
          scheduled-function
          scheduled-args
          canceled-timer
          (request-count 0)
          primary
          hydrated)
      (unwind-protect
          (cl-letf (((symbol-function 'slack-room-has-unread-p)
                     (lambda (&rest _args) t))
                    ((symbol-function 'slack-room-muted-p)
                     (lambda (&rest _args) nil))
                    ((symbol-function 'slack-room--update-latest) #'ignore)
                    ((symbol-function 'run-at-time)
                     (lambda (_delay _repeat function &rest args)
                       (setq scheduled-function function
                             scheduled-args args)
                       timer))
                    ((symbol-function 'cancel-timer)
                     (lambda (candidate) (setq canceled-timer candidate)))
                    ((symbol-function 'slack-conversations-history)
                     (lambda (_room _team &rest args)
                       (cl-incf request-count)
                       (setq primary (plist-get args :on-primary-page)
                             hydrated (plist-get args :after-success))))
                    ((symbol-function 'slack-buffer-display) #'ignore))
            (slack-prefetch-unread-channels team)
            (should (eq 'unloaded
                        (slack-page-state-status
                         (oref channel history-state))))
            (should (= 0 request-count))
            (slack-room-display channel team)
            (should (eq timer canceled-timer))
            (should (= 1 request-count))
            (apply scheduled-function scheduled-args)
            (should (= 1 request-count))
            (funcall primary
                     (list (slack-test-room-message "3.000" "interactive"))
                     "cursor-i")
            (funcall hydrated nil "cursor-i")
            (should-not (slack-page-state-error
                         (oref channel history-state)))
            (should (eq 'ready
                        (slack-page-state-status
                         (oref channel history-state)))))
        (slack-test-kill-room-buffer channel team)))))

(ert-deftest slack-test-canceled-room-prefetch-does-not-start-page-state ()
  "Canceling scheduled room prefetch leaves its page state unloaded."
  (slack-test-setup
    (oset team counts t)
    (let ((timer (timer-create))
          canceled-timer)
      (cl-letf (((symbol-function 'slack-room-has-unread-p)
                 (lambda (&rest _args) t))
                ((symbol-function 'slack-room-muted-p)
                 (lambda (&rest _args) nil))
                ((symbol-function 'run-at-time)
                 (lambda (&rest _args) timer))
                ((symbol-function 'cancel-timer)
                 (lambda (candidate) (setq canceled-timer candidate))))
        (slack-prefetch-unread-channels team)
        (slack-room-cancel-history-prefetch channel)
        (should (eq timer canceled-timer))
        (should (eq 'unloaded
                    (slack-page-state-status
                     (oref channel history-state))))
        (should-not (slack-page-state-in-flight-p
                     (oref channel history-state)))))))

(ert-deftest slack-test-prefetch-skips-loaded-and-in-flight-room-history ()
  "Unread prefetch does not duplicate loaded or in-flight room requests."
  (slack-test-setup
    (oset team counts t)
    (let ((request-count 0)
          (state (oref channel history-state)))
      (cl-letf (((symbol-function 'slack-room-has-unread-p)
                 (lambda (&rest _args) t))
                ((symbol-function 'slack-room-muted-p)
                 (lambda (&rest _args) nil))
                ((symbol-function 'run-at-time)
                 (lambda (_delay _repeat function &rest args)
                   (apply function args)))
                ((symbol-function 'slack-conversations-history)
                 (lambda (&rest _args) (cl-incf request-count))))
        (slack-page-state-store state nil "" nil)
        (slack-prefetch-unread-channels team)
        (should (= 0 request-count))
        (slack-page-state-restart state)
        (slack-prefetch-unread-channels team)
        (should (= 0 request-count))))))

(ert-deftest slack-test-prefetch-history-error-fails-room-generation ()
  "A prefetch failure reaches the same terminal room history state."
  (slack-test-setup
    (let* ((state (oref channel history-state))
           (generation (slack-page-state-begin state))
           on-error)
      (cl-letf (((symbol-function 'slack-conversations-history)
                 (lambda (_room _team &rest args)
                   (setq on-error (plist-get args :on-error)))))
        (slack-prefetch-room-history channel team generation)
        (should (= 1 (length (oref channel history-snapshots))))
        (funcall on-error
                 :error-thrown '(error "offline")
                 :symbol-status 'error)
        (should (eq 'failed (slack-page-state-status state)))
        (should (equal "offline" (slack-page-state-error state)))
        (should-not (oref channel history-snapshots))))))

(ert-deftest slack-test-prefetch-stale-primary-releases-snapshot-once ()
  "A stale prefetch primary releases its history snapshot exactly once."
  (slack-test-setup
    (let* ((state (oref channel history-state))
           (generation (slack-page-state-begin state))
           (release-function
            (symbol-function 'slack-room-history-release-snapshot))
           (release-count 0)
           primary
           hydrated)
      (cl-letf (((symbol-function 'slack-room-history-release-snapshot)
                 (lambda (room snapshot)
                   (cl-incf release-count)
                   (funcall release-function room snapshot)))
                ((symbol-function 'slack-conversations-history)
                 (lambda (_room _team &rest args)
                   (setq primary (plist-get args :on-primary-page)
                         hydrated (plist-get args :after-success)))))
        (slack-prefetch-room-history channel team generation)
        (should (= 1 (length (oref channel history-snapshots))))
        (let ((replacement-generation (slack-page-state-restart state)))
          (funcall primary
                   (list (slack-test-room-message "1.000" "stale"))
                   "stale-cursor")
          (funcall hydrated nil "stale-cursor")
          (should (= 1 release-count))
          (should-not (oref channel history-snapshots))
          (should (= replacement-generation
                     (slack-page-state-generation state)))
          (should (eq 'loading (slack-page-state-status state)))
          (should-not (slack-page-state-loaded-p state))
          (should-not (slack-page-state-value state)))))))

(ert-deftest slack-test-prefetch-sync-error-releases-snapshot-once ()
  "A synchronous prefetch history error releases its snapshot once."
  (slack-test-setup
    (let* ((state (oref channel history-state))
           (generation (slack-page-state-begin state))
           (release-function
            (symbol-function 'slack-room-history-release-snapshot))
           (release-count 0))
      (cl-letf (((symbol-function 'slack-room-history-release-snapshot)
                 (lambda (room snapshot)
                   (cl-incf release-count)
                   (funcall release-function room snapshot)))
                ((symbol-function 'slack-conversations-history)
                 (lambda (&rest _args)
                   (error "synchronous prefetch failure"))))
        (slack-prefetch-room-history channel team generation)
        (should (= 1 release-count))
        (should-not (oref channel history-snapshots))
        (should (eq 'failed (slack-page-state-status state)))
        (should (equal '(error "synchronous prefetch failure")
                       (slack-page-state-error state)))))))

(ert-deftest slack-test-room-load-more-updates-shared-continuation-on-primary-page ()
  "Pagination publishes its next cursor and messages before user hydration."
  (slack-test-setup
    (oset team mark-as-read-immediately nil)
    (let* ((initial (slack-test-room-message "2.000" "initial"))
           (older (slack-test-room-message "1.000" "older"))
           (state (oref channel history-state))
           (object (slack-create-message-buffer channel "cursor-1" team))
           buffer
           primary
           hydrated)
      (slack-room-set-messages channel (list initial) team)
      (slack-page-state-store state (list initial) "cursor-1" t)
      (unwind-protect
          (cl-letf (((symbol-function 'slack-conversations-history)
                     (lambda (_room _team &rest args)
                       (setq primary (plist-get args :on-primary-page)
                             hydrated (plist-get args :after-success)))))
            (setq buffer (slack-buffer-buffer object))
            (with-current-buffer buffer
              (should (slack-buffer-goto "2.000"))
              (slack-buffer-load-more object))
            (should (functionp primary))
            (funcall primary (list older) "cursor-2")
            (should (eq older (slack-room-find-message channel "1.000")))
            (should (equal (list older initial)
                           (slack-page-state-value state)))
            (should (equal "cursor-2"
                           (slack-page-state-continuation state)))
            (should (equal "cursor-2" (oref object cursor)))
            (with-current-buffer buffer
              (should (equal "2.000" (get-text-property (point) 'ts))))
            (funcall hydrated nil "cursor-2")
            (with-current-buffer buffer
              (should-not slack-buffer--loading-more-p)
              (should (equal "2.000" (get-text-property (point) 'ts)))))
        (when (buffer-live-p buffer) (kill-buffer buffer))))))

(ert-deftest slack-test-room-load-more-keeps-concurrent-websocket-edit ()
  "A WebSocket edit wins over an overlapping stale pagination message."
  (slack-test-setup
    (oset team mark-as-read-immediately nil)
    (let* ((original (slack-test-room-message "2.000" "original"))
           (stale (slack-test-room-message "2.000" "stale HTTP"))
           (edited (slack-test-room-message "2.000" "WebSocket edit"))
           (older (slack-test-room-message "1.000" "older"))
           (state (oref channel history-state))
           (object (slack-create-message-buffer channel "cursor-1" team))
           buffer
           primary
           hydrated)
      (slack-room-set-messages channel (list original) team)
      (slack-page-state-store state (list original) "cursor-1" t)
      (unwind-protect
          (cl-letf (((symbol-function 'slack-conversations-history)
                     (lambda (_room _team &rest args)
                       (setq primary (plist-get args :on-primary-page)
                             hydrated (plist-get args :after-success)))))
            (setq buffer (slack-buffer-buffer object))
            (with-current-buffer buffer
              (slack-buffer-load-more object))
            (should (= 1 (length (oref channel history-snapshots))))
            (slack-room-push-message channel edited team)
            (funcall primary (list older stale) "cursor-2")
            (should-not (oref channel history-snapshots))
            (should (eq edited
                        (slack-room-find-message channel "2.000")))
            (should (equal (list older edited)
                           (slack-page-state-value state)))
            (funcall hydrated nil "cursor-2"))
        (when (buffer-live-p buffer) (kill-buffer buffer))))))

(ert-deftest slack-test-room-load-more-keeps-concurrent-websocket-delete ()
  "A WebSocket delete prevents pagination from resurrecting its message."
  (slack-test-setup
    (oset team mark-as-read-immediately nil)
    (let* ((original (slack-test-room-message "2.000" "original"))
           (stale (slack-test-room-message "2.000" "stale HTTP"))
           (older (slack-test-room-message "1.000" "older"))
           (state (oref channel history-state))
           (object (slack-create-message-buffer channel "cursor-1" team))
           buffer
           primary
           hydrated)
      (slack-room-set-messages channel (list original) team)
      (slack-page-state-store state (list original) "cursor-1" t)
      (unwind-protect
          (cl-letf (((symbol-function 'slack-conversations-history)
                     (lambda (_room _team &rest args)
                       (setq primary (plist-get args :on-primary-page)
                             hydrated (plist-get args :after-success)))))
            (setq buffer (slack-buffer-buffer object))
            (with-current-buffer buffer
              (slack-buffer-load-more object))
            (slack-room-delete-message channel "2.000")
            (funcall primary (list older stale) "cursor-2")
            (should-not (slack-room-find-message channel "2.000"))
            (should (equal (list older)
                           (slack-page-state-value state)))
            (funcall hydrated nil "cursor-2"))
        (when (buffer-live-p buffer) (kill-buffer buffer))))))

(ert-deftest slack-test-room-load-more-does-not-delete-http-omissions ()
  "Pagination keeps cached messages omitted from its append-only HTTP page."
  (slack-test-setup
    (oset team mark-as-read-immediately nil)
    (let* ((initial (slack-test-room-message "2.000" "initial"))
           (older (slack-test-room-message "1.000" "older"))
           (state (oref channel history-state))
           (object (slack-create-message-buffer channel "cursor-1" team))
           buffer
           primary
           hydrated)
      (slack-room-set-messages channel (list initial) team)
      (slack-page-state-store state (list initial) "cursor-1" t)
      (unwind-protect
          (cl-letf (((symbol-function 'slack-conversations-history)
                     (lambda (_room _team &rest args)
                       (setq primary (plist-get args :on-primary-page)
                             hydrated (plist-get args :after-success)))))
            (setq buffer (slack-buffer-buffer object))
            (with-current-buffer buffer
              (slack-buffer-load-more object))
            (funcall primary (list older) "cursor-2")
            (should (eq initial
                        (slack-room-find-message channel "2.000")))
            (should (equal (list older initial)
                           (slack-page-state-value state)))
            (funcall hydrated nil "cursor-2"))
        (when (buffer-live-p buffer) (kill-buffer buffer))))))

(ert-deftest slack-test-room-load-more-coalesces-duplicate-invocations ()
  "A room buffer sends only one pagination request while one is in flight."
  (slack-test-setup
    (oset team mark-as-read-immediately nil)
    (let* ((initial (slack-test-room-message "2.000" "initial"))
           (state (oref channel history-state))
           (object (slack-create-message-buffer channel "cursor-1" team))
           (request-count 0)
           buffer
           hydrated)
      (slack-room-set-messages channel (list initial) team)
      (slack-page-state-store state (list initial) "cursor-1" t)
      (unwind-protect
          (cl-letf (((symbol-function 'slack-conversations-history)
                     (lambda (_room _team &rest args)
                       (cl-incf request-count)
                       (setq hydrated (plist-get args :after-success)))))
            (setq buffer (slack-buffer-buffer object))
            (with-current-buffer buffer
              (slack-buffer-load-more object)
              (slack-buffer-load-more object)
              (should slack-buffer--loading-more-p))
            (should (= 1 request-count))
            (funcall hydrated nil "cursor-1")
            (with-current-buffer buffer
              (should-not slack-buffer--loading-more-p)))
        (when (buffer-live-p buffer) (kill-buffer buffer))))))

(ert-deftest slack-test-room-load-more-rejects-stale-generation-callback ()
  "An older pagination page cannot overwrite a newer room refresh."
  (slack-test-setup
    (oset team mark-as-read-immediately nil)
    (let* ((initial (slack-test-room-message "2.000" "initial"))
           (older (slack-test-room-message "1.000" "stale older"))
           (refreshed (slack-test-room-message "3.000" "refreshed"))
           (state (oref channel history-state))
           (object (slack-create-message-buffer channel "cursor-1" team))
           buffer
           primary
           hydrated)
      (slack-room-set-messages channel (list initial) team)
      (slack-page-state-store state (list initial) "cursor-1" t)
      (unwind-protect
          (cl-letf (((symbol-function 'slack-conversations-history)
                     (lambda (_room _team &rest args)
                       (setq primary (plist-get args :on-primary-page)
                             hydrated (plist-get args :after-success)))))
            (setq buffer (slack-buffer-buffer object))
            (with-current-buffer buffer
              (slack-buffer-load-more object))
            (let ((generation (slack-page-state-restart state)))
              (slack-room-set-messages channel (list refreshed) team)
              (slack-page-state-commit
               state generation (list refreshed) "cursor-refresh" t))
            (funcall primary (list older) "cursor-2")
            (should-not (slack-room-find-message channel "1.000"))
            (should (eq refreshed
                        (slack-room-find-message channel "3.000")))
            (should (equal "cursor-refresh"
                           (slack-page-state-continuation state)))
            (should (equal (list refreshed)
                           (slack-page-state-value state)))
            (funcall hydrated nil "cursor-2")
            (with-current-buffer buffer
              (should-not slack-buffer--loading-more-p)))
        (when (buffer-live-p buffer) (kill-buffer buffer))))))

(ert-deftest slack-test-room-load-more-stale-primary-releases-snapshot-once ()
  "A stale pagination primary releases its history snapshot exactly once."
  (slack-test-setup
    (oset team mark-as-read-immediately nil)
    (let* ((initial (slack-test-room-message "2.000" "initial"))
           (older (slack-test-room-message "1.000" "stale older"))
           (state (oref channel history-state))
           (object (slack-create-message-buffer channel "cursor-1" team))
           (release-function
            (symbol-function 'slack-room-history-release-snapshot))
           (release-count 0)
           buffer
           primary
           hydrated)
      (slack-room-set-messages channel (list initial) team)
      (slack-page-state-store state (list initial) "cursor-1" t)
      (unwind-protect
          (cl-letf (((symbol-function 'slack-room-history-release-snapshot)
                     (lambda (room snapshot)
                       (cl-incf release-count)
                       (funcall release-function room snapshot)))
                    ((symbol-function 'slack-conversations-history)
                     (lambda (_room _team &rest args)
                       (setq primary (plist-get args :on-primary-page)
                             hydrated (plist-get args :after-success)))))
            (setq buffer (slack-buffer-buffer object))
            (with-current-buffer buffer
              (slack-buffer-load-more object)
              (should slack-buffer--loading-more-p))
            (should (= 1 (length (oref channel history-snapshots))))
            (let ((replacement-generation (slack-page-state-restart state)))
              (funcall primary (list older) "cursor-2")
              (should (= 1 release-count))
              (should-not (oref channel history-snapshots))
              (should-not (slack-room-find-message channel "1.000"))
              (should (= replacement-generation
                         (slack-page-state-generation state)))
              (should (eq 'refreshing (slack-page-state-status state)))
              (should (equal (list initial)
                             (slack-page-state-value state)))
              (funcall hydrated nil "cursor-2")
              (should (= 1 release-count))
              (with-current-buffer buffer
                (should-not slack-buffer--loading-more-p))
              (should (equal "cursor-1" (oref object cursor)))))
        (when (buffer-live-p buffer) (kill-buffer buffer))))))

(ert-deftest slack-test-room-load-more-sync-error-releases-snapshot-once ()
  "A synchronous pagination history error releases its snapshot once."
  (slack-test-setup
    (oset team mark-as-read-immediately nil)
    (let* ((initial (slack-test-room-message "2.000" "initial"))
           (state (oref channel history-state))
           (object (slack-create-message-buffer channel "cursor-1" team))
           (release-function
            (symbol-function 'slack-room-history-release-snapshot))
           (release-count 0)
           buffer)
      (slack-room-set-messages channel (list initial) team)
      (slack-page-state-store state (list initial) "cursor-1" t)
      (unwind-protect
          (cl-letf (((symbol-function 'slack-room-history-release-snapshot)
                     (lambda (room snapshot)
                       (cl-incf release-count)
                       (funcall release-function room snapshot)))
                    ((symbol-function 'slack-conversations-history)
                     (lambda (&rest _args)
                       (error "synchronous pagination failure")))
                    ((symbol-function 'message) #'ignore))
            (setq buffer (slack-buffer-buffer object))
            (with-current-buffer buffer
              (slack-buffer-load-more object)
              (should-not slack-buffer--loading-more-p))
            (should (= 1 release-count))
            (should-not (oref channel history-snapshots))
            (should (eq 'ready (slack-page-state-status state)))
            (should (equal "cursor-1"
                           (slack-page-state-continuation state)))
            (should (equal "cursor-1" (oref object cursor))))
        (when (buffer-live-p buffer) (kill-buffer buffer))))))

(ert-deftest slack-test-room-load-more-rejects-stale-cursor-callback ()
  "A pagination page requested for an obsolete cursor is ignored."
  (slack-test-setup
    (oset team mark-as-read-immediately nil)
    (let* ((initial (slack-test-room-message "2.000" "initial"))
           (older (slack-test-room-message "1.000" "stale older"))
           (state (oref channel history-state))
           (object (slack-create-message-buffer channel "cursor-1" team))
           buffer
           primary
           hydrated)
      (slack-room-set-messages channel (list initial) team)
      (slack-page-state-store state (list initial) "cursor-1" t)
      (unwind-protect
          (cl-letf (((symbol-function 'slack-conversations-history)
                     (lambda (_room _team &rest args)
                       (setq primary (plist-get args :on-primary-page)
                             hydrated (plist-get args :after-success)))))
            (setq buffer (slack-buffer-buffer object))
            (with-current-buffer buffer
              (slack-buffer-load-more object))
            (oset object cursor "cursor-new")
            (setf (slack-page-state-continuation state) "cursor-new")
            (funcall primary (list older) "cursor-2")
            (should-not (slack-room-find-message channel "1.000"))
            (should (equal "cursor-new" (oref object cursor)))
            (should (equal "cursor-new"
                           (slack-page-state-continuation state)))
            (should (equal (list initial)
                           (slack-page-state-value state)))
            (funcall hydrated nil "cursor-2")
            (with-current-buffer buffer
              (should-not slack-buffer--loading-more-p)))
        (when (buffer-live-p buffer) (kill-buffer buffer))))))

(ert-deftest slack-test-room-load-more-resets-and-reports-on-error ()
  "A pagination failure is visible and does not leave the buffer guarded."
  (slack-test-setup
    (oset team mark-as-read-immediately nil)
    (let* ((initial (slack-test-room-message "2.000" "initial"))
           (state (oref channel history-state))
           (object (slack-create-message-buffer channel "cursor-1" team))
           (request-count 0)
           reported
           buffer
           on-error)
      (slack-room-set-messages channel (list initial) team)
      (slack-page-state-store state (list initial) "cursor-1" t)
      (unwind-protect
          (cl-letf (((symbol-function 'slack-conversations-history)
                     (lambda (_room _team &rest args)
                       (cl-incf request-count)
                       (setq on-error (plist-get args :on-error))))
                    ((symbol-function 'message)
                     (lambda (format-string &rest args)
                       (setq reported (apply #'format format-string args)))))
            (setq buffer (slack-buffer-buffer object))
            (with-current-buffer buffer
              (slack-buffer-load-more object)
              (should slack-buffer--loading-more-p))
            (should (= 1 (length (oref channel history-snapshots))))
            (funcall on-error
                     :error-thrown '(error "offline")
                     :symbol-status 'error)
            (with-current-buffer buffer
              (should-not slack-buffer--loading-more-p)
              (slack-buffer-load-more object))
            (should (= 1 (length (oref channel history-snapshots))))
            (should (= 2 request-count))
            (should (string-match-p "offline" reported))
            (funcall on-error "invalid_auth")
            (with-current-buffer buffer
              (should-not slack-buffer--loading-more-p))
            (should (string-match-p "invalid_auth" reported))
            (should-not (oref channel history-snapshots))
            (should (eq 'ready (slack-page-state-status state)))
            (should (equal "cursor-1"
                           (slack-page-state-continuation state))))
        (when (buffer-live-p buffer) (kill-buffer buffer))))))

(if noninteractive
    (ert-run-tests-batch-and-exit)
  (ert t))
