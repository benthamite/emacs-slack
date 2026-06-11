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
            (should (eq 1 (length elements)))
            (let ((element (car elements)))
              (should (string= "text" (plist-get element :type)))
              (should (string= "<!here> fff" (plist-get element :text)))
              (should (eq 2 (length (plist-get element :style))))
              (let ((style (plist-get element :style)))
                (should (eq t (plist-get style :bold))))))))))
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
          (should (eq 1 (plist-get section :indent)))
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
  (let* ((str (string-trim "
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
                               :items (list parent)))
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
                                  :text "Parent message"))
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
                 (lambda (_team after-success &optional _cursor)
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
                 (lambda (_team after-success &optional _cursor)
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

(if noninteractive
    (ert-run-tests-batch-and-exit)
  (ert t))
