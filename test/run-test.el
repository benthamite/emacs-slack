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
(require 'slack-dialog-buffer)

(defvar slack-channel-button-keymap nil)
(setq slack-render-image-p t)

(defmacro slack-test-setup (&rest body)
  (declare (indent 0) (debug t))
  `(let* ((slack-tokens-by-id (make-hash-table :test 'equal))
          (slack-teams-by-token (make-hash-table :test 'equal))
          (channel-id "C11111")
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
                               :id "T99999"
                               :token "xoxb-test-token"
                               :name "Test Team"
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

(defun slack-test-star-item (ts room-id)
  "Return a saved message item at TS in ROOM-ID."
  (make-instance 'slack-star-item
                 :item-id room-id
                 :item-type "message"
                 :ts ts))

(defun slack-test-search-pagination (page page-count first last)
  "Return search pagination for PAGE of PAGE-COUNT covering FIRST through LAST."
  (make-instance 'slack-search-pagination
                 :total_count last
                 :page page
                 :per_page (max 0 (1+ (- last first)))
                 :page_count page-count
                 :first first
                 :last last))

(defun slack-test-search-file (id title user-id created)
  "Return a minimal file search match with ID, TITLE, USER-ID, and CREATED."
  (make-instance 'slack-file
                 :id id
                 :created created
                 :title title
                 :mimetype "text/plain"
                 :user user-id
                 :mode "hosted"))

(defun slack-test-file-search-result
    (query matches page page-count first last)
  "Return a file search result for QUERY and MATCHES at PAGE of PAGE-COUNT."
  (make-instance 'slack-file-search-result
                 :query query
                 :sort "timestamp"
                 :sort-dir "desc"
                 :total (length matches)
                 :matches matches
                 :pagination
                 (slack-test-search-pagination page page-count first last)))

(defun slack-test-scheduled-draft (id post-at text &optional channel-id)
  "Return a scheduled draft payload with ID, POST-AT, TEXT, and CHANNEL-ID."
  (list :id id
        :date_scheduled post-at
        :last_updated_ts (format "%s.001" post-at)
        :blocks (list (list :elements
                            (list (list :elements
                                        (list (list :text text))))))
        :destinations (list (list :channel_id
                                  (or channel-id "C11111")))))

(defun slack-test-scheduled-response (&rest drafts)
  "Return a successful scheduled-list response containing DRAFTS."
  (list :ok t :drafts drafts))

(defun slack-test-pin-message-payload (ts text &optional user-id)
  "Return a pinned-message payload at TS with TEXT from USER-ID."
  (list :type "message"
        :message (list :type "message"
                       :user (or user-id "U11111")
                       :ts ts
                       :text text)))

(defun slack-test-pinned-item (room team ts text &optional user-id)
  "Return a pinned item in ROOM on TEAM at TS with TEXT from USER-ID."
  (slack-pinned-item-create
   (slack-test-pin-message-payload ts text user-id)
   room team))

(defun slack-test-file-detail (id title &optional page)
  "Return a renderable Slack file fixture with ID, TITLE, and PAGE."
  (slack-file-create
   (list :id id
         :created 1710000000
         :name title
         :title title
         :size 1000
         :public :json-false
         :filetype "text"
         :mimetype "text/plain"
         :pretty_type "Plain Text"
         :user "U11111"
         :preview ""
         :permalink (format "https://example.test/files/%s" id)
         :username "TestUser"
         :page (or page 1)
         :url_private (format "https://example.test/files/%s/view" id)
         :url_private_download "")))

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

(ert-deftest slack-test-search-empty-pagination-is-fully-initialized ()
  (let ((pagination (slack-search-empty-pagination)))
    (dolist (slot '(total-count page per-page page-count first last))
      (should (= 0 (slot-value pagination slot))))))

(ert-deftest slack-test-search-commands-display-before-request-with-stable-keys ()
  (slack-test-setup
    (let ((original-page-state (symbol-function 'slack-team-page-state))
          keys
          events
          objects)
      (unwind-protect
          (cl-letf (((symbol-function 'slack-search-query-params)
                     (lambda (&optional _query)
                       (list team "needle" "timestamp" "desc")))
                    ((symbol-function 'slack-team-page-state)
                     (lambda (state-team key)
                       (push key keys)
                       (funcall original-page-state state-team key)))
                    ((symbol-function 'slack-buffer-display)
                     (lambda (object)
                       (push object objects)
                       (push (list 'display
                                   (if (slack-file-search-result-p
                                        (oref object search-result))
                                       'file
                                     'messages))
                             events)))
                    ((symbol-function 'slack-search-request)
                     (lambda (result &rest _args)
                       (should (= 0 (oref result total)))
                       (should-not (oref result matches))
                       (dolist (slot '(total-count page per-page page-count
                                      first last))
                         (should (= 0 (slot-value
                                       (oref result pagination) slot))))
                       (push (list 'request
                                   (if (slack-file-search-result-p result)
                                       'file
                                     'messages))
                             events))))
            (slack-search-from-messages nil)
            (slack-search-from-files)
            (should
             (equal '((display messages) (request messages)
                      (display file) (request file))
                    (nreverse events)))
            (should (member '(search messages "needle" "timestamp" "desc")
                            keys))
            (should (member '(search file "needle" "timestamp" "desc")
                            keys)))
        (dolist (object objects)
          (when (buffer-live-p (oref object buf))
            (kill-buffer (oref object buf))))))))

(ert-deftest slack-test-search-primary-precedes-user-hydration ()
  (slack-test-setup
    (let* ((result (make-instance
                    'slack-search-result
                    :query "needle"
                    :sort "timestamp"
                    :sort-dir "desc"
                    :total 0
                    :matches nil
                    :pagination (slack-test-search-pagination 0 0 0 0)))
           request-success
           users-success
           events)
      (cl-letf (((symbol-function 'slack-request)
                 (lambda (request)
                   (setq request-success (oref request success))))
                ((symbol-function 'slack-team-missing-user-ids)
                 (lambda (&rest _args) (list "U-missing")))
                ((symbol-function 'slack-users-info-request)
                 (lambda (_ids _team &rest args)
                   (setq users-success (plist-get args :after-success))
                   (push 'users events))))
        (slack-search-request
         result
         (lambda () (push 'ready events))
         team 1
         (lambda (&rest _args) (push 'error events))
         (lambda (primary)
           (should (eq primary result))
           (push 'primary events)))
        (funcall
         request-success
         :data
         (list :ok t
               :query "needle"
               :messages
               (list :total 1
                     :matches
                     (list
                      (list :channel (list :id channel-id
                                           :name channel-name)
                            :user user-id
                            :username user-name
                            :permalink "https://example.com/message"
                            :type "message"
                            :text "search result"
                            :ts "1.000"))
                     :pagination
                     (list :total_count 1 :page 1 :per_page 20
                           :page_count 1 :first 1 :last 1))))
        (should (equal '(users primary) events))
        (should (= 1 (length (oref result matches))))
        (funcall users-success)
        (should (equal '(ready users primary) events))))))

(ert-deftest slack-test-search-five-argument-errors-remain-compatible ()
  (slack-test-setup
    (let ((result (slack-test-file-search-result "needle" nil 0 0 0 0))
          request-success
          request-error
          errors)
      (cl-letf (((symbol-function 'slack-request)
                 (lambda (request)
                   (setq request-success (oref request success)
                         request-error (oref request error)))))
        (slack-search-request
         result #'ignore team 1
         (lambda (&rest values) (push values errors)))
        (funcall request-success
                 :data (list :ok :json-false :error "invalid_auth"))
        (funcall request-error :error-thrown "transport failure"))
      (should (= 2 (length errors)))
      (should (equal '("invalid_auth") (cadr errors)))
      (should (equal '(:error-thrown "transport failure")
                     (car errors))))))

(ert-deftest slack-test-search-repeated-query-renders-in-same-buffer ()
  (slack-test-setup
    (let (callbacks
          object
          first-buffer)
      (unwind-protect
          (cl-letf (((symbol-function 'slack-search-query-params)
                     (lambda (&optional _query)
                       (list team "needle" "timestamp" "desc")))
                    ((symbol-function 'slack-buffer-display) #'ignore)
                    ((symbol-function 'slack-search-request)
                     (lambda (_result after-success _team &optional _page
                              on-error on-primary-page)
                       (setq callbacks
                             (list after-success on-error on-primary-page)))))
            (setq object (slack-search-from-files)
                  first-buffer (oref object buf))
            (let ((first
                   (slack-test-file-search-result
                    "needle"
                    (list (slack-test-search-file
                           "F1" "first result" user-id 1))
                    1 1 1 1)))
              (funcall (nth 2 callbacks) first)
              (funcall (car callbacks)))
            (with-current-buffer first-buffer
              (goto-char (point-min))
              (should (search-forward "first result" nil t)))
            (let ((again (slack-search-from-files)))
              (should (eq object again))
              (should (eq first-buffer (oref again buf))))
            (let ((second
                   (slack-test-file-search-result
                    "needle"
                    (list (slack-test-search-file
                           "F2" "second result" user-id 2))
                    1 1 1 1)))
              (funcall (nth 2 callbacks) second)
              (funcall (car callbacks)))
            (with-current-buffer first-buffer
              (goto-char (point-min))
              (should (search-forward "second result" nil t))
              (goto-char (point-min))
              (should-not (search-forward "first result" nil t))))
        (when (buffer-live-p first-buffer)
          (kill-buffer first-buffer))))))

(ert-deftest slack-test-search-empty-error-and-retry-stay-in-place ()
  (slack-test-setup
    (let (callbacks
          object
          emacs-buffer
          retry
          (requests 0))
      (unwind-protect
          (cl-letf (((symbol-function 'slack-search-query-params)
                     (lambda (&optional _query)
                       (list team "needle" "timestamp" "desc")))
                    ((symbol-function 'slack-buffer-display) #'ignore)
                    ((symbol-function 'slack-search-request)
                     (lambda (_result after-success _team &optional _page
                              on-error on-primary-page)
                       (cl-incf requests)
                       (setq callbacks
                             (list after-success on-error on-primary-page)))))
            (setq object (slack-search-from-messages nil)
                  emacs-buffer (oref object buf))
            (funcall
             (nth 2 callbacks)
             (make-instance
              'slack-search-result
              :query "needle"
              :sort "timestamp"
              :sort-dir "desc"
              :total 0
              :matches nil
              :pagination (slack-test-search-pagination 1 1 0 0)))
            (funcall (car callbacks))
            (with-current-buffer emacs-buffer
              (goto-char (point-min))
              (should (search-forward "(no more messages)" nil t)))
            (setq object (slack-search-from-messages nil))
            (funcall (nth 1 callbacks) "rate_limited")
            (should (eq emacs-buffer (oref object buf)))
            (with-current-buffer emacs-buffer
              (goto-char (point-min))
              (should (search-forward "Slack request failed: rate_limited"
                                      nil t))
              (setq retry slack-buffer-page-retry-function))
            (funcall retry)
            (should (= 3 requests))
            (should (eq emacs-buffer (oref object buf))))
        (when (buffer-live-p emacs-buffer)
          (kill-buffer emacs-buffer))))))

(ert-deftest slack-test-search-pagination-survives-buffer-kill ()
  (slack-test-setup
    (let (callbacks
          requested-page
          object
          old-buffer
          reopened-buffer)
      (unwind-protect
          (cl-letf (((symbol-function 'slack-search-query-params)
                     (lambda (&optional _query)
                       (list team "needle" "timestamp" "desc")))
                    ((symbol-function 'slack-buffer-display) #'ignore)
                    ((symbol-function 'slack-search-request)
                     (lambda (_result after-success _team &optional page
                              on-error on-primary-page)
                       (setq requested-page page
                             callbacks
                             (list after-success on-error on-primary-page)))))
            (setq object (slack-search-from-files)
                  old-buffer (oref object buf))
            (funcall
             (nth 2 callbacks)
             (slack-test-file-search-result
              "needle"
              (list (slack-test-search-file "F1" "first" user-id 1))
              1 2 1 1))
            (funcall (car callbacks))
            (with-current-buffer old-buffer
              (slack-buffer-load-more object))
            (should (= 2 requested-page))
            (kill-buffer old-buffer)
            (funcall
             (nth 2 callbacks)
             (slack-test-file-search-result
              "needle"
              (list (slack-test-search-file "F2" "second" user-id 2))
              2 2 2 2))
            (funcall (car callbacks))
            (let* ((key '(search file "needle" "timestamp" "desc"))
                   (state (slack-team-page-state team key)))
              (should (= 2 (length
                            (oref (slack-page-state-value state) matches))))
              (should-not (slack-page-state-continuation state))
              (should-not (buffer-live-p old-buffer)))
            (setq object (slack-search-from-files)
                  reopened-buffer (oref object buf))
            (with-current-buffer reopened-buffer
              (goto-char (point-min))
              (should (search-forward "first" nil t))
              (goto-char (point-min))
              (should (search-forward "second" nil t))))
        (when (buffer-live-p old-buffer)
          (kill-buffer old-buffer))
        (when (buffer-live-p reopened-buffer)
          (kill-buffer reopened-buffer))))))

(ert-deftest slack-test-search-load-more-rejects-stale-generation ()
  (slack-test-setup
    (let* ((key '(search file "needle" "timestamp" "desc"))
           (state (slack-team-page-state team key))
           (initial
            (slack-test-file-search-result
             "needle"
             (list (slack-test-search-file "F1" "first" user-id 1))
             1 2 1 1))
           (newer
            (slack-test-file-search-result
             "needle"
             (list (slack-test-search-file "F3" "newer" user-id 3))
             1 1 1 1))
           (object (slack-create-search-result-buffer initial team))
           (emacs-buffer (slack-buffer-buffer object))
           callbacks)
      (slack-page-state-store state initial 2 t)
      (unwind-protect
          (cl-letf (((symbol-function 'slack-search-request)
                     (lambda (_result after-success _team &optional _page
                              on-error on-primary-page)
                       (setq callbacks
                             (list after-success on-error on-primary-page)))))
            (with-current-buffer emacs-buffer
              (slack-buffer-load-more object))
            (slack-page-state-store state newer nil nil)
            (funcall
             (nth 2 callbacks)
             (slack-test-file-search-result
              "needle"
              (list (slack-test-search-file "F2" "stale" user-id 2))
              2 2 2 2))
            (funcall (car callbacks))
            (should (eq newer (slack-page-state-value state)))
            (should (equal (list "F3")
                           (mapcar (lambda (file) (oref file id))
                                   (oref newer matches))))
            (with-current-buffer emacs-buffer
              (should-not slack-buffer--loading-more-p)))
        (when (buffer-live-p emacs-buffer)
          (kill-buffer emacs-buffer))))))

(ert-deftest slack-test-search-load-more-preserves-point-through-hydration ()
  (slack-test-setup
    (let* ((key '(search file "needle" "timestamp" "desc"))
           (state (slack-team-page-state team key))
           (initial
            (slack-test-file-search-result
             "needle"
             (list (slack-test-search-file "F1" "first" user-id 1))
             1 2 1 1))
           (object (slack-create-search-result-buffer initial team))
           (emacs-buffer (slack-buffer-buffer object))
           callbacks
           original-point)
      (slack-page-state-store state initial 2 t)
      (unwind-protect
          (cl-letf (((symbol-function 'slack-search-request)
                     (lambda (_result after-success _team &optional _page
                              on-error on-primary-page)
                       (setq callbacks
                             (list after-success on-error on-primary-page)))))
            (with-current-buffer emacs-buffer
              (slack-search-result-buffer-render-page-state object state)
              (goto-char (point-min))
              (should (search-forward "first" nil t))
              (setq original-point (point))
              (slack-buffer-load-more object))
            (funcall
             (nth 2 callbacks)
             (slack-test-file-search-result
              "needle"
              (list (slack-test-search-file "F2" "second" user-id 2))
              2 2 2 2))
            (with-current-buffer emacs-buffer
              (should (= original-point (point))))
            (funcall (car callbacks))
            (with-current-buffer emacs-buffer
              (should (= original-point (point)))))
        (when (buffer-live-p emacs-buffer)
          (kill-buffer emacs-buffer))))))

(ert-deftest slack-test-search-load-more-rejects-replaced-live-owner ()
  (slack-test-setup
    (let* ((key '(search file "needle" "timestamp" "desc"))
           (state (slack-team-page-state team key))
           (initial
            (slack-test-file-search-result
             "needle"
             (list (slack-test-search-file "F1" "first" user-id 1))
             1 2 1 1))
           (object (slack-create-search-result-buffer initial team))
           (old-buffer (slack-buffer-buffer object))
           callbacks
           replacement
           replacement-buffer)
      (slack-page-state-store state initial 2 t)
      (unwind-protect
          (cl-letf (((symbol-function 'slack-search-request)
                     (lambda (_result after-success _team &optional _page
                              on-error on-primary-page)
                       (setq callbacks
                             (list after-success on-error on-primary-page)))))
            (with-current-buffer old-buffer
              (slack-search-result-buffer-render-page-state object state)
              (slack-buffer-load-more object))
            (setq replacement
                  (make-instance 'slack-search-result-buffer
                                 :team-id (oref team id)
                                 :search-result initial))
            (slack-buffer-cache-team replacement team)
            (setq replacement-buffer (slack-buffer-buffer replacement))
            (should
             (eq replacement
                 (slack-buffer-find
                  'slack-search-result-buffer team initial)))
            (funcall
             (nth 2 callbacks)
             (slack-test-file-search-result
              "needle"
              (list (slack-test-search-file "F2" "late" user-id 2))
              2 2 2 2))
            (should (eq initial (slack-page-state-value state)))
            (with-current-buffer old-buffer
              (goto-char (point-min))
              (should-not (search-forward "late" nil t))
              (should-not slack-buffer--loading-more-p)))
        (when (buffer-live-p replacement-buffer)
          (kill-buffer replacement-buffer))
        (when (buffer-live-p old-buffer)
          (kill-buffer old-buffer))))))

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
                                 :thread_ts thread-ts
                                 :reactions nil))
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
    (let* ((slack-has-unreads t)
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
    (let* ((message-ts "1710000001.000100")
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

(ert-deftest slack-test-activity-feed-cold-open-displays-before-request ()
  (slack-test-setup
    (let ((slack-current-team team)
          request-success
          displayed-object
          displayed-buffer
          room-prefetch
          events)
      (unwind-protect
          (cl-letf (((symbol-function 'slack-activity-feed--selected-team)
                     (lambda () team))
                    ((symbol-function 'slack-buffer-display)
                     (lambda (buffer)
                       (setq displayed-object buffer
                             displayed-buffer (oref buffer buf))
                       (push 'display events)))
                    ((symbol-function 'slack-activity-feed-request)
                     (lambda (_team success &rest _)
                       (setq request-success success)
                       (push 'request events)))
                    ((symbol-function
                      'slack-activity-feed--fetch-watched-activities)
                     (lambda (_team callback)
                       (funcall callback nil)))
                    ((symbol-function 'slack-activity-feed--prefetch-rooms)
                     (lambda (_activities _team callback)
                       (setq room-prefetch callback))))
            (slack-activity-feed-show)
            (should (equal '(request display) events))
            (should (functionp request-success))
            (should (buffer-live-p displayed-buffer))
            (funcall request-success
                     (list :items nil
                           :response_metadata (list :next_cursor nil)))
            (should (eq displayed-object
                        (slack-buffer-find
                         'slack-activity-feed-buffer team)))
            (should (eq displayed-buffer (oref displayed-object buf)))
            (should (functionp room-prefetch)))
        (when (buffer-live-p displayed-buffer)
          (kill-buffer displayed-buffer))))))

(ert-deftest slack-test-activity-feed-mode-has-distinct-page-state ()
  (slack-test-setup
    (let ((slack-activity-feed-mode-show-only-unread t))
      (should (equal '(activity-feed t)
                     (slack-activity-feed--page-key)))
      (should (equal '(activity-feed nil)
                     (slack-activity-feed--page-key nil)))
      (should-not
       (eq (slack-team-page-state
            team (slack-activity-feed--page-key))
           (slack-team-page-state
            team (slack-activity-feed--page-key nil)))))))

(ert-deftest slack-test-activity-feed-cache-helpers-use-page-state ()
  (slack-test-setup
    (let* ((snapshot
            (slack-activity-feed--cache-put team '(first) "cursor-1"))
           (all-state
            (slack-team-page-state
             team (slack-activity-feed--page-key nil)))
           captured-key)
      (should (eq snapshot (slack-page-state-value all-state)))
      (should (equal "cursor-1"
                     (slack-page-state-continuation all-state)))
      (should (slack-page-state-has-more all-state))
      (let ((slack-activity-feed-mode-show-only-unread t))
        (setq captured-key (slack-activity-feed--cache-key team)))
      (slack-activity-feed--cache-put-key captured-key '(unread) nil)
      (let ((unread-state
             (slack-team-page-state
              team (slack-activity-feed--page-key t))))
        (should (equal '(unread)
                       (plist-get (slack-page-state-value unread-state)
                                  :activities)))
        (should-not (slack-page-state-has-more unread-state)))
      (slack-team-page-state team '(unrelated nil))
      (let ((keys (slack-activity-feed--cache-keys-for-team team)))
        (should (= 2 (length keys)))
        (should (member '(activity-feed nil) keys))
        (should (member '(activity-feed t) keys))))))

(ert-deftest slack-test-activity-feed-cache-write-keeps-primary-generation ()
  (slack-test-setup
    (let* ((snapshot (slack-activity-feed--cache-put team '(old) nil))
           (state
            (slack-team-page-state
             team (slack-activity-feed--page-key nil)))
           (generation (slack-page-state-begin state t)))
      (should-not (slack-activity-feed--cache-put team '(event) nil))
      (should (= generation (slack-page-state-generation state)))
      (should (eq 'refreshing (slack-page-state-status state)))
      (should (eq snapshot (slack-page-state-value state))))))

(ert-deftest slack-test-activity-feed-displays-before-room-prefetch-finishes ()
  (slack-test-setup
    (let (request-success
          displayed-object
          displayed-buffer
          room-success
          message-success
          state
          state-loaded-at-room
          (ready-count 0))
      (unwind-protect
          (cl-letf (((symbol-function 'slack-activity-feed--selected-team)
                     (lambda () team))
                    ((symbol-function 'slack-activity-feed-render-page-state)
                     #'ignore)
                    ((symbol-function 'slack-buffer-display)
                     (lambda (buffer)
                       (setq displayed-object buffer
                             displayed-buffer (oref buffer buf))))
                    ((symbol-function 'slack-activity-feed-request)
                     (lambda (_team success &optional _cursor _on-error)
                       (setq request-success success)))
                    ((symbol-function
                      'slack-activity-feed--fetch-watched-activities)
                     (lambda (_team callback)
                       (funcall callback nil)))
                    ((symbol-function 'slack-activity-feed--prefetch-rooms)
                     (lambda (_activities _team callback)
                       (setq state-loaded-at-room
                             (slack-page-state-loaded-p state)
                             room-success callback)))
                    ((symbol-function 'slack-activity-feed--prefetch-messages)
                     (lambda (_activities _team callback &rest _)
                       (setq message-success callback))))
            (slack-activity-feed-show)
            (setq state
                  (slack-team-page-state
                   team (slack-activity-feed--page-key)))
            (should (buffer-live-p displayed-buffer))
            (should-not (slack-page-state-loaded-p state))
            (funcall request-success
                     (list :items nil
                           :response_metadata (list :next_cursor "next")))
            (should state-loaded-at-room)
            (should (eq 'loading (slack-page-state-status state)))
            (should (eq displayed-object
                        (slack-buffer-find
                         'slack-activity-feed-buffer team)))
            (should (eq displayed-buffer (oref displayed-object buf)))
            (should (functionp room-success))
            (should-not message-success)
            (slack-page-state-on-ready
             state (lambda (_state) (cl-incf ready-count)))
            (funcall room-success)
            (should (functionp message-success))
            (should (eq 'loading (slack-page-state-status state)))
            (funcall message-success)
            (should (eq 'ready (slack-page-state-status state)))
            (should (= 1 ready-count))
            (funcall message-success)
            (should (= 1 ready-count)))
        (when (buffer-live-p displayed-buffer)
          (kill-buffer displayed-buffer))))))

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
    (let* ((displayed-counts nil)
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
    (let* ((slack-current-team team)
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
           request-success
           displayed-object
           room-prefetch
           rendered)
      (slack-activity-feed--cache-put team (list cached-activity) nil)
      (unwind-protect
          (cl-letf (((symbol-function 'slack-activity-feed--selected-team)
                     (lambda () team))
                    ((symbol-function 'slack-activity-feed-render-page-state)
                     (lambda (_buffer state)
                       (push (plist-get (slack-page-state-value state)
                                        :activities)
                             rendered)))
                    ((symbol-function 'slack-buffer-display)
                     (lambda (buffer)
                       (setq displayed-object buffer)))
                    ((symbol-function 'slack-activity-feed-request)
                     (lambda (_team success &optional _cursor _on-error)
                       (setq request-success success)))
                    ((symbol-function
                      'slack-activity-feed--fetch-watched-activities)
                     (lambda (_team callback)
                       (funcall callback nil)))
                    ((symbol-function 'slack-activity-feed--prefetch-rooms)
                     (lambda (_activities _team callback)
                       (setq room-prefetch callback)))
                ((symbol-function 'run-at-time)
                 (lambda (&rest _args)
                       (error "state renderer must not start a timer"))))
            (slack-activity-feed-show)
            (should (equal (list cached-activity) (car (last rendered))))
            (should (functionp request-success))
            (funcall request-success
                     (list :items
                           (list (list :is_unread nil
                                       :feed_ts "1710000001.000100"
                                       :item
                                       (list :type "channel_message"
                                             :message
                                             (list :ts "1710000001.000100"
                                                   :channel channel-id))))
                           :response_metadata nil))
            (should (equal "1710000001.000100"
                           (oref (car (car rendered)) feed-ts)))
            (should (functionp room-prefetch)))
        (when (and displayed-object
                   (buffer-live-p (oref displayed-object buf)))
          (kill-buffer (oref displayed-object buf)))))))

(ert-deftest slack-test-activity-feed-refresh-and-retry-keep-stale-buffer ()
  (slack-test-setup
    (let* ((stale-snapshot
            (slack-activity-feed--cache-put team nil "cursor"))
           (state
            (slack-team-page-state
             team (slack-activity-feed--page-key nil)))
           displayed-object
           displayed-buffer
           errors)
      (unwind-protect
          (cl-letf (((symbol-function 'slack-activity-feed--selected-team)
                     (lambda () team))
                    ((symbol-function 'slack-activity-feed-render-page-state)
                     #'ignore)
                    ((symbol-function 'slack-buffer-display)
                     (lambda (buffer)
                       (setq displayed-object buffer
                             displayed-buffer (oref buffer buf))))
                    ((symbol-function 'slack-activity-feed-request)
                     (lambda (_team _success &optional _cursor on-error)
                       (push on-error errors))))
            (slack-activity-feed-show)
            (should (eq 'refreshing (slack-page-state-status state)))
            (funcall (car errors) "first failure")
            (should (eq 'failed (slack-page-state-status state)))
            (should (eq stale-snapshot (slack-page-state-value state)))
            (slack-activity-feed-refresh)
            (should (= 2 (length errors)))
            (should (eq displayed-object
                        (slack-buffer-find
                         'slack-activity-feed-buffer team)))
            (should (eq displayed-buffer (oref displayed-object buf)))
            (funcall (car errors) "second failure")
            (with-current-buffer displayed-buffer
              (funcall slack-buffer-page-retry-function))
            (should (= 3 (length errors)))
            (should (eq displayed-object
                        (slack-buffer-find
                         'slack-activity-feed-buffer team)))
            (should (eq displayed-buffer (oref displayed-object buf)))
            (should (eq stale-snapshot (slack-page-state-value state))))
        (when (buffer-live-p displayed-buffer)
          (kill-buffer displayed-buffer))))))

(ert-deftest slack-test-activity-feed-primary-processing-error-fails-state ()
  (slack-test-setup
    (let (request-success displayed-buffer)
      (unwind-protect
          (cl-letf (((symbol-function 'slack-activity-feed--selected-team)
                     (lambda () team))
                    ((symbol-function 'slack-activity-feed-render-page-state)
                     #'ignore)
                    ((symbol-function 'slack-buffer-display)
                     (lambda (object)
                       (setq displayed-buffer (oref object buf))))
                    ((symbol-function 'slack-activity-feed-request)
                     (lambda (_team success &optional _cursor _on-error)
                       (setq request-success success)))
                    ((symbol-function 'slack-activity-feed--parse-item)
                     (lambda (_item)
                       (error "invalid activity"))))
            (slack-activity-feed-show)
            (funcall request-success (list :items '(invalid)))
            (let ((state
                   (slack-team-page-state
                    team (slack-activity-feed--page-key))))
              (should (eq 'failed (slack-page-state-status state)))
              (should (equal '(error "invalid activity")
                             (slack-page-state-error state)))))
        (when (buffer-live-p displayed-buffer)
          (kill-buffer displayed-buffer))))))

(ert-deftest slack-test-activity-feed-hydration-callback-errors-finish-state ()
  (slack-test-setup
    (let (request-success
          watched-success
          message-success
          messages-callback
          unavailable-callback
          displayed-buffer
          fail-render)
      (unwind-protect
          (cl-letf (((symbol-function 'slack-activity-feed--selected-team)
                     (lambda () team))
                    ((symbol-function 'slack-activity-feed-render-page-state)
                     (lambda (&rest _)
                       (when fail-render
                         (error "watched render failed"))))
                    ((symbol-function 'slack-buffer-display)
                     (lambda (object)
                       (setq displayed-buffer (oref object buf))))
                    ((symbol-function 'slack-activity-feed-request)
                     (lambda (_team success &optional _cursor _on-error)
                       (setq request-success success)))
                    ((symbol-function
                      'slack-activity-feed--fetch-watched-activities)
                     (lambda (_team callback)
                       (setq watched-success callback)))
                    ((symbol-function 'slack-activity-feed--prefetch-rooms)
                     (lambda (_activities _team callback)
                       (funcall callback)))
                    ((symbol-function 'slack-activity-feed--prefetch-messages)
                     (lambda (_activities _team callback row-callback
                                          missing-callback)
                       (setq message-success callback
                             messages-callback row-callback
                             unavailable-callback missing-callback)))
                    ((symbol-function
                      'slack-activity-feed--replace-buffer-messages)
                     (lambda (&rest _)
                       (error "message render failed")))
                    ((symbol-function
                      'slack-activity-feed--mark-message-unavailable)
                     #'ignore)
                    ((symbol-function
                      'slack-activity-feed--replace-buffer-unavailable)
                     (lambda (&rest _)
                       (error "unavailable render failed"))))
            (slack-activity-feed-show)
            (funcall request-success (list :items nil))
            (let ((state
                   (slack-team-page-state
                    team (slack-activity-feed--page-key))))
              (should (eq 'loading (slack-page-state-status state)))
              (setq fail-render t)
              (funcall watched-success nil)
              (setq fail-render nil)
              (should (functionp messages-callback))
              (should (functionp unavailable-callback))
              (funcall messages-callback nil nil)
              (funcall unavailable-callback nil)
              (funcall message-success)
              (should (eq 'ready (slack-page-state-status state)))))
        (when (buffer-live-p displayed-buffer)
          (kill-buffer displayed-buffer))))))

(ert-deftest slack-test-activity-feed-stale-watched-hydration-is-rejected ()
  (slack-test-setup
    (let* ((state
            (slack-team-page-state
             team (slack-activity-feed--page-key nil)))
           (old-activity
            (slack-activity-feed--message-activity
             (make-instance 'slack-message
                            :type "message"
                            :channel channel-id
                            :ts "1710000000.000100"
                            :text "old")
             channel))
           (extra-activity
            (slack-activity-feed--message-activity
             (make-instance 'slack-message
                            :type "message"
                            :channel channel-id
                            :ts "1710000001.000100"
                            :text "extra")
             channel))
           (old-snapshot (list :activities (list old-activity)
                               :pagination nil))
           (replacement-snapshot (list :activities '(replacement)
                                       :pagination nil))
           (object
            (make-instance 'slack-activity-feed-buffer
                           :team-id (oref team id)
                           :room-id "__activity-feed__"
                           :cached-team team
                           :page-key (slack-activity-feed--page-key nil)
                           :activity-feed
                           (make-instance 'slack-activity-feed)))
           (buffer (generate-new-buffer " *slack-test-stale-watched*"))
           (generation (slack-page-state-begin state))
           watched-success
           (renders 0)
           (room-prefetches 0))
      (slack-page-state-commit state generation old-snapshot nil nil t)
      (slack-buffer-cache-team object team)
      (oset object buf buffer)
      (with-current-buffer buffer
        (setq-local slack-buffer-page-presentation-token (cons state nil)))
      (unwind-protect
          (cl-letf (((symbol-function
                      'slack-activity-feed--fetch-watched-activities)
                     (lambda (_team callback)
                       (setq watched-success callback)))
                    ((symbol-function
                      'slack-activity-feed-render-page-state)
                     (lambda (&rest _)
                       (cl-incf renders)))
                    ((symbol-function 'slack-activity-feed--prefetch-rooms)
                     (lambda (&rest _)
                       (cl-incf room-prefetches))))
            (slack-activity-feed--hydrate-page
             object team state generation nil old-snapshot)
            (let ((replacement-generation (slack-page-state-restart state)))
              (slack-page-state-commit
               state replacement-generation replacement-snapshot nil nil t)
              (funcall watched-success (list extra-activity))
              (should (equal (list old-activity)
                             (plist-get old-snapshot :activities)))
              (should (eq replacement-snapshot
                          (slack-page-state-value state)))
              (should (= 0 renders))
              (should (= 0 room-prefetches))))
        (when (buffer-live-p buffer)
          (kill-buffer buffer))))))

(ert-deftest slack-test-activity-feed-stale-message-hydration-is-rejected ()
  (slack-test-setup
    (let* ((state
            (slack-team-page-state
             team (slack-activity-feed--page-key nil)))
           (activity
            (slack-activity-feed--message-activity
             (make-instance 'slack-message
                            :type "message"
                            :channel channel-id
                            :ts "1710000000.000100"
                            :text "old")
             channel))
           (activity-message (oref (oref activity item) message))
           (snapshot (list :activities (list activity) :pagination nil))
           (replacement-snapshot (list :activities '(replacement)
                                       :pagination nil))
           (object
            (make-instance 'slack-activity-feed-buffer
                           :team-id (oref team id)
                           :room-id "__activity-feed__"
                           :cached-team team
                           :page-key (slack-activity-feed--page-key nil)
                           :activity-feed
                           (make-instance 'slack-activity-feed)))
           (buffer (generate-new-buffer " *slack-test-stale-messages*"))
           (generation (slack-page-state-begin state))
           watched-success
           room-success
           message-success
           messages-callback
           unavailable-callback
           (message-replacements 0)
           (unavailable-replacements 0))
      (slack-page-state-commit state generation snapshot nil nil t)
      (slack-buffer-cache-team object team)
      (oset object buf buffer)
      (with-current-buffer buffer
        (setq-local slack-buffer-page-presentation-token (cons state nil)))
      (unwind-protect
          (cl-letf (((symbol-function
                      'slack-activity-feed--fetch-watched-activities)
                     (lambda (_team callback)
                       (setq watched-success callback)))
                    ((symbol-function 'slack-activity-feed--prefetch-rooms)
                     (lambda (_activities _team callback)
                       (setq room-success callback)))
                    ((symbol-function 'slack-activity-feed--prefetch-messages)
                     (lambda (_activities _team callback row-callback
                                          missing-callback)
                       (setq message-success callback
                             messages-callback row-callback
                             unavailable-callback missing-callback)))
                    ((symbol-function
                      'slack-activity-feed--replace-buffer-messages)
                     (lambda (&rest _)
                       (cl-incf message-replacements)))
                    ((symbol-function
                      'slack-activity-feed--replace-buffer-unavailable)
                     (lambda (&rest _)
                       (cl-incf unavailable-replacements)))
                    ((symbol-function
                      'slack-activity-feed-render-page-state)
                     #'ignore))
            (slack-activity-feed--hydrate-page
             object team state generation nil snapshot)
            (funcall watched-success nil)
            (funcall room-success)
            (let ((replacement-generation (slack-page-state-restart state)))
              (slack-page-state-commit
               state replacement-generation replacement-snapshot nil nil t)
              (funcall messages-callback nil '(message))
              (funcall unavailable-callback activity)
              (funcall message-success)
              (should-not (oref activity-message source-message-unavailable))
              (should (eq replacement-snapshot
                          (slack-page-state-value state)))
              (should (= 0 message-replacements))
              (should (= 0 unavailable-replacements))))
        (when (buffer-live-p buffer)
          (kill-buffer buffer))))))

(ert-deftest slack-test-activity-feed-replays-event-before-primary-commit ()
  (slack-test-setup
    (let ((slack-current-team team)
          (slack-activity-feed-watch-channels (list channel-name))
          request-success
          watched-success
          displayed-buffer)
      (oset channel last-read "1710000000.000000")
      (slack-activity-feed--cache-put team nil nil)
      (unwind-protect
          (cl-letf (((symbol-function 'slack-activity-feed--selected-team)
                     (lambda () team))
                    ((symbol-function 'slack-activity-feed-render-page-state)
                     #'ignore)
                    ((symbol-function 'slack-buffer-display)
                     (lambda (object)
                       (setq displayed-buffer (oref object buf))))
                    ((symbol-function 'slack-activity-feed-request)
                     (lambda (_team success &optional _cursor _on-error)
                       (setq request-success success)))
                    ((symbol-function
                      'slack-activity-feed--fetch-watched-activities)
                     (lambda (_team callback)
                       (setq watched-success callback)))
                    ((symbol-function 'slack-activity-feed--prefetch-rooms)
                     (lambda (_activities _team callback)
                       (funcall callback)))
                    ((symbol-function 'slack-activity-feed--prefetch-messages)
                     (lambda (_activities _team callback &rest _)
                       (funcall callback)))
                    ((symbol-function 'force-mode-line-update)
                     #'ignore))
            (slack-activity-feed-show)
            (should
             (eq 'refreshing
                 (slack-page-state-status
                  (slack-team-page-state
                   team (slack-activity-feed--page-key nil)))))
            (slack-activity-feed-watch-channel-message
             (make-instance 'slack-message
                            :type "message"
                            :channel channel-id
                            :ts "1710000001.000100"
                            :text "live")
             channel team)
            (funcall request-success (list :items nil :response_metadata nil))
            (funcall watched-success nil)
            (let* ((state
                    (slack-team-page-state
                     team (slack-activity-feed--page-key nil)))
                   (activities
                    (plist-get (slack-page-state-value state) :activities)))
              (should (eq 'ready (slack-page-state-status state)))
              (should (= 1 (length activities)))
              (should (equal "1710000001.000100"
                             (oref (car activities) feed-ts)))))
        (when (buffer-live-p displayed-buffer)
          (kill-buffer displayed-buffer))))))

(ert-deftest slack-test-activity-feed-keeps-event-after-primary-commit ()
  (slack-test-setup
    (let ((slack-current-team team)
          (slack-activity-feed-watch-channels (list channel-name))
          request-success
          watched-success
          displayed-buffer)
      (oset channel last-read "1710000000.000000")
      (unwind-protect
          (cl-letf (((symbol-function 'slack-activity-feed--selected-team)
                     (lambda () team))
                    ((symbol-function 'slack-activity-feed-render-page-state)
                     #'ignore)
                    ((symbol-function 'slack-buffer-display)
                     (lambda (object)
                       (setq displayed-buffer (oref object buf))))
                    ((symbol-function 'slack-activity-feed-request)
                     (lambda (_team success &optional _cursor _on-error)
                       (setq request-success success)))
                    ((symbol-function
                      'slack-activity-feed--fetch-watched-activities)
                     (lambda (_team callback)
                       (setq watched-success callback)))
                    ((symbol-function 'slack-activity-feed--prefetch-rooms)
                     (lambda (_activities _team callback)
                       (funcall callback)))
                    ((symbol-function 'slack-activity-feed--prefetch-messages)
                     (lambda (_activities _team callback &rest _)
                       (funcall callback)))
                    ((symbol-function 'force-mode-line-update)
                     #'ignore))
            (slack-activity-feed-show)
            (funcall request-success (list :items nil :response_metadata nil))
            (slack-activity-feed-watch-channel-message
             (make-instance 'slack-message
                            :type "message"
                            :channel channel-id
                            :ts "1710000001.000100"
                            :text "live")
             channel team)
            (let* ((state
                    (slack-team-page-state
                     team (slack-activity-feed--page-key nil)))
                   (activities
                    (plist-get (slack-page-state-value state) :activities)))
              (should (eq 'loading (slack-page-state-status state)))
              (should (= 1 (length activities)))
              (should (equal "1710000001.000100"
                             (oref (car activities) feed-ts))))
            (funcall watched-success nil)
            (let* ((state
                    (slack-team-page-state
                     team (slack-activity-feed--page-key nil)))
                   (activities
                    (plist-get (slack-page-state-value state) :activities)))
              (should (eq 'ready (slack-page-state-status state)))
              (should (= 1 (length activities)))
              (should (equal "1710000001.000100"
                             (oref (car activities) feed-ts)))))
        (when (buffer-live-p displayed-buffer)
          (kill-buffer displayed-buffer))))))

(ert-deftest slack-test-activity-feed-watched-message-updates-existing-cache ()
  (slack-test-setup
    (let* ((slack-activity-feed-watch-channels (list channel-name))
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
    (let* ((old-activity (make-instance
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

(ert-deftest slack-test-stars-prefetch-settles-each-child-once ()
  (slack-test-setup
    (let ((items (list (slack-test-star-item "1.000" channel-id)
                       (slack-test-star-item "2.000" channel-id)))
          callbacks
          (completed 0))
      (cl-letf (((symbol-function 'slack-conversations-history)
                 (lambda (_room _team &rest args)
                   (push (cons (plist-get args :latest)
                               (cons (plist-get args :after-success)
                                     (plist-get args :on-error)))
                         callbacks))))
        (slack-stars--prefetch-messages
         items team (lambda () (cl-incf completed))))
      (let* ((first (assoc "1.000" callbacks))
             (second (assoc "2.000" callbacks))
             (first-success (cadr first))
             (first-error (cddr first))
             (second-error (cddr second)))
        (funcall first-success nil)
        (funcall first-success nil)
        (funcall first-error "duplicate terminal callback")
        (should (= 0 completed))
        (funcall second-error "transport failure")
        (funcall second-error "duplicate transport failure")
        (should (= 1 completed))))))

(ert-deftest slack-test-stars-prefetch-settles-synchronous-error ()
  (slack-test-setup
    (let ((item (slack-test-star-item "1.000" channel-id))
          (completed 0))
      (cl-letf (((symbol-function 'slack-conversations-history)
                 (lambda (&rest _args)
                   (error "synchronous history failure"))))
        (slack-stars--prefetch-messages
         (list item) team (lambda () (cl-incf completed))))
      (should (= 1 completed)))))

(ert-deftest slack-test-stars-prefetch-settles-nested-synchronous-error ()
  (slack-test-setup
    (let ((item (slack-test-star-item "1.000" channel-id))
          history-success
          (completed 0))
      (cl-letf (((symbol-function 'slack-conversations-history)
                 (lambda (_room _team &rest args)
                   (setq history-success
                         (plist-get args :after-success))))
                ((symbol-function 'slack-stars--permalink-thread-ts)
                 (lambda (&rest _args)
                   (error "synchronous permalink failure"))))
        (slack-stars--prefetch-messages
         (list item) team (lambda () (cl-incf completed)))
        (funcall
         history-success
         (list (slack-message-create
                (list :type "message"
                      :ts "2.000"
                      :text "wrong history row"
                      :user user-id
                      :channel channel-id)
                team channel))))
      (should (= 1 completed)))))

(ert-deftest slack-test-stars-list-primary-precedes-user-hydration ()
  (slack-test-setup
    (let (request-success
          users-success
          primary-star
          events)
      (cl-letf (((symbol-function 'slack-request)
                 (lambda (request)
                   (setq request-success (oref request success))))
                ((symbol-function 'slack-team-missing-user-ids)
                 (lambda (&rest _args) '("U-missing")))
                ((symbol-function 'slack-users-info-request)
                 (lambda (_ids _team &rest args)
                   (setq users-success (plist-get args :after-success))
                   (push 'users events))))
        (slack-stars-list-request
         team nil
         (lambda () (push 'ready events))
         (lambda (&rest _args) (push 'error events))
         (lambda (page stored-star)
           (should (eq page stored-star))
           (setq primary-star stored-star)
           (push 'primary events)))
        (funcall request-success
                 :data
                 (list :ok t
                       :saved_items
                       (list
                        (list :item_id channel-id
                              :item_type "message"
                              :message
                              (list :type "message"
                                    :user user-id
                                    :text "saved body"
                                    :ts "1.000")))
                       :response_metadata
                       (list :next_cursor "cursor-1"))))
      (should (eq primary-star (oref team star)))
      (should (slack-room-find-message channel "1.000"))
      (should (equal '(users primary) events))
      (should (functionp users-success))
      (funcall users-success)
      (should (equal '(ready users primary) events)))))

(ert-deftest slack-test-stars-list-refresh-preserves-live-event-delta ()
  (slack-test-setup
    (let* ((kept-item (slack-test-star-item "2.000" channel-id))
           (removed-item (slack-test-star-item "1.000" channel-id))
           (source-star (make-instance 'slack-star
                                       :items (list kept-item removed-item)
                                       :cursor "old-cursor"))
           request-success)
      (oset team star source-star)
      (cl-letf (((symbol-function 'slack-request)
                 (lambda (request)
                   (setq request-success (oref request success))))
                ((symbol-function 'slack-team-missing-user-ids)
                 (lambda (&rest _args) nil)))
        (slack-stars-list-request team)
        (slack-event-update-star-item
         (slack-create-star-event
          (list :type "star_added"
                :item (list :type "message" :channel channel-id
                            :message (list :ts "3.000"))))
         team)
        (slack-event-update-star-item
         (slack-create-star-event
          (list :type "star_removed"
                :item (list :type "message" :channel channel-id
                            :message (list :ts "1.000"))))
         team)
        (funcall
         request-success
         :data
         (list :ok t
               :saved_items
               (list
                (list :item_id channel-id :item_type "message"
                      :message (list :type "message" :user user-id
                                     :text "kept" :ts "2.000"))
                (list :item_id channel-id :item_type "message"
                      :message (list :type "message" :user user-id
                                     :text "removed" :ts "1.000")))
               :response_metadata (list :next_cursor "new-cursor"))))
      (should (equal '("3.000" "2.000")
                     (mapcar #'slack-ts
                             (slack-star-items (oref team star))))))))

(ert-deftest slack-test-stars-list-replays-pending-add-after-ack ()
  "Keep a pre-request add in an older response even after its write succeeds."
  (slack-test-setup
    (let ((ts "3.000")
          list-requests
          write-request)
      (cl-letf (((symbol-function 'slack-request)
                 (lambda (request)
                   (if (string= (oref request url) slack-stars-list-url)
                       (push request list-requests)
                     (setq write-request request))
                   request))
                ((symbol-function 'slack-team-missing-user-ids)
                 (lambda (&rest _args) nil)))
        (slack-star-api-request
         slack-message-stars-add-url
         (list (cons "item_id" channel-id)
               (cons "item_type" "message")
               (cons "ts" ts))
         team
         (lambda () (slack-team-mark-saved team channel-id ts)))
        (slack-stars-list-request team)
        (funcall (oref write-request success) :data (list :ok t))
        (funcall
         (oref (car list-requests) success)
         :data
         (list :ok t :saved_items nil
               :response_metadata (list :next_cursor "")))
        (should (slack-ts-saved-p team ts "message" channel-id))
        (slack-stars-list-request team)
        (funcall
         (oref (car list-requests) success)
         :data
         (list :ok t :saved_items nil
               :response_metadata (list :next_cursor "")))
        (should-not (slack-ts-saved-p team ts "message" channel-id))))))

(ert-deftest slack-test-stars-list-replays-pending-remove-after-ack ()
  "Keep a pre-request removal in an older response after its write succeeds."
  (slack-test-setup
    (let* ((ts "1.000")
           (item (slack-test-star-item ts channel-id))
           list-requests
           write-request)
      (oset team star (make-instance 'slack-star :items (list item)))
      (cl-letf (((symbol-function 'slack-request)
                 (lambda (request)
                   (if (string= (oref request url) slack-stars-list-url)
                       (push request list-requests)
                     (setq write-request request))
                   request))
                ((symbol-function 'slack-team-missing-user-ids)
                 (lambda (&rest _args) nil)))
        (slack-star-api-request
         slack-message-stars-remove-url
         (list (cons "item_id" channel-id)
               (cons "item_type" "message")
               (cons "ts" ts))
         team
         (lambda ()
           (slack-team-mark-unsaved team ts "message" channel-id)))
        (slack-stars-list-request team)
        (funcall (oref write-request success) :data (list :ok t))
        (funcall
         (oref (car list-requests) success)
         :data
         (list :ok t
               :saved_items
               (list (list :item_id channel-id :item_type "message" :ts ts))
               :response_metadata (list :next_cursor "")))
        (should-not (slack-ts-saved-p team ts "message" channel-id))
        (slack-stars-list-request team)
        (funcall
         (oref (car list-requests) success)
         :data
         (list :ok t
               :saved_items
               (list (list :item_id channel-id :item_type "message" :ts ts))
               :response_metadata (list :next_cursor "")))
        (should (slack-ts-saved-p team ts "message" channel-id))))))

(ert-deftest slack-test-stars-list-failed-add-rolls-back-pending-delta ()
  "Rollback a failed add and exclude it from an active list response."
  (slack-test-setup
    (let ((ts "3.000")
          list-request
          write-request)
      (cl-letf (((symbol-function 'slack-request)
                 (lambda (request)
                   (if (string= (oref request url) slack-stars-list-url)
                       (setq list-request request)
                     (setq write-request request))
                   request))
                ((symbol-function 'slack-team-missing-user-ids)
                 (lambda (&rest _args) nil)))
        (slack-star-api-request
         slack-message-stars-add-url
         (list (cons "item_id" channel-id)
               (cons "item_type" "message")
               (cons "ts" ts))
         team
         (lambda () (slack-team-mark-saved team channel-id ts)))
        (slack-stars-list-request team)
        (funcall (oref write-request success)
                 :data (list :ok :json-false :error "not_allowed"))
        (should-not (slack-ts-saved-p team ts "message" channel-id))
        (funcall
         (oref list-request success)
         :data
         (list :ok t :saved_items nil
               :response_metadata (list :next_cursor "")))
        (should-not (slack-ts-saved-p team ts "message" channel-id))))))

(ert-deftest slack-test-stars-list-failed-remove-restores-pending-delta ()
  "Rollback a failed removal and retain the server row in an active response."
  (slack-test-setup
    (let* ((ts "1.000")
           (item (slack-test-star-item ts channel-id))
           list-request
           write-request)
      (oset team star (make-instance 'slack-star :items (list item)))
      (cl-letf (((symbol-function 'slack-request)
                 (lambda (request)
                   (if (string= (oref request url) slack-stars-list-url)
                       (setq list-request request)
                     (setq write-request request))
                   request))
                ((symbol-function 'slack-team-missing-user-ids)
                 (lambda (&rest _args) nil)))
        (slack-star-api-request
         slack-message-stars-remove-url
         (list (cons "item_id" channel-id)
               (cons "item_type" "message")
               (cons "ts" ts))
         team
         (lambda ()
           (slack-team-mark-unsaved team ts "message" channel-id)))
        (slack-stars-list-request team)
        (funcall (oref write-request success)
                 :data (list :ok :json-false :error "not_allowed"))
        (should (slack-ts-saved-p team ts "message" channel-id))
        (funcall
         (oref list-request success)
         :data
         (list :ok t
               :saved_items
               (list (list :item_id channel-id :item_type "message" :ts ts))
               :response_metadata (list :next_cursor "")))
        (should (slack-ts-saved-p team ts "message" channel-id))))))

(ert-deftest slack-test-stars-list-removal-is-scoped-to-item-identity ()
  (slack-test-setup
    (let* ((other-channel-id "C22222")
           (other-channel (make-instance 'slack-channel
                                         :id other-channel-id
                                         :name "OtherChannel"))
           (shared-ts "1.000")
           request-success)
      (puthash other-channel-id other-channel (oref team channels))
      (cl-letf (((symbol-function 'slack-request)
                 (lambda (request)
                   (setq request-success (oref request success))))
                ((symbol-function 'slack-team-missing-user-ids)
                 (lambda (&rest _args) nil)))
        (slack-stars-list-request team)
        (slack-event-update-star-item
         (slack-create-star-event
          (list :type "star_removed"
                :item (list :type "message" :channel channel-id
                            :message (list :ts shared-ts))))
         team)
        (funcall
         request-success
         :data
         (list
          :ok t
          :saved_items
          (list
           (list :item_id channel-id :item_type "message"
                 :message (list :type "message" :user user-id
                                :text "removed channel" :ts shared-ts))
           (list :item_id other-channel-id :item_type "message"
                 :message (list :type "message" :user user-id
                                :text "kept channel" :ts shared-ts)))
          :response_metadata (list :next_cursor ""))))
      (should (equal (list other-channel-id)
                     (mapcar (lambda (item) (oref item item-id))
                             (slack-star-items (oref team star))))))))

(ert-deftest slack-test-team-mark-unsaved-ts-only-keeps-legacy-wide-match ()
  (slack-test-setup
    (let* ((shared-ts "1.000")
           (first (slack-test-star-item shared-ts channel-id))
           (second (slack-test-star-item shared-ts "C22222")))
      (oset team star
            (make-instance 'slack-star :items (list first second)))
      (slack-team-mark-unsaved team shared-ts)
      (should-not (slack-star-items (oref team star))))))

(ert-deftest slack-test-ts-saved-p-scopes-optional-item-identity ()
  (slack-test-setup
    (let* ((shared-ts "1.000")
           (item (slack-test-star-item shared-ts channel-id)))
      (oset team star (make-instance 'slack-star :items (list item)))
      (should (slack-ts-saved-p team shared-ts))
      (should (slack-ts-saved-p
               team shared-ts "message" channel-id))
      (should-not (slack-ts-saved-p
                   team shared-ts "message" "C22222")))))

(ert-deftest slack-test-saved-items-stale-refresh-restores-current-cache ()
  (slack-test-setup
    (let* ((old-star
            (make-instance
             'slack-star
             :items (list (slack-test-star-item "2.000" channel-id))
             :cursor "old-cursor"))
           (newer-star
            (make-instance
             'slack-star
             :items (list (slack-test-star-item "3.000" channel-id))
             :cursor "newer-cursor"))
           (state (slack-team-page-state team 'saved-items))
           request-success
           object
           emacs-buffer)
      (oset team star old-star)
      (slack-page-state-store state old-star "old-cursor" t)
      (unwind-protect
          (cl-letf (((symbol-function 'slack-buffer-display) #'ignore)
                    ((symbol-function 'slack-request)
                     (lambda (request)
                       (setq request-success (oref request success))))
                    ((symbol-function 'slack-team-missing-user-ids)
                     (lambda (&rest _args) nil)))
            (setq object (slack-stars-buffer--present team t)
                  emacs-buffer (oref object buf))
            (slack-page-state-store state newer-star "newer-cursor" t)
            (oset team star newer-star)
            (funcall
             request-success
             :data
             (list :ok t
                   :saved_items
                   (list (list :item_id channel-id
                               :item_type "message"
                               :ts "1.000"))
                   :response_metadata (list :next_cursor "")))
            (should (eq newer-star (slack-page-state-value state)))
            (should (eq newer-star (oref team star))))
        (when (buffer-live-p emacs-buffer)
          (kill-buffer emacs-buffer))))))

(ert-deftest slack-test-saved-items-cold-refresh-preserves-live-adds ()
  (slack-test-setup
    (let* ((local-ts "1.000")
           (event-ts "2.000")
           (local-message
            (slack-message-create
             (list :type "message" :ts local-ts :user user-id
                   :text "locally saved during refresh" :channel channel-id)
             team channel))
           (event-message
            (slack-message-create
             (list :type "message" :ts event-ts :user user-id
                   :text "event saved during refresh" :channel channel-id)
             team channel))
           request-success
           object
           emacs-buffer)
      (slack-room-set-messages channel (list local-message event-message) team)
      (unwind-protect
          (cl-letf (((symbol-function 'slack-team-select) (lambda () team))
                    ((symbol-function 'slack-team-ensure-conversations-loaded)
                     #'ignore)
                    ((symbol-function 'slack-buffer-display)
                     (lambda (displayed)
                       (setq object displayed
                             emacs-buffer (oref displayed buf))))
                    ((symbol-function 'slack-request)
                     (lambda (request)
                       (setq request-success (oref request success)))))
            (should-not (oref team star))
            (slack-saved-items)
            (slack-team-mark-saved team channel-id local-ts)
            (slack-event-update-star-item
             (slack-create-star-event
              (list :type "star_added"
                    :item (list :type "message" :channel channel-id
                                :message (list :ts event-ts))))
             team)
            (funcall request-success
                     :data
                     (list :ok t :saved_items nil
                           :response_metadata (list :next_cursor "")))
            (let* ((state (slack-team-page-state team 'saved-items))
                   (cached-ts
                    (mapcar #'slack-ts
                            (slack-star-items (oref team star))))
                   (durable-ts
                    (mapcar #'slack-ts
                            (slack-star-items
                             (slack-page-state-value state)))))
              (should (equal (list event-ts local-ts) cached-ts))
              (should (equal cached-ts durable-ts))
              (should (eq 'ready (slack-page-state-status state))))
            (with-current-buffer emacs-buffer
              (goto-char (point-min))
              (should (search-forward "event saved during refresh" nil t))
              (goto-char (point-min))
              (should (search-forward "locally saved during refresh" nil t))))
        (when (buffer-live-p emacs-buffer)
          (kill-buffer emacs-buffer))))))

(ert-deftest slack-test-saved-items-cold-refresh-preserves-live-removals ()
  (slack-test-setup
    (let ((local-ts "1.000")
          (event-ts "2.000")
          request-success
          object
          emacs-buffer)
      (unwind-protect
          (cl-letf (((symbol-function 'slack-team-select) (lambda () team))
                    ((symbol-function 'slack-team-ensure-conversations-loaded)
                     #'ignore)
                    ((symbol-function 'slack-buffer-display)
                     (lambda (displayed)
                       (setq object displayed
                             emacs-buffer (oref displayed buf))))
                    ((symbol-function 'slack-request)
                     (lambda (request)
                       (setq request-success (oref request success)))))
            (should-not (oref team star))
            (slack-saved-items)
            (slack-team-mark-unsaved team local-ts)
            (slack-event-update-star-item
             (slack-create-star-event
              (list :type "star_removed"
                    :item (list :type "message" :channel channel-id
                                :message (list :ts event-ts))))
             team)
            (funcall
             request-success
             :data
             (list
              :ok t
              :saved_items
              (list
               (list :item_id channel-id :item_type "message"
                     :message
                     (list :type "message" :user user-id
                           :text "stale local removal" :ts local-ts))
               (list :item_id channel-id :item_type "message"
                     :message
                     (list :type "message" :user user-id
                           :text "stale event removal" :ts event-ts)))
              :response_metadata (list :next_cursor "")))
            (let* ((state (slack-team-page-state team 'saved-items))
                   (cached (slack-star-items (oref team star)))
                   (durable
                    (slack-star-items (slack-page-state-value state))))
              (should-not cached)
              (should-not durable)
              (should (eq 'ready (slack-page-state-status state))))
            (with-current-buffer emacs-buffer
              (goto-char (point-min))
              (should-not (search-forward "stale local removal" nil t))
              (goto-char (point-min))
              (should-not (search-forward "stale event removal" nil t))
              (goto-char (point-min))
              (should (search-forward "(no more items)" nil t))))
        (when (buffer-live-p emacs-buffer)
          (kill-buffer emacs-buffer))))))

(ert-deftest slack-test-star-mutation-journal-prunes-without-losing-active-delta ()
  (slack-test-setup
    (let ((slack-star-mutation-journal-limit 2)
          token)
      (slack-star-mutation-journal-record-remove team "old-1")
      (slack-star-mutation-journal-record-remove team "old-2")
      (slack-star-mutation-journal-record-remove team "old-3")
      (should (= 2 (slack-star-mutation-journal-size team)))
      (setq token (slack-star-mutation-journal-register team))
      (slack-star-mutation-journal-record-remove team "new-1")
      (slack-star-mutation-journal-record-remove team "new-2")
      (slack-star-mutation-journal-record-remove team "new-3")
      ;; Entries needed by an active request are retained even above the
      ;; idle-history bound; releasing it immediately restores the bound.
      (should (= 3 (length
                    (slack-star-mutation-journal-entries-since team token))))
      (slack-star-mutation-journal-release team token)
      (should (= 2 (slack-star-mutation-journal-size team))))))

(ert-deftest slack-test-stars-list-hydrates-saved-item-authors ()
  (slack-test-setup
    (let ((missing-user-id "U-saved-author")
          request-success
          requested-user-ids)
      (cl-letf (((symbol-function 'slack-request)
                 (lambda (request)
                   (setq request-success (oref request success))))
                ((symbol-function 'slack-users-info-request)
                 (lambda (user-ids _team &rest _args)
                   (setq requested-user-ids user-ids))))
        (slack-stars-list-request team)
        (funcall
         request-success
         :data
         (list :ok t
               :saved_items
               (list
                (list :item_id channel-id :item_type "message"
                      :message (list :type "message" :user missing-user-id
                                     :text "unknown author" :ts "1.000")))
               :response_metadata (list :next_cursor ""))))
      (should (equal (list missing-user-id) requested-user-ids)))))

(ert-deftest slack-test-stars-list-four-argument-call-remains-compatible ()
  (slack-test-setup
    (let (request-success
          events)
      (cl-letf (((symbol-function 'slack-request)
                 (lambda (request)
                   (setq request-success (oref request success))))
                ((symbol-function 'slack-team-missing-user-ids)
                 (lambda (&rest _args) nil)))
        (slack-stars-list-request
         team "cursor-1"
         (lambda () (push 'ready events))
         (lambda (&rest _args) (push 'error events)))
        (funcall request-success
                 :data
                 (list :ok t
                       :saved_items nil
                       :response_metadata (list :next_cursor ""))))
      (should (equal '(ready) events))
      (should (equal "" (oref (oref team star) cursor))))))

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
    (unwind-protect
        (progn
          (cl-letf (((symbol-function 'slack-list-scheduled-messages-request)
                     (lambda (_team after-success &optional _on-error)
                       (setq slack-current-team team2)
                       (funcall after-success
                                :data
                                '(:ok t
                                  :drafts ((:id "D1"
                                            :date_scheduled 1710000000
                                            :last_updated_ts "1710000000.001"
                                            :blocks ((:elements ((:elements ((:text "hello"))))))
                                            :destinations ((:channel_id "C11111"))))))))
                    ((symbol-function 'slack-buffer-display)
                     (lambda (buffer)
                       (setq captured-buffer buffer))))
            (slack-scheduled-messages-show team1))
          (should (string= "T1" (oref captured-buffer team-id))))
      (when (and captured-buffer
                 (buffer-live-p (oref captured-buffer buf)))
        (kill-buffer (oref captured-buffer buf))))))

(ert-deftest slack-test-scheduled-messages-cold-open-displays-before-request ()
  (slack-test-setup
    (let (events object emacs-buffer request-success)
      (unwind-protect
          (cl-letf (((symbol-function 'slack-scheduled-messages--team)
                     (lambda (&optional _selected) team))
                    ((symbol-function 'slack-buffer-display)
                     (lambda (displayed)
                       (setq object displayed
                             emacs-buffer (oref displayed buf))
                       (push 'display events)))
                    ((symbol-function 'slack-list-scheduled-messages-request)
                     (lambda (_team after-success &optional _on-error)
                       (setq request-success after-success)
                       (push 'request events))))
            (slack-scheduled-messages-show team)
            (should (equal '(display request) (nreverse events)))
            (should (eq object
                        (slack-buffer-find
                         'slack-scheduled-messages-buffer team)))
            (should-not (oref object messages))
            (should (functionp request-success))
            (should (eq 'loading
                        (slack-page-state-status
                         (slack-team-page-state team 'scheduled-messages)))))
        (when (buffer-live-p emacs-buffer)
          (kill-buffer emacs-buffer))))))

(ert-deftest slack-test-scheduled-messages-parse-sorts-and-skips-unscheduled ()
  (let ((parsed
         (slack-scheduled-messages-parse
          (slack-test-scheduled-response
           (list :id "unscheduled" :last_updated_ts "1")
           (slack-test-scheduled-draft "later" 30 "later")
           (slack-test-scheduled-draft "earlier" 10 "earlier")))))
    (should (equal '("earlier" "later")
                   (mapcar (lambda (message) (oref message draft-id))
                           parsed)))
    (should-not (slack-scheduled-messages-parse
                 (slack-test-scheduled-response)))))

(ert-deftest slack-test-scheduled-messages-refreshes-same-buffer-in-place ()
  (slack-test-setup
    (let (request-success object emacs-buffer)
      (unwind-protect
          (cl-letf (((symbol-function 'slack-scheduled-messages--team)
                     (lambda (&optional _selected) team))
                    ((symbol-function 'slack-buffer-display) #'ignore)
                    ((symbol-function 'slack-list-scheduled-messages-request)
                     (lambda (_team after-success &optional _on-error)
                       (setq request-success after-success))))
            (setq object (slack-scheduled-messages-show team)
                  emacs-buffer (oref object buf))
            (funcall request-success
                     :data
                     (slack-test-scheduled-response
                      (slack-test-scheduled-draft "D1" 10 "first")))
            (should (eq 'ready
                        (slack-page-state-status
                         (slack-team-page-state team 'scheduled-messages))))
            (with-current-buffer emacs-buffer
              (goto-char (point-min))
              (should (search-forward "first" nil t)))
            (should (eq object (slack-scheduled-messages-show team)))
            (should (eq emacs-buffer (oref object buf)))
            (funcall request-success
                     :data
                     (slack-test-scheduled-response
                      (slack-test-scheduled-draft "D2" 20 "second")))
            (with-current-buffer emacs-buffer
              (goto-char (point-min))
              (should (search-forward "second" nil t))
              (goto-char (point-min))
              (should-not (search-forward "first" nil t)))
            (should (eq object (slack-scheduled-messages-show team)))
            (funcall request-success
                     :data (slack-test-scheduled-response))
            (should (eq emacs-buffer (oref object buf)))
            (should-not (oref object messages))
            (with-current-buffer emacs-buffer
              (goto-char (point-min))
              (should (search-forward "(No scheduled messages.)" nil t))))
        (when (buffer-live-p emacs-buffer)
          (kill-buffer emacs-buffer))))))

(ert-deftest slack-test-scheduled-messages-failure-retries-in-place ()
  (slack-test-setup
    (let ((requests 0)
          request-success
          request-error
          object
          emacs-buffer
          retry)
      (unwind-protect
          (cl-letf (((symbol-function 'slack-scheduled-messages--team)
                     (lambda (&optional _selected) team))
                    ((symbol-function 'slack-buffer-display) #'ignore)
                    ((symbol-function 'slack-list-scheduled-messages-request)
                     (lambda (_team after-success &optional on-error)
                       (cl-incf requests)
                       (setq request-success after-success
                             request-error on-error))))
            (setq object (slack-scheduled-messages-show team)
                  emacs-buffer (oref object buf))
            (funcall request-success
                     :data (list :ok :json-false :error "invalid_auth"))
            (should (eq 'failed
                        (slack-page-state-status
                         (slack-team-page-state team 'scheduled-messages))))
            (with-current-buffer emacs-buffer
              (goto-char (point-min))
              (should (search-forward "Slack request failed: invalid_auth"
                                      nil t))
              (setq retry slack-buffer-page-retry-function))
            (funcall retry)
            (should (= 2 requests))
            (funcall request-error :error-thrown "network down")
            (should (eq 'failed
                        (slack-page-state-status
                         (slack-team-page-state team 'scheduled-messages))))
            (with-current-buffer emacs-buffer
              (goto-char (point-min))
              (should (search-forward "network down" nil t)))
            (should (eq emacs-buffer (oref object buf))))
        (when (buffer-live-p emacs-buffer)
          (kill-buffer emacs-buffer))))))

(ert-deftest slack-test-scheduled-list-request-forwards-transport-errors ()
  (slack-test-setup
    (let (request reported)
      (cl-letf (((symbol-function 'slack-request)
                 (lambda (created) (setq request created))))
        (slack-list-scheduled-messages-request
         team #'ignore (lambda (&rest values) (setq reported values))))
      (should (functionp (oref request error)))
      (funcall (oref request error) :error-thrown "transport")
      (should (equal '(:error-thrown "transport") reported)))))

(ert-deftest slack-test-scheduled-messages-mutations-refresh-exact-buffer ()
  (slack-test-setup
    (let (request-success
          create-success
          delete-success
          object
          emacs-buffer)
      (oset team id "T11111")
      (oset team token "test-token")
      (puthash (oref team id) (oref team token) slack-tokens-by-id)
      (puthash (oref team token) team slack-teams-by-token)
      (unwind-protect
          (cl-letf (((symbol-function 'slack-scheduled-messages--team)
                     (lambda (&optional _selected) team))
                    ((symbol-function 'slack-buffer-display) #'ignore)
                    ((symbol-function 'slack-list-scheduled-messages-request)
                     (lambda (_team after-success &optional _on-error)
                       (setq request-success after-success)))
                    ((symbol-function 'slack-schedule-message-request)
                     (lambda (_team _channel _text _post-at after-success)
                       (setq create-success after-success)))
                    ((symbol-function 'slack-delete-scheduled-message-request)
                     (lambda (_team _draft-id _updated after-success)
                       (setq delete-success after-success)))
                    ((symbol-function 'y-or-n-p) (lambda (&rest _args) t))
                    ((symbol-function 'message) #'ignore))
            (setq object (slack-scheduled-messages-show team)
                  emacs-buffer (oref object buf))
            (funcall request-success
                     :data
                     (slack-test-scheduled-response
                      (slack-test-scheduled-draft "D1" 10 "first")))
            (with-current-buffer emacs-buffer
              (slack-schedule-message channel-id "new draft" 5 team))
            (funcall create-success :data (list :ok t))
            (funcall request-success
                     :data
                     (slack-test-scheduled-response
                      (slack-test-scheduled-draft "D2" 20 "created")))
            (should (eq emacs-buffer (oref object buf)))
            (with-current-buffer emacs-buffer
              (goto-char (point-min))
              (should (search-forward "created" nil t))
              (let ((draft-position
                     (text-property-not-all
                      (point-min) (point-max) 'draft-id nil)))
                (should draft-position)
                (should (equal "D2"
                               (get-text-property draft-position 'draft-id)))
                (goto-char draft-position))
              (slack-scheduled-messages-delete-at-point))
            (should (functionp delete-success))
            (funcall delete-success :data (list :ok t))
            (funcall request-success
                     :data (slack-test-scheduled-response))
            (should (eq emacs-buffer (oref object buf)))
            (with-current-buffer emacs-buffer
              (goto-char (point-min))
              (should (search-forward "(No scheduled messages.)" nil t))))
        (when (buffer-live-p emacs-buffer)
          (kill-buffer emacs-buffer))
        (remhash (oref team id) slack-tokens-by-id)
        (remhash (oref team token) slack-teams-by-token)))))

(ert-deftest slack-test-scheduled-messages-mutation-follows-in-flight-list ()
  (slack-test-setup
    (let (request-successes create-success object emacs-buffer)
      (unwind-protect
          (cl-letf (((symbol-function 'slack-scheduled-messages--team)
                     (lambda (&optional _selected) team))
                    ((symbol-function 'slack-buffer-display) #'ignore)
                    ((symbol-function 'slack-list-scheduled-messages-request)
                     (lambda (_team after-success &optional _on-error)
                       (push after-success request-successes)))
                    ((symbol-function 'slack-schedule-message-request)
                     (lambda (_team _channel _text _post-at after-success)
                       (setq create-success after-success)))
                    ((symbol-function 'message) #'ignore))
            (setq object (slack-scheduled-messages-show team)
                  emacs-buffer (oref object buf))
            (with-current-buffer emacs-buffer
              (slack-schedule-message channel-id "new draft" 5 team))
            (funcall create-success :data (list :ok t))
            (should (= 1 (length request-successes)))
            (funcall (car request-successes)
                     :data
                     (slack-test-scheduled-response
                      (slack-test-scheduled-draft "stale" 10 "stale")))
            (should (= 2 (length request-successes)))
            (funcall (car request-successes)
                     :data
                     (slack-test-scheduled-response
                      (slack-test-scheduled-draft "fresh" 20 "fresh")))
            (should (eq emacs-buffer (oref object buf)))
            (with-current-buffer emacs-buffer
              (goto-char (point-min))
              (should (search-forward "fresh" nil t))
              (goto-char (point-min))
              (should-not (search-forward "stale" nil t))))
        (when (buffer-live-p emacs-buffer)
          (kill-buffer emacs-buffer))))))

(ert-deftest slack-test-scheduled-delete-result-does-not-resurrect-buffer ()
  (slack-test-setup
    (let ((requests 0))
      (oset team id "T11111")
      (oset team token "test-token")
      (puthash (oref team id) (oref team token) slack-tokens-by-id)
      (puthash (oref team token) team slack-teams-by-token)
      (let (request-success delete-success object emacs-buffer)
        (unwind-protect
            (cl-letf (((symbol-function 'slack-scheduled-messages--team)
                       (lambda (&optional _selected) team))
                      ((symbol-function 'slack-buffer-display) #'ignore)
                      ((symbol-function 'slack-list-scheduled-messages-request)
                       (lambda (_team after-success &optional _on-error)
                         (cl-incf requests)
                         (setq request-success after-success)))
                      ((symbol-function 'slack-delete-scheduled-message-request)
                       (lambda (_team _draft-id _updated after-success)
                         (setq delete-success after-success)))
                      ((symbol-function 'y-or-n-p) (lambda (&rest _args) t))
                      ((symbol-function 'message) #'ignore))
              (setq object (slack-scheduled-messages-show team)
                    emacs-buffer (oref object buf))
              (funcall request-success
                       :data
                       (slack-test-scheduled-response
                        (slack-test-scheduled-draft "D1" 10 "delete me")))
              (with-current-buffer emacs-buffer
                (goto-char
                 (text-property-not-all
                  (point-min) (point-max) 'draft-id nil))
                (slack-scheduled-messages-delete-at-point))
              (kill-buffer emacs-buffer)
              (funcall delete-success :data (list :ok t))
              (should (= 1 requests))
              (should-not
               (slack-buffer-find 'slack-scheduled-messages-buffer team)))
          (let ((cached
                 (slack-buffer-find 'slack-scheduled-messages-buffer team)))
            (when (and cached
                       (slot-boundp cached 'buf)
                       (buffer-live-p (oref cached buf)))
              (kill-buffer (oref cached buf))))
          (when (buffer-live-p emacs-buffer)
            (kill-buffer emacs-buffer))
          (remhash (oref team id) slack-tokens-by-id)
          (remhash (oref team token) slack-teams-by-token))))))

(ert-deftest slack-test-scheduled-messages-rejects-stale-generation ()
  (slack-test-setup
    (let (request-success object emacs-buffer)
      (unwind-protect
          (cl-letf (((symbol-function 'slack-scheduled-messages--team)
                     (lambda (&optional _selected) team))
                    ((symbol-function 'slack-buffer-display) #'ignore)
                    ((symbol-function 'slack-list-scheduled-messages-request)
                     (lambda (_team after-success &optional _on-error)
                       (setq request-success after-success))))
            (setq object (slack-scheduled-messages-show team)
                  emacs-buffer (oref object buf))
            (let* ((state (slack-team-page-state team 'scheduled-messages))
                   (new-generation (slack-page-state-restart state)))
              (funcall request-success
                       :data
                       (slack-test-scheduled-response
                        (slack-test-scheduled-draft "stale" 10 "stale")))
              (should (= new-generation (slack-page-state-generation state)))
              (should-not (slack-page-state-loaded-p state))
              (should-not (slack-page-state-value state))))
        (when (buffer-live-p emacs-buffer)
          (kill-buffer emacs-buffer))))))

(ert-deftest slack-test-scheduled-messages-result-does-not-resurrect-killed-buffer ()
  (slack-test-setup
    (let (request-success object emacs-buffer)
      (cl-letf (((symbol-function 'slack-scheduled-messages--team)
                 (lambda (&optional _selected) team))
                ((symbol-function 'slack-buffer-display) #'ignore)
                ((symbol-function 'slack-list-scheduled-messages-request)
                 (lambda (_team after-success &optional _on-error)
                   (setq request-success after-success))))
        (setq object (slack-scheduled-messages-show team)
              emacs-buffer (oref object buf))
        (kill-buffer emacs-buffer)
        (funcall request-success
                 :data
                 (slack-test-scheduled-response
                  (slack-test-scheduled-draft "D1" 10 "durable")))
        (should-not (buffer-live-p emacs-buffer))
        (should (equal "D1"
                       (oref (car (slack-page-state-value
                                   (slack-team-page-state
                                    team 'scheduled-messages)))
                             draft-id)))))))

(ert-deftest slack-test-scheduled-messages-result-skips-replacement-buffer ()
  (slack-test-setup
    (let (request-success object original replacement)
      (unwind-protect
          (cl-letf (((symbol-function 'slack-scheduled-messages--team)
                     (lambda (&optional _selected) team))
                    ((symbol-function 'slack-buffer-display) #'ignore)
                    ((symbol-function 'slack-list-scheduled-messages-request)
                     (lambda (_team after-success &optional _on-error)
                       (setq request-success after-success))))
            (setq object (slack-scheduled-messages-show team)
                  original (oref object buf)
                  replacement (generate-new-buffer
                               " *slack-scheduled-replacement*"))
            (oset object buf replacement)
            (with-current-buffer replacement
              (insert "replacement sentinel"))
            (funcall request-success
                     :data
                     (slack-test-scheduled-response
                      (slack-test-scheduled-draft "D1" 10 "new")))
            (with-current-buffer replacement
              (should (equal "replacement sentinel" (buffer-string)))))
        (when (buffer-live-p original)
          (kill-buffer original))
        (when (buffer-live-p replacement)
          (kill-buffer replacement))))))

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

(defun slack-test--all-thread-view (team room ts last-read &optional text)
  "Return a test thread view for TEAM in ROOM at TS and LAST-READ."
  (make-instance
   'slack-thread-view
   :root_msg
   (slack-message-create
    (list :type "message"
          :ts ts
          :last_read last-read
          :text (or text (format "thread %s" ts))
          :user "U11111"
          :channel (oref room id))
    team room)))

(ert-deftest slack-test-all-threads-displays-before-request-and-clears-on-primary ()
  (slack-test-setup
    (let (displayed primary emacs-buffer
          (clear-count 0)
          events)
      (unwind-protect
          (cl-letf (((symbol-function 'slack-team-select) (lambda () team))
                    ((symbol-function 'slack-buffer-display)
                     (lambda (object)
                       (setq displayed object
                             emacs-buffer (oref object buf))
                       (push 'display events)))
                    ((symbol-function 'slack-subscriptions-thread-clear-all)
                     (lambda (_team) (cl-incf clear-count)))
                    ((symbol-function 'slack-subscriptions-thread-get-view)
                     (lambda (_team _current-ts _after-success
                              &optional on-primary-page _on-error)
                       (setq primary on-primary-page)
                       (push 'request events))))
            (slack-all-threads)
            (should displayed)
            (should (equal '(request display) events))
            (should (= 0 clear-count))
            (funcall primary 0 0 nil nil)
            (should (= 1 clear-count))
            ;; A buggy duplicate primary callback must not repeat the side effect.
            (funcall primary 0 0 nil nil)
            (should (= 1 clear-count)))
        (when (buffer-live-p emacs-buffer)
          (kill-buffer emacs-buffer))))))

(ert-deftest slack-test-all-threads-primary-precedes-user-hydration-and-forwards-error ()
  (slack-test-setup
    (let (request-success request-error users-success events)
      (cl-letf (((symbol-function 'slack-request)
                 (lambda (request)
                   (setq request-success (oref request success)
                         request-error (oref request error))))
                ((symbol-function 'slack-team-missing-user-ids)
                 (lambda (&rest _args) '("U-missing")))
                ((symbol-function 'slack-users-info-request)
                 (lambda (_ids _team &rest args)
                   (setq users-success (plist-get args :after-success))
                   (push 'users events))))
        (slack-subscriptions-thread-get-view
         team nil
         (lambda (&rest _args) (push 'hydrated events))
         (lambda (&rest _args) (push 'primary events))
         (lambda (&rest _args) (push 'error events)))
        (funcall request-success
                 :data (list :ok t
                             :total_unread_replies 0
                             :new_threads_count 0
                             :threads nil
                             :has_more :json-false))
        (should (equal '(users primary) events))
        (funcall users-success)
        (should (equal '(hydrated users primary) events))
        (funcall request-error "transport failure")
        (should (equal '(error hydrated users primary) events))))))

(ert-deftest slack-test-all-threads-now-ts-does-not-assume-float-precision ()
  "All Threads timestamps remain valid when Emacs prints only three decimals."
  (cl-letf (((symbol-function 'time-to-seconds)
             (lambda (&optional _time) 1785703455.607)))
    (should (equal "1785703455.6070" (slack-all-threads--now-ts)))))

(ert-deftest slack-test-all-threads-primary-renders-uncached-author ()
  "The primary page renders a stable author ID before user hydration."
  (slack-test-setup
    (let (emacs-buffer request-success users-success
          (clear-count 0))
      (unwind-protect
          (cl-letf (((symbol-function 'slack-team-select) (lambda () team))
                    ((symbol-function 'slack-buffer-display)
                     (lambda (object) (setq emacs-buffer (oref object buf))))
                    ((symbol-function 'slack-request)
                     (lambda (request)
                       (setq request-success (oref request success))))
                    ((symbol-function 'slack-users-info-request)
                     (lambda (_ids _team &rest args)
                       (setq users-success (plist-get args :after-success))))
                    ((symbol-function 'slack-subscriptions-thread-clear-all)
                     (lambda (_team) (cl-incf clear-count))))
            (slack-all-threads)
            (funcall
             request-success
             :data
             (list
              :ok t
              :total_unread_replies 1
              :new_threads_count 1
              :threads
              (list
               (list
                :root_msg
                (list :type "message"
                      :ts "2.000"
                      :last_read "1.500"
                      :text "uncached author message"
                      :user "U-missing"
                      :channel channel-id)
                :latest_replies nil
                :unread_replies nil))
              :has_more :json-false))
            (should (functionp users-success))
            (should (= 1 clear-count))
            (with-current-buffer emacs-buffer
              (goto-char (point-min))
              (should (search-forward "U-missing" nil t))
              (should (search-forward "uncached author message" nil t))))
        (when (buffer-live-p emacs-buffer)
          (kill-buffer emacs-buffer))))))

(ert-deftest slack-test-all-threads-clear-follows-successful-primary-render ()
  "Unread state is not cleared when the accepted primary page cannot render."
  (slack-test-setup
    (let ((original-render
           (symbol-function
            'slack-all-threads-buffer--replace-live-contents))
          emacs-buffer request-primary
          (clear-count 0))
      (unwind-protect
          (cl-letf (((symbol-function 'slack-team-select) (lambda () team))
                    ((symbol-function 'slack-buffer-display)
                     (lambda (object) (setq emacs-buffer (oref object buf))))
                    ((symbol-function 'slack-subscriptions-thread-get-view)
                     (lambda (_team _current-ts _after-success
                              &optional on-primary-page _on-error)
                       (setq request-primary on-primary-page)))
                    ((symbol-function
                      'slack-all-threads-buffer--replace-live-contents)
                     (lambda (object state)
                       (if (= (slack-page-state-generation state)
                              (slack-page-state-committed-generation state))
                           (error "Primary renderer boom")
                         (funcall original-render object state))))
                    ((symbol-function 'slack-subscriptions-thread-clear-all)
                     (lambda (_team) (cl-incf clear-count)))
                    ((symbol-function 'display-warning) #'ignore))
            (slack-all-threads)
            (funcall request-primary 0 0 nil nil)
            (should (= 0 clear-count)))
        (when (buffer-live-p emacs-buffer)
          (kill-buffer emacs-buffer))))))

(ert-deftest slack-test-all-threads-normalization-error-fails-once ()
  "Malformed success data reaches the request error continuation once."
  (slack-test-setup
    (let (request-success request-error terminal-error
          (error-count 0))
      (cl-letf (((symbol-function 'slack-request)
                 (lambda (request)
                   (setq request-success (oref request success)
                         request-error (oref request error)))))
        (slack-subscriptions-thread-get-view
         team nil nil nil
         (lambda (&rest errors)
           (cl-incf error-count)
           (setq terminal-error errors)))
        (funcall request-success
                 :data (list :ok t :threads (list (list :root_msg nil))))
        (funcall request-error "late transport failure")
        (should (= 1 error-count))
        (should terminal-error)))))

(ert-deftest slack-test-all-threads-refresh-normalization-error-keeps-stale-page ()
  "Malformed refresh data preserves the stale page and exposes retry."
  (slack-test-setup
    (let* ((thread (slack-test--all-thread-view
                    team channel "2.000" "1.500" "stale thread"))
           (state (slack-team-page-state team 'all-threads))
           (value (slack-all-threads--page-value
                   1 1 (list thread) nil "1.500"))
           emacs-buffer request-success)
      (slack-page-state-store state value "1.500" nil)
      (unwind-protect
          (cl-letf (((symbol-function 'slack-team-select) (lambda () team))
                    ((symbol-function 'slack-buffer-display)
                     (lambda (object) (setq emacs-buffer (oref object buf))))
                    ((symbol-function 'slack-request)
                     (lambda (request)
                       (setq request-success (oref request success))))
                    ((symbol-function 'slack-subscriptions-thread-clear-all)
                     #'ignore))
            (slack-all-threads)
            (funcall request-success
                     :data (list :ok t
                                 :threads (list (list :root_msg nil))))
            (should (eq 'failed (slack-page-state-status state)))
            (should (eq value (slack-page-state-value state)))
            (with-current-buffer emacs-buffer
              (goto-char (point-min))
              (should (search-forward "stale thread" nil t))
              (goto-char (point-min))
              (should (search-forward "Slack request failed" nil t))
              (should (search-forward "Retry" nil t))))
        (when (buffer-live-p emacs-buffer)
          (kill-buffer emacs-buffer))))))

(ert-deftest slack-test-all-threads-empty-failure-retries-in-place ()
  (slack-test-setup
    (let (object emacs-buffer request-primary request-hydrated request-error retry
          (request-count 0))
      (unwind-protect
          (cl-letf (((symbol-function 'slack-team-select) (lambda () team))
                    ((symbol-function 'slack-buffer-display)
                     (lambda (displayed)
                       (setq object displayed
                             emacs-buffer (oref displayed buf))))
                    ((symbol-function 'slack-subscriptions-thread-clear-all)
                     #'ignore)
                    ((symbol-function 'slack-subscriptions-thread-get-view)
                     (lambda (_team _current-ts after-success
                              &optional on-primary-page on-error)
                       (cl-incf request-count)
                       (setq request-primary on-primary-page
                             request-hydrated after-success
                             request-error on-error))))
            (slack-all-threads)
            (setq retry
                  (buffer-local-value
                   'slack-buffer-page-retry-function emacs-buffer))
            (funcall request-error "rate_limited")
            (should (eq 'failed
                        (slack-page-state-status
                         (slack-team-page-state team 'all-threads))))
            (with-current-buffer emacs-buffer
              (goto-char (point-min))
              (should (search-forward "Slack request failed: rate_limited" nil t))
              (should (search-forward "Retry" nil t)))
            (funcall retry)
            (should (= 2 request-count))
            (should (eq emacs-buffer (oref object buf)))
            (funcall request-primary 0 0 nil nil)
            (funcall request-hydrated 0 0 nil nil)
            (should (eq 'ready
                        (slack-page-state-status
                         (slack-team-page-state team 'all-threads))))
            (with-current-buffer emacs-buffer
              (goto-char (point-min))
              (should (search-forward "No threads." nil t))
              (goto-char (point-min))
              (should-not (search-forward "Slack request failed" nil t))))
        (when (buffer-live-p emacs-buffer)
          (kill-buffer emacs-buffer))))))

(ert-deftest slack-test-all-threads-refresh-keeps-buffer-and-durable-page ()
  (slack-test-setup
    (let* ((thread (slack-test--all-thread-view
                    team channel "2.000" "1.500" "durable thread"))
           object emacs-buffer request-primary request-hydrated
           (clear-count 0))
      (unwind-protect
          (cl-letf (((symbol-function 'slack-team-select) (lambda () team))
                    ((symbol-function 'slack-buffer-display)
                     (lambda (displayed)
                       (setq object displayed
                             emacs-buffer (oref displayed buf))))
                    ((symbol-function 'slack-subscriptions-thread-clear-all)
                     (lambda (_team) (cl-incf clear-count)))
                    ((symbol-function 'slack-subscriptions-thread-get-view)
                     (lambda (_team _current-ts after-success
                              &optional on-primary-page _on-error)
                       (setq request-primary on-primary-page
                             request-hydrated after-success))))
            (slack-all-threads)
            (funcall request-primary 1 1 (list thread) t)
            (funcall request-hydrated 1 1 (list thread) t)
            (let* ((state (slack-team-page-state team 'all-threads))
                   (value (slack-page-state-value state)))
              (should (equal (list thread) (plist-get value :threads)))
              (should (equal "1.500" (plist-get value :current-ts)))
              (should (equal "1.500"
                             (slack-page-state-continuation state)))
              (should (slack-page-state-has-more state)))
            (let ((first-buffer emacs-buffer))
              (slack-all-threads)
              (should (eq first-buffer emacs-buffer))
              (should (buffer-live-p first-buffer)))
            (should (= 1 clear-count)))
        (when (buffer-live-p emacs-buffer)
          (kill-buffer emacs-buffer))))))

(ert-deftest slack-test-all-threads-late-hydration-does-not-recreate-killed-buffer ()
  (slack-test-setup
    (let (object emacs-buffer request-primary request-hydrated)
      (cl-letf (((symbol-function 'slack-team-select) (lambda () team))
                ((symbol-function 'slack-buffer-display)
                 (lambda (displayed)
                   (setq object displayed
                         emacs-buffer (oref displayed buf))))
                ((symbol-function 'slack-subscriptions-thread-clear-all)
                 #'ignore)
                ((symbol-function 'slack-subscriptions-thread-get-view)
                 (lambda (_team _current-ts after-success
                          &optional on-primary-page _on-error)
                   (setq request-primary on-primary-page
                         request-hydrated after-success))))
        (slack-all-threads)
        (funcall request-primary 0 0 nil nil)
        (kill-buffer emacs-buffer)
        (let ((buffers-after-kill (buffer-list)))
          (funcall request-hydrated 0 0 nil nil)
          (should-not (buffer-live-p emacs-buffer))
          (should (equal buffers-after-kill (buffer-list)))
          (should (eq 'ready
                      (slack-page-state-status
                       (slack-team-page-state team 'all-threads)))))))))

(ert-deftest slack-test-all-threads-load-more-commits-durable-page ()
  (slack-test-setup
    (let* ((old-thread (slack-test--all-thread-view
                        team channel "3.000" "2.500" "old thread"))
           (new-thread (slack-test--all-thread-view
                        team channel "2.000" "1.500" "new thread"))
           (state (slack-team-page-state team 'all-threads))
           (old-value (list :total-unread-replies 1
                            :new-threads-count 1
                            :threads (list old-thread)
                            :has-more t
                            :current-ts "2.500"))
           (object (slack-create-all-threads-buffer
                    team 1 1 (list old-thread) t "2.500"))
           (emacs-buffer (slack-buffer-buffer object))
           request-primary request-hydrated requested-ts)
      (slack-page-state-store state old-value "2.500" t)
      (unwind-protect
          (cl-letf (((symbol-function 'slack-subscriptions-thread-get-view)
                     (lambda (_team current-ts after-success
                              &optional on-primary-page _on-error)
                       (setq requested-ts current-ts
                             request-primary on-primary-page
                             request-hydrated after-success))))
            (with-current-buffer emacs-buffer
              (slack-buffer-load-more object)
              (should slack-buffer--loading-more-p))
            (should (equal "2.500" requested-ts))
            (funcall request-primary 0 0 (list new-thread) nil)
            (let ((value (slack-page-state-value state)))
              (should (equal (list old-thread new-thread)
                             (plist-get value :threads)))
              (should (equal "1.500" (plist-get value :current-ts)))
              (should-not (slack-page-state-has-more state)))
            (funcall request-hydrated 0 0 (list new-thread) nil)
            (with-current-buffer emacs-buffer
              (should-not slack-buffer--loading-more-p)
              (goto-char (point-min))
              (should (search-forward "new thread" nil t))))
        (when (buffer-live-p emacs-buffer)
          (kill-buffer emacs-buffer))))))

(ert-deftest slack-test-all-threads-load-more-rejects-stale-generation ()
  (slack-test-setup
    (let* ((old-thread (slack-test--all-thread-view
                        team channel "4.000" "3.500"))
           (page-thread (slack-test--all-thread-view
                         team channel "3.000" "2.500"))
           (newer-thread (slack-test--all-thread-view
                          team channel "5.000" "4.500"))
           (old-value (slack-all-threads--page-value
                       1 1 (list old-thread) t "3.500"))
           (newer-value (slack-all-threads--page-value
                         2 2 (list newer-thread) t "4.500"))
           (state (slack-team-page-state team 'all-threads))
           (object (slack-create-all-threads-buffer
                    team 1 1 (list old-thread) t "3.500"))
           (emacs-buffer (slack-buffer-buffer object))
           request-primary request-hydrated)
      (slack-page-state-store state old-value "3.500" t)
      (unwind-protect
          (cl-letf (((symbol-function 'slack-subscriptions-thread-get-view)
                     (lambda (_team _current-ts after-success
                              &optional on-primary-page _on-error)
                       (setq request-primary on-primary-page
                             request-hydrated after-success))))
            (with-current-buffer emacs-buffer
              (slack-buffer-load-more object)
              (should slack-buffer--loading-more-p))
            (slack-page-state-store state newer-value "4.500" t)
            (funcall request-primary 0 0 (list page-thread) nil)
            (funcall request-hydrated 0 0 (list page-thread) nil)
            (should (eq newer-value (slack-page-state-value state)))
            (should (equal (list newer-thread)
                           (plist-get (slack-page-state-value state)
                                      :threads)))
            (should (equal "4.500"
                           (slack-page-state-continuation state)))
            (with-current-buffer emacs-buffer
              (should-not slack-buffer--loading-more-p)))
        (when (buffer-live-p emacs-buffer)
          (kill-buffer emacs-buffer))))))

(ert-deftest slack-test-all-threads-load-more-resets-on-error ()
  (slack-test-setup
    (let* ((thread (slack-test--all-thread-view
                    team channel "3.000" "2.500"))
           (value (slack-all-threads--page-value
                   1 1 (list thread) t "2.500"))
           (state (slack-team-page-state team 'all-threads))
           (object (slack-create-all-threads-buffer
                    team 1 1 (list thread) t "2.500"))
           (emacs-buffer (slack-buffer-buffer object))
           request-error)
      (slack-page-state-store state value "2.500" t)
      (unwind-protect
          (cl-letf (((symbol-function 'slack-subscriptions-thread-get-view)
                     (lambda (_team _current-ts _after-success
                              &optional _on-primary-page on-error)
                       (setq request-error on-error))))
            (with-current-buffer emacs-buffer
              (slack-buffer-load-more object)
              (should slack-buffer--loading-more-p))
            (funcall request-error "rate_limited")
            (should (eq value (slack-page-state-value state)))
            (with-current-buffer emacs-buffer
              (should-not slack-buffer--loading-more-p)))
        (when (buffer-live-p emacs-buffer)
          (kill-buffer emacs-buffer))))))

(ert-deftest slack-test-all-threads-load-more-skips-replacement-buffer ()
  (slack-test-setup
    (let* ((old-thread (slack-test--all-thread-view
                        team channel "3.000" "2.500"))
           (new-thread (slack-test--all-thread-view
                        team channel "2.000" "1.500"))
           (old-value (slack-all-threads--page-value
                       1 1 (list old-thread) t "2.500"))
           (state (slack-team-page-state team 'all-threads))
           (object (slack-create-all-threads-buffer
                    team 1 1 (list old-thread) t "2.500"))
           (original (slack-buffer-buffer object))
           (replacement (generate-new-buffer
                         " *slack-test-all-threads-replacement*"))
           request-primary request-hydrated)
      (slack-page-state-store state old-value "2.500" t)
      (unwind-protect
          (cl-letf (((symbol-function 'slack-subscriptions-thread-get-view)
                     (lambda (_team _current-ts after-success
                              &optional on-primary-page _on-error)
                       (setq request-primary on-primary-page
                             request-hydrated after-success))))
            (with-current-buffer original
              (slack-buffer-load-more object)
              (should slack-buffer--loading-more-p))
            (oset object buf replacement)
            (with-current-buffer replacement
              (insert "replacement sentinel"))
            (funcall request-primary 0 0 (list new-thread) nil)
            (funcall request-hydrated 0 0 (list new-thread) nil)
            (should (equal (list old-thread new-thread)
                           (plist-get (slack-page-state-value state)
                                      :threads)))
            (with-current-buffer replacement
              (should (equal "replacement sentinel" (buffer-string))))
            (with-current-buffer original
              (should-not slack-buffer--loading-more-p)))
        (when (buffer-live-p original)
          (kill-buffer original))
        (when (buffer-live-p replacement)
          (kill-buffer replacement))))))

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

(ert-deftest slack-test-file-detail-cold-open-displays-before-request ()
  (slack-test-setup
    (let* ((source (slack-create-message-buffer channel "" team))
           (full (slack-test-file-detail "F11111" "Full file"))
           events success object emacs-buffer)
      (unwind-protect
          (cl-letf (((symbol-function 'slack-buffer-display)
                     (lambda (displayed)
                       (setq object displayed
                             emacs-buffer (oref displayed buf))
                       (push 'display events)))
                    ((symbol-function 'slack-file-request-info)
                     (lambda (file-id page request-team
                                      &optional after-success _on-error
                                      _accept-result)
                       (should (equal "F11111" file-id))
                       (should (= 1 page))
                       (should (eq team request-team))
                       (setq success after-success)
                       (push 'request events))))
            (let ((result (slack-buffer-display-file source "F11111")))
              (should (eq object result)))
            (should (equal '(display request) (nreverse events)))
            (should (string-match-p "F11111" (buffer-name emacs-buffer)))
            (with-current-buffer emacs-buffer
              (goto-char (point-min))
              (should (search-forward "Loading Slack data" nil t)))
            (funcall success full team)
            (let ((state (slack-team-page-state team '(file-info "F11111"))))
              (should (eq 'ready (slack-page-state-status state)))
              (should (eq full (slack-page-state-value state))))
            (should (eq object
                        (slack-buffer-find
                         'slack-file-info-buffer team "F11111")))
            (should (eq emacs-buffer (oref object buf)))
            (with-current-buffer emacs-buffer
              (goto-char (point-min))
              (should (search-forward "Full file" nil t))
              (goto-char (point-min))
              (should-not (search-forward "Loading Slack data" nil t))))
        (when (buffer-live-p emacs-buffer)
          (kill-buffer emacs-buffer))))))

(ert-deftest slack-test-file-detail-renders-cached-summary-before-refresh ()
  (slack-test-setup
    (let* ((source (slack-create-message-buffer channel "" team))
           (summary (slack-test-file-detail "F11111" "Cached summary"))
           (full (slack-test-file-detail "F11111" "Hydrated detail"))
           success object emacs-buffer)
      (slack-file-pushnew summary team)
      (unwind-protect
          (cl-letf (((symbol-function 'slack-buffer-display) #'ignore)
                    ((symbol-function 'slack-file-request-info)
                     (lambda (_file-id _page _team
                                      &optional after-success _on-error
                                      _accept-result)
                       (setq success after-success))))
            (setq object (slack-buffer-display-file source "F11111")
                  emacs-buffer (oref object buf))
            (with-current-buffer emacs-buffer
              (goto-char (point-min))
              (should (search-forward "Cached summary" nil t))
              (goto-char (point-min))
              (should (search-forward "Refreshing Slack data" nil t)))
            (funcall success full team)
            (should (eq emacs-buffer (oref object buf)))
            (with-current-buffer emacs-buffer
              (goto-char (point-min))
              (should (search-forward "Hydrated detail" nil t))
              (goto-char (point-min))
              (should-not (search-forward "Cached summary" nil t))))
        (when (buffer-live-p emacs-buffer)
          (kill-buffer emacs-buffer))))))

(ert-deftest slack-test-file-detail-failure-preserves-summary-and-retries ()
  (slack-test-setup
    (let* ((source (slack-create-message-buffer channel "" team))
           (summary (slack-test-file-detail "F11111" "Cached summary"))
           (requests 0)
           on-error object emacs-buffer retry)
      (slack-file-pushnew summary team)
      (unwind-protect
          (cl-letf (((symbol-function 'slack-buffer-display) #'ignore)
                    ((symbol-function 'slack-file-request-info)
                     (lambda (_file-id _page _team
                                      &optional _after-success error
                                      _accept-result)
                       (cl-incf requests)
                       (setq on-error error))))
            (setq object (slack-buffer-display-file source "F11111")
                  emacs-buffer (oref object buf))
            (funcall on-error "offline")
            (let ((state (slack-team-page-state team '(file-info "F11111"))))
              (should (eq 'failed (slack-page-state-status state)))
              (should (eq summary (slack-page-state-value state))))
            (with-current-buffer emacs-buffer
              (goto-char (point-min))
              (should (search-forward "Cached summary" nil t))
              (goto-char (point-min))
              (should (search-forward "offline" nil t))
              (setq retry slack-buffer-page-retry-function))
            (funcall retry)
            (should (= 2 requests))
            (should (eq emacs-buffer (oref object buf))))
        (when (buffer-live-p emacs-buffer)
          (kill-buffer emacs-buffer))))))

(ert-deftest slack-test-file-detail-late-result-does-not-recreate-killed-buffer ()
  (slack-test-setup
    (let* ((source (slack-create-message-buffer channel "" team))
           (full (slack-test-file-detail "F11111" "Hydrated detail"))
           success object emacs-buffer)
      (cl-letf (((symbol-function 'slack-buffer-display) #'ignore)
                ((symbol-function 'slack-file-request-info)
                 (lambda (_file-id _page _team
                                  &optional after-success _on-error
                                  _accept-result)
                   (setq success after-success))))
        (setq object (slack-buffer-display-file source "F11111")
              emacs-buffer (oref object buf))
        (kill-buffer emacs-buffer)
        (funcall success full team)
        (should-not (buffer-live-p emacs-buffer))
        (should-not (slack-buffer-find
                     'slack-file-info-buffer team "F11111"))
        (should (eq full
                    (slack-page-state-value
                     (slack-team-page-state team '(file-info "F11111")))))))))

(ert-deftest slack-test-file-info-request-routes-every-failure ()
  (slack-test-setup
    (dolist (failure '(api transport normalization))
      (let (request reported success-called)
        (cl-letf (((symbol-function 'slack-request)
                   (lambda (created &rest _)
                     (setq request created))))
          (slack-file-request-info
           "F11111" 1 team
           (lambda (&rest _) (setq success-called t))
           (lambda (&rest errors) (push errors reported)))
          (pcase failure
            ('api
             (funcall (oref request success)
                      :data '(:ok :json-false :error "invalid_auth")))
            ('transport
             (funcall (oref request error)
                      :error-thrown '(error "offline")
                      :symbol-status 'error))
            ('normalization
             (funcall (oref request success)
                      :data '(:ok t :file (:id "F11111" :mimetype 4)))))
          (should (= 1 (length reported)))
          (should-not success-called))))))

(ert-deftest slack-test-file-info-request-hydrates-cached-object-in-place ()
  (slack-test-setup
    (let ((summary (slack-test-file-detail "F11111" "Cached summary"))
          request returned)
      (slack-file-pushnew summary team)
      (cl-letf (((symbol-function 'slack-request)
                 (lambda (created &rest _)
                   (setq request created))))
        (slack-file-request-info
         "F11111" 1 team (lambda (file &rest _) (setq returned file)))
        (funcall
         (oref request success)
         :data
         '(:ok t
           :file (:id "F11111" :created 1710000000
                  :name "Hydrated detail" :title "Hydrated detail"
                  :size 2000 :public :json-false :filetype "text"
                  :mimetype "text/plain" :pretty_type "Plain Text"
                  :user "U11111" :preview "complete"
                  :permalink "https://example.test/files/F11111"
                  :username "TestUser" :page 1
                  :url_private "https://example.test/files/F11111/view"
                  :url_private_download "")
           :comments nil))
        (should (eq summary returned))
        (should (eq summary (slack-file-find "F11111" team)))
        (should (equal "Hydrated detail" (oref summary title)))
        (should (equal "complete" (oref summary preview)))))))

(ert-deftest slack-test-file-detail-event-replacement-uses-file-id-key ()
  (slack-test-setup
    (let* ((summary (slack-test-file-detail "F11111" "Cached summary"))
           (updated (slack-test-file-detail "F11111" "Event update"))
           (state (slack-team-page-state team '(file-info "F11111")))
           (object (slack-create-file-info-buffer team "F11111" summary))
           (emacs-buffer (slack-buffer-buffer object)))
      (slack-page-state-store state summary nil nil)
      (unwind-protect
          (progn
            (slack-message-replace-buffer updated team)
            (should (eq updated (oref object file)))
            (should (eq updated (slack-page-state-value state)))
            (with-current-buffer emacs-buffer
              (goto-char (point-min))
              (should (search-forward "Event update" nil t))))
        (when (buffer-live-p emacs-buffer)
          (kill-buffer emacs-buffer))))))

(ert-deftest slack-test-file-detail-event-replacement-supersedes-refresh ()
  (slack-test-setup
    (let* ((summary (slack-test-file-detail "F11111" "Cached summary"))
           (updated (slack-test-file-detail "F11111" "Event update"))
           (state (slack-team-page-state team '(file-info "F11111")))
           (object (slack-create-file-info-buffer team "F11111" summary))
           (emacs-buffer (slack-buffer-buffer object)))
      (slack-page-state-store state summary nil nil)
      (slack-page-state-begin state t)
      (unwind-protect
          (progn
            (slack-message-replace-buffer updated team)
            (should (eq 'ready (slack-page-state-status state)))
            (should (eq updated (oref object file)))
            (should (eq updated (slack-page-state-value state)))
            (with-current-buffer emacs-buffer
              (goto-char (point-min))
              (should (search-forward "Event update" nil t))))
        (when (buffer-live-p emacs-buffer)
          (kill-buffer emacs-buffer))))))

(ert-deftest slack-test-file-detail-stale-result-does-not-mutate-current-cache ()
  (slack-test-setup
    (let* ((summary (slack-test-file-detail "F11111" "Cached summary"))
           (newer (slack-test-file-detail "F11111" "Newer detail"))
           (state (slack-team-page-state team '(file-info "F11111")))
           request object emacs-buffer)
      (slack-file-pushnew summary team)
      (unwind-protect
          (cl-letf (((symbol-function 'slack-buffer-display) #'ignore)
                    ((symbol-function 'slack-request)
                     (lambda (created &rest _ignored)
                       (setq request created))))
            (setq object (slack-file-info-buffer--present
                          team "F11111" 1 t)
                  emacs-buffer (oref object buf))
            (puthash "F11111" newer (oref team files))
            (slack-page-state-store state newer nil nil)
            (funcall
             (oref request success)
             :data
             '(:ok t
               :file (:id "F11111" :created 1710000000
                      :name "Stale detail" :title "Stale detail"
                      :size 2000 :public :json-false :filetype "text"
                      :mimetype "text/plain" :pretty_type "Plain Text"
                      :user "U11111" :preview "stale"
                      :permalink "https://example.test/files/F11111"
                      :username "TestUser" :page 1
                      :url_private "https://example.test/files/F11111/view"
                      :url_private_download "")
               :comments nil))
            (should (eq newer (slack-file-find "F11111" team)))
            (should (eq newer (slack-page-state-value state)))
            (should (equal "Newer detail" (oref newer title))))
        (when (buffer-live-p emacs-buffer)
          (kill-buffer emacs-buffer))))))

(ert-deftest slack-test-file-detail-cache-replacement-finishes-current-load ()
  (slack-test-setup
    (let* ((summary (slack-test-file-detail "F11111" "Cached summary"))
           (newer (slack-test-file-detail "F11111" "Newer detail"))
           (state (slack-team-page-state team '(file-info "F11111")))
           request object emacs-buffer)
      (slack-file-pushnew summary team)
      (unwind-protect
          (cl-letf (((symbol-function 'slack-buffer-display) #'ignore)
                    ((symbol-function 'slack-request)
                     (lambda (created &rest _ignored)
                       (setq request created))))
            (setq object (slack-file-info-buffer--present
                          team "F11111" 1 t)
                  emacs-buffer (oref object buf))
            (puthash "F11111" newer (oref team files))
            (funcall
             (oref request success)
             :data
             '(:ok t
               :file (:id "F11111" :created 1710000000
                      :name "Stale detail" :title "Stale detail"
                      :size 2000 :public :json-false :filetype "text"
                      :mimetype "text/plain" :pretty_type "Plain Text"
                      :user "U11111" :preview "stale"
                      :permalink "https://example.test/files/F11111"
                      :username "TestUser" :page 1
                      :url_private "https://example.test/files/F11111/view"
                      :url_private_download "")
               :comments nil))
            (should (eq 'ready (slack-page-state-status state)))
            (should (eq newer (slack-page-state-value state)))
            (should (eq newer (slack-file-find "F11111" team)))
            (should (equal "Newer detail" (oref newer title))))
        (when (buffer-live-p emacs-buffer)
          (kill-buffer emacs-buffer))))))

(ert-deftest slack-test-file-detail-refresh-reconciles-live-star-event ()
  (slack-test-setup
    (let* ((summary (slack-test-file-detail "F11111" "Cached summary"))
           (state (slack-team-page-state team '(file-info "F11111")))
           request object emacs-buffer)
      (slack-file-pushnew summary team)
      (unwind-protect
          (cl-letf (((symbol-function 'slack-buffer-display) #'ignore)
                    ((symbol-function 'slack-request)
                     (lambda (created &rest _ignored)
                       (setq request created))))
            (setq object (slack-file-info-buffer--present
                          team "F11111" 1 t)
                  emacs-buffer (oref object buf))
            (slack-message-star-added summary)
            (slack-message-replace-buffer summary team)
            (funcall
             (oref request success)
             :data
             '(:ok t
               :file (:id "F11111" :created 1710000000
                      :name "Hydrated detail" :title "Hydrated detail"
                      :size 2000 :public :json-false :filetype "text"
                      :mimetype "text/plain" :pretty_type "Plain Text"
                      :user "U11111" :preview "complete"
                      :is_starred :json-false
                      :permalink "https://example.test/files/F11111"
                      :username "TestUser" :page 1
                      :url_private "https://example.test/files/F11111/view"
                      :url_private_download "")
               :comments nil))
            (should (eq 'ready (slack-page-state-status state)))
            (should (equal "Hydrated detail" (oref summary title)))
            (should (oref summary is-starred))
            (with-current-buffer emacs-buffer
              (goto-char (point-min))
              (should (search-forward "Hydrated detail" nil t))))
        (when (buffer-live-p emacs-buffer)
          (kill-buffer emacs-buffer))))))

(ert-deftest slack-test-file-info-request-does-not-reclassify-callback-errors ()
  (slack-test-setup
    (let ((data
           '(:ok t
             :file (:id "F11111" :created 1710000000
                    :name "Detail" :title "Detail"
                    :size 2000 :public :json-false :filetype "text"
                    :mimetype "text/plain" :pretty_type "Plain Text"
                    :user "U11111" :preview "complete"
                    :permalink "https://example.test/files/F11111"
                    :username "TestUser" :page 1
                    :url_private "https://example.test/files/F11111/view"
                    :url_private_download "")
             :comments nil)))
      (let (request (error-count 0))
        (cl-letf (((symbol-function 'slack-request)
                   (lambda (created &rest _ignored)
                     (setq request created))))
          (slack-file-request-info
           "F11111" 1 team
           (lambda (&rest _ignored) (error "consumer failed"))
           (lambda (&rest _ignored) (cl-incf error-count)))
          (should-error (funcall (oref request success) :data data))
          (should (= 0 error-count))))
      (let (request (error-count 0))
        (cl-letf (((symbol-function 'slack-request)
                   (lambda (created &rest _ignored)
                     (setq request created))))
          (slack-file-request-info
           "F11111" 1 team #'ignore
           (lambda (&rest _ignored)
             (cl-incf error-count)
             (error "error consumer failed")))
          (should-error
           (funcall (oref request success)
                    :data '(:ok :json-false :error "invalid_auth")))
          (should (= 1 error-count)))))))

(ert-deftest slack-test-file-update-refreshes-same-file-buffer ()
  (slack-test-setup
    (let* ((file (slack-test-file-detail "F11111" "Initial detail" 3))
           (state (slack-team-page-state team '(file-info "F11111")))
           (object (slack-create-file-info-buffer team "F11111" file))
           (emacs-buffer (slack-buffer-buffer object))
           captured-id captured-page)
      (slack-page-state-store state file nil nil)
      (unwind-protect
          (cl-letf (((symbol-function 'slack-buffer-display) #'ignore)
                    ((symbol-function 'slack-file-request-info)
                     (lambda (file-id page _team
                                      &optional _after-success _on-error
                                      _accept-result)
                       (setq captured-id file-id
                             captured-page page))))
            (let ((slack-current-buffer object))
              (should (eq object (slack-file-update))))
            (should (equal "F11111" captured-id))
            (should (= 3 captured-page))
            (should (eq emacs-buffer (oref object buf)))
            (should (eq 'refreshing (slack-page-state-status state))))
        (when (buffer-live-p emacs-buffer)
          (kill-buffer emacs-buffer))))))

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
      (unwind-protect
          (progn
            (cl-letf (((symbol-function 'slack-list-scheduled-messages-request)
                       (lambda (_team cb &optional _on-error)
                         (funcall cb :data
                                  (list :ok t
                                        :drafts
                                        (list (list :id "D1"
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
            (should (eq 1 (length (oref displayed messages)))))
        (when (and displayed (buffer-live-p (oref displayed buf)))
          (kill-buffer (oref displayed buf)))))))

(ert-deftest slack-test-scheduled-messages-surface-api-errors ()
  (slack-test-setup
    (let ((displayed nil))
      (unwind-protect
          (progn
            (cl-letf (((symbol-function 'slack-list-scheduled-messages-request)
                       (lambda (_team cb &optional _on-error)
                         (funcall cb :data (list :ok :json-false
                                                 :error "invalid_auth"))))
                      ((symbol-function 'slack-buffer-display)
                       (lambda (buf) (setq displayed buf)))
                      ((symbol-function 'slack-scheduled-messages--team)
                       (lambda (&optional _t) team)))
              (slack-scheduled-messages-show team))
            (should displayed)
            (should (eq 'failed
                        (slack-page-state-status
                         (slack-team-page-state team 'scheduled-messages))))
            (with-current-buffer (oref displayed buf)
              (goto-char (point-min))
              (should (search-forward "invalid_auth" nil t))))
        (when (and displayed (buffer-live-p (oref displayed buf)))
          (kill-buffer (oref displayed buf)))))))

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

(ert-deftest slack-test-activity-feed-load-more-updates-page-state ()
  (slack-test-setup
    (let* ((old-activity
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
           (key (slack-activity-feed--page-key nil))
           (state (slack-team-page-state team key))
           (object
            (make-instance
             'slack-activity-feed-buffer
             :team-id (oref team id)
             :room-id "__activity-feed__"
             :cached-team team
             :page-key key
             :activity-feed
             (make-instance 'slack-activity-feed
                            :activities (list old-activity)
                            :pagination "cursor-1"
                            :last nil)))
           (buffer (generate-new-buffer " *slack-test-feed-load-more*"))
           request-success
           request-error
           requested-cursor
           room-success
           state-count-at-room
           (insertions 0))
      (slack-page-state-store
       state
       (list :activities (list old-activity)
             :pagination "cursor-1"
             :updated-at (current-time))
       "cursor-1" t)
      (slack-buffer-cache-team object team)
      (oset object buf buffer)
      (unwind-protect
          (cl-letf (((symbol-function 'slack-activity-feed-request)
                     (lambda (_team success &optional cursor on-error)
                       (setq request-success success
                             request-error on-error
                             requested-cursor cursor)))
                    ((symbol-function 'slack-activity-feed--prefetch-rooms)
                     (lambda (_activities _team callback)
                       (setq state-count-at-room
                             (length
                              (plist-get (slack-page-state-value state)
                                         :activities))
                             room-success callback)))
                    ((symbol-function 'slack-activity-feed--prefetch-messages)
                     #'ignore)
                    ((symbol-function 'slack-buffer-insert--history)
                     (lambda (_buffer) (cl-incf insertions))))
            (with-current-buffer buffer
              (slack-buffer-load-more object)
              (should slack-buffer--loading-more-p))
            (should (equal "cursor-1" requested-cursor))
            (should (functionp request-success))
            (funcall request-success
                     (list :items
                           (list (list :feed_ts "1710000001.000100"
                                       :item
                                       (list :type "channel_message"
                                             :message
                                             (list :ts "1710000001.000100"
                                                   :channel channel-id))))
                           :response_metadata
                           (list :next_cursor "cursor-2")))
            (should (= 2 state-count-at-room))
            (should (= 2
                       (length
                        (plist-get (slack-page-state-value state)
                                   :activities))))
            (should (equal "cursor-2"
                           (slack-page-state-continuation state)))
            (should (slack-page-state-has-more state))
            (should (= 1 insertions))
            (with-current-buffer buffer
              (should-not slack-buffer--loading-more-p)
              (slack-buffer-load-more object)
              (should slack-buffer--loading-more-p))
            (should (functionp request-error))
            (funcall request-error "load more failed")
            (with-current-buffer buffer
              (should-not slack-buffer--loading-more-p))
            (should (equal "cursor-2"
                           (slack-page-state-continuation state))))
        (when (buffer-live-p buffer)
          (kill-buffer buffer))))))

(ert-deftest slack-test-activity-feed-load-more-preserves-consumer-errors ()
  "Do not reclassify a pagination consumer error as a request failure."
  (slack-test-setup
    (let* ((key (slack-activity-feed--page-key nil))
           (state (slack-team-page-state team key))
           (feed (make-instance 'slack-activity-feed
                                :activities '(old)
                                :pagination "cursor"
                                :last nil))
           (object
            (make-instance
             'slack-activity-feed-buffer
             :team-id (oref team id)
             :room-id "__activity-feed__"
             :cached-team team
             :page-key key
             :activity-feed feed))
           (buffer
            (generate-new-buffer " *slack-test-feed-consumer-error*"))
           request-success
           request-errors)
      (slack-page-state-store
       state
       (list :activities '(old) :pagination "cursor")
       "cursor" t)
      (slack-buffer-cache-team object team)
      (oset object buf buffer)
      (unwind-protect
          (cl-letf (((symbol-function 'slack-activity-feed-request)
                     (lambda (_team success &optional _cursor _on-error)
                       (setq request-success success)))
                    ((symbol-function 'slack-activity-feed--parse-item)
                     #'identity)
                    ((symbol-function 'slack-activity-feed--prefetch-rooms)
                     (lambda (_activities _team callback)
                       (funcall callback)))
                    ((symbol-function 'slack-activity-feed--prefetch-messages)
                     #'ignore))
            (slack-buffer-request-history
             object
             (lambda (&rest _)
               (error "pagination consumer boom"))
             (lambda (&rest errors)
               (push errors request-errors)))
            (should-error
             (funcall request-success
                      (list :items '(next)
                            :response_metadata
                            (list :next_cursor "next-cursor"))))
            (should-not request-errors)
            (should (equal '(old next)
                           (plist-get (slack-page-state-value state)
                                      :activities)))
            (should (equal "next-cursor"
                           (slack-page-state-continuation state))))
        (when (buffer-live-p buffer)
          (kill-buffer buffer))))))

(ert-deftest slack-test-activity-feed-load-more-waits-for-primary-hydration ()
  (slack-test-setup
    (let* ((key (slack-activity-feed--page-key nil))
           (state (slack-team-page-state team key))
           (object
            (make-instance
             'slack-activity-feed-buffer
             :team-id (oref team id)
             :room-id "__activity-feed__"
             :cached-team team
             :page-key key
             :activity-feed
             (make-instance 'slack-activity-feed
                            :activities '(old)
                            :pagination "cursor"
                            :last nil)))
           (buffer (generate-new-buffer " *slack-test-feed-hydrating*"))
           requested)
      (slack-page-state-store
       state
       (list :activities '(old) :pagination "cursor")
       "cursor" t)
      (slack-page-state-begin state t)
      (slack-buffer-cache-team object team)
      (oset object buf buffer)
      (unwind-protect
          (cl-letf (((symbol-function 'slack-activity-feed-request)
                     (lambda (&rest _)
                       (setq requested t))))
            (with-current-buffer buffer
              (slack-buffer-load-more object)
              (should-not slack-buffer--loading-more-p))
            (should-not requested)
            (should (eq 'refreshing (slack-page-state-status state))))
        (when (buffer-live-p buffer)
          (kill-buffer buffer))))))

(ert-deftest slack-test-activity-feed-load-more-uses-durable-state ()
  (slack-test-setup
    (let* ((key (slack-activity-feed--page-key nil))
           (state (slack-team-page-state team key))
           (object
            (make-instance
             'slack-activity-feed-buffer
             :team-id (oref team id)
             :room-id "__activity-feed__"
             :cached-team team
             :page-key key
             :activity-feed
             (make-instance 'slack-activity-feed
                            :activities '(buffer-old)
                            :pagination nil
                            :last nil)))
           (buffer (generate-new-buffer " *slack-test-feed-state-page*"))
           request-success
           requested-cursor
           requested-cursors)
      (slack-page-state-store
       state
       (list :activities '(state-old) :pagination "state-cursor")
       "state-cursor" t)
      (slack-buffer-cache-team object team)
      (oset object buf buffer)
      (unwind-protect
          (cl-letf (((symbol-function 'slack-activity-feed-request)
                     (lambda (_team success &optional cursor _on-error)
                       (setq request-success success
                             requested-cursor cursor)
                       (push cursor requested-cursors)))
                    ((symbol-function 'slack-activity-feed--parse-item)
                     #'identity)
                    ((symbol-function 'slack-activity-feed--prefetch-rooms)
                     (lambda (_activities _team callback)
                       (funcall callback)))
                    ((symbol-function 'slack-activity-feed--prefetch-messages)
                     #'ignore)
                    ((symbol-function 'slack-buffer-insert)
                     (lambda (_buffer activity)
                       (insert (format "%s\n" activity))))
                    ((symbol-function
                      'slack-activity-feed--replace-live-contents)
                     (lambda (feed-buffer activities)
                       (with-current-buffer (oref feed-buffer buf)
                         (erase-buffer)
                         (dolist (activity activities)
                           (insert (format "%s\n" activity)))))))
            (with-current-buffer buffer
              (insert "buffer-old\n"))
            (with-current-buffer buffer
              (slack-buffer-load-more object)
              (should slack-buffer--loading-more-p))
            (should (equal "state-cursor" requested-cursor))
            (funcall request-success
                     (list :items '(state-next)
                           :response_metadata
                           (list :next_cursor "next-cursor")))
            (should (equal '(state-old state-next)
                           (plist-get (slack-page-state-value state)
                                      :activities)))
            (should (equal "next-cursor"
                           (slack-page-state-continuation state)))
            (should (slack-page-state-has-more state))
            (with-current-buffer buffer
              (should (equal "state-old\nstate-next\n"
                             (buffer-string)))
              (slack-buffer-load-more object)
              (should slack-buffer--loading-more-p))
            (should (equal "next-cursor" requested-cursor))
            (funcall request-success
                     (list :items '(state-last)
                           :response_metadata
                           (list :next_cursor "final-cursor")))
            (should (equal '(state-old state-next state-last)
                           (plist-get (slack-page-state-value state)
                                      :activities)))
            (should (equal '("state-cursor" "next-cursor")
                           (nreverse requested-cursors)))
            (with-current-buffer buffer
              (should (equal "state-old\nstate-next\nstate-last\n"
                             (buffer-string)))))
        (when (buffer-live-p buffer)
          (kill-buffer buffer))))))

(ert-deftest slack-test-activity-feed-load-more-preserves-newer-event-state ()
  (slack-test-setup
    (let* ((key (slack-activity-feed--page-key nil))
           (state (slack-team-page-state team key))
           (source-feed
            (make-instance 'slack-activity-feed
                           :activities '(old)
                           :pagination "old-cursor"
                           :last nil))
           (object
            (make-instance
             'slack-activity-feed-buffer
             :team-id (oref team id)
             :room-id "__activity-feed__"
             :cached-team team
             :page-key key
             :activity-feed source-feed))
           (buffer (generate-new-buffer " *slack-test-feed-event-race*"))
           request-success
           (insertions 0))
      (slack-page-state-store
       state
       (list :activities '(old) :pagination "old-cursor")
       "old-cursor" t)
      (slack-buffer-cache-team object team)
      (oset object buf buffer)
      (unwind-protect
          (cl-letf (((symbol-function 'slack-activity-feed-request)
                     (lambda (_team success &optional _cursor _on-error)
                       (setq request-success success)))
                    ((symbol-function 'slack-activity-feed--parse-item)
                     #'identity)
                    ((symbol-function 'slack-buffer-insert--history)
                     (lambda (_buffer)
                       (cl-incf insertions))))
            (with-current-buffer buffer
              (slack-buffer-load-more object)
              (should slack-buffer--loading-more-p))
            (slack-activity-feed--cache-put team '(event-new) "")
            (funcall request-success
                     (list :items '(late-page)
                           :response_metadata
                           (list :next_cursor "late-cursor")))
            (should (equal '(event-new)
                           (plist-get (slack-page-state-value state)
                                      :activities)))
            (should (equal "" (slack-page-state-continuation state)))
            (should-not (slack-page-state-has-more state))
            (should (eq source-feed (oref object activity-feed)))
            (should (= 0 insertions))
            (with-current-buffer buffer
              (should-not slack-buffer--loading-more-p)))
        (when (buffer-live-p buffer)
          (kill-buffer buffer))))))

(ert-deftest slack-test-activity-feed-load-more-keeps-captured-mode ()
  (slack-test-setup
    (let* ((old-key (slack-activity-feed--page-key nil))
           (old-state (slack-team-page-state team old-key))
           (old-feed
            (make-instance 'slack-activity-feed
                           :activities '(old)
                           :pagination "cursor"
                           :last nil))
           (object
            (make-instance
             'slack-activity-feed-buffer
             :team-id (oref team id)
             :room-id "__activity-feed__"
             :cached-team team
             :page-key old-key
             :activity-feed old-feed))
           (buffer (generate-new-buffer " *slack-test-feed-mode-race*"))
           request-success
           (insertions 0))
      (slack-page-state-store
       old-state
       (list :activities '(old) :pagination "cursor")
       "cursor" t)
      (slack-buffer-cache-team object team)
      (oset object buf buffer)
      (unwind-protect
          (cl-letf (((symbol-function 'slack-activity-feed-request)
                     (lambda (_team success &optional _cursor _on-error)
                       (setq request-success success)))
                    ((symbol-function 'slack-activity-feed--parse-item)
                     #'identity)
                    ((symbol-function 'slack-activity-feed--prefetch-rooms)
                     (lambda (_activities _team callback)
                       (funcall callback)))
                    ((symbol-function 'slack-activity-feed--prefetch-messages)
                     #'ignore)
                    ((symbol-function 'slack-buffer-insert--history)
                     (lambda (_buffer)
                       (cl-incf insertions))))
            (with-current-buffer buffer
              (slack-buffer-load-more object)
              (should slack-buffer--loading-more-p))
            (let ((new-feed
                   (make-instance 'slack-activity-feed
                                  :activities '(new-mode)
                                  :pagination nil
                                  :last nil)))
              (oset object page-key (slack-activity-feed--page-key t))
              (oset object activity-feed new-feed)
              (funcall request-success
                       (list :items '(old-next)
                             :response_metadata nil))
              (should (eq new-feed (oref object activity-feed))))
            (should (equal '(old old-next)
                           (plist-get (slack-page-state-value old-state)
                                      :activities)))
            (should (= 0 insertions))
            (with-current-buffer buffer
              (should-not slack-buffer--loading-more-p)))
        (when (buffer-live-p buffer)
          (kill-buffer buffer))))))

(ert-deftest slack-test-activity-feed-empty-cursor-has-no-next-page ()
  (slack-test-setup
    (let ((object
           (make-instance
            'slack-activity-feed-buffer
            :team-id (oref team id)
            :room-id "__activity-feed__"
            :cached-team team
            :page-key (slack-activity-feed--page-key nil)
            :activity-feed
            (make-instance 'slack-activity-feed
                           :activities nil
                           :pagination ""
                           :last nil))))
      (should-not (slack-buffer-has-next-page-p object)))))

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

(ert-deftest slack-test-activity-feed-hydration-does-not-resurrect-buffer ()
  (slack-test-setup
    (let (request-success
          object
          buffer
          room-success
          message-success
          state)
      (cl-letf (((symbol-function 'slack-activity-feed--selected-team)
                 (lambda () team))
                ((symbol-function 'slack-activity-feed-render-page-state)
                 #'ignore)
                ((symbol-function 'slack-buffer-display)
                 (lambda (displayed)
                   (setq object displayed
                         buffer (oref displayed buf))))
                ((symbol-function 'slack-activity-feed-request)
                 (lambda (_team success &optional _cursor _on-error)
                   (setq request-success success)))
                ((symbol-function
                  'slack-activity-feed--fetch-watched-activities)
                 (lambda (_team callback)
                   (funcall callback nil)))
                ((symbol-function 'slack-activity-feed--prefetch-rooms)
                 (lambda (_activities _team callback)
                   (setq room-success callback)))
                ((symbol-function 'slack-activity-feed--prefetch-messages)
                 (lambda (_activities _team callback &rest _)
                   (setq message-success callback))))
        (slack-activity-feed-show)
        (setq state
              (slack-team-page-state team (slack-activity-feed--page-key)))
        (funcall request-success
                 (list :items nil :response_metadata nil))
        (should (functionp room-success))
        (kill-buffer buffer)
        (cl-letf (((symbol-function 'slack-buffer-buffer)
                   (lambda (&rest _)
                     (ert-fail "hydration resurrected a killed buffer"))))
          (funcall room-success)
          (should (functionp message-success))
          (funcall message-success))
        (should-not (buffer-live-p buffer))
        (should-not (buffer-live-p (oref object buf)))
        (should (eq 'ready (slack-page-state-status state)))))))

(ert-deftest slack-test-activity-feed-unavailable-survives-kill-and-reopen ()
  (slack-test-setup
    (let (request-success
          object
          buffer
          hydrated-activities
          message-success
          unavailable-callback)
      (unwind-protect
          (cl-letf (((symbol-function 'slack-activity-feed--selected-team)
                     (lambda () team))
                    ((symbol-function 'slack-buffer-display)
                     (lambda (displayed)
                       (setq object displayed
                             buffer (oref displayed buf))))
                    ((symbol-function 'slack-activity-feed-request)
                     (lambda (_team success &optional _cursor _on-error)
                       (setq request-success success)))
                    ((symbol-function
                      'slack-activity-feed--fetch-watched-activities)
                     (lambda (_team callback)
                       (funcall callback nil)))
                    ((symbol-function 'slack-activity-feed--prefetch-rooms)
                     (lambda (_activities _team callback)
                       (funcall callback)))
                    ((symbol-function 'slack-activity-feed--prefetch-messages)
                     (lambda (activities _team callback _row-callback
                                         missing-callback)
                       (setq hydrated-activities activities
                             message-success callback
                             unavailable-callback missing-callback))))
            (slack-activity-feed-show)
            (funcall request-success
                     (list :items
                           (list
                            (list :feed_ts "1710000000.000100"
                                  :item
                                  (list :type "channel_message"
                                        :message
                                        (list :ts "1710000000.000100"
                                              :channel channel-id))))))
            (let* ((state
                    (slack-team-page-state
                     team (slack-activity-feed--page-key nil)))
                   (activity (car hydrated-activities))
                   (activity-message (oref (oref activity item) message)))
              (should-not (oref activity-message source-message-unavailable))
              (kill-buffer buffer)
              (funcall unavailable-callback activity)
              (funcall message-success)
              (should (eq 'ready (slack-page-state-status state)))
              (should (oref activity-message source-message-unavailable))
              (slack-activity-feed-show)
              (should (buffer-live-p buffer))
              (with-current-buffer buffer
                (goto-char (point-min))
                (should (search-forward "Message unavailable." nil t))
                (goto-char (point-min))
                (should-not (search-forward "Loading message..." nil t)))))
        (when (buffer-live-p buffer)
          (kill-buffer buffer))))))

(ert-deftest slack-test-activity-feed-unavailable-survives-mode-switch ()
  (slack-test-setup
    (let ((slack-activity-feed-mode-show-only-unread nil)
          request-success
          object
          buffer
          hydrated-activities
          message-success
          unavailable-callback
          (row-replacements 0))
      (unwind-protect
          (cl-letf (((symbol-function 'slack-activity-feed--selected-team)
                     (lambda () team))
                    ((symbol-function 'slack-buffer-display)
                     (lambda (displayed)
                       (setq object displayed
                             buffer (oref displayed buf))))
                    ((symbol-function 'slack-activity-feed-request)
                     (lambda (_team success &optional _cursor _on-error)
                       (setq request-success success)))
                    ((symbol-function
                      'slack-activity-feed--fetch-watched-activities)
                     (lambda (_team callback)
                       (funcall callback nil)))
                    ((symbol-function 'slack-activity-feed--prefetch-rooms)
                     (lambda (_activities _team callback)
                       (funcall callback)))
                    ((symbol-function 'slack-activity-feed--prefetch-messages)
                     (lambda (activities _team callback _row-callback
                                         missing-callback)
                       (setq hydrated-activities activities
                             message-success callback
                             unavailable-callback missing-callback)))
                    ((symbol-function
                      'slack-activity-feed--replace-buffer-unavailable)
                     (lambda (&rest _)
                       (cl-incf row-replacements))))
            (slack-activity-feed-show)
            (funcall request-success
                     (list :items
                           (list
                            (list :feed_ts "1710000000.000100"
                                  :item
                                  (list :type "channel_message"
                                        :message
                                        (list :ts "1710000000.000100"
                                              :channel channel-id))))))
            (let* ((old-state
                    (slack-team-page-state
                     team (slack-activity-feed--page-key nil)))
                   (activity (car hydrated-activities))
                   (activity-message (oref (oref activity item) message)))
              (setq slack-activity-feed-mode-show-only-unread t)
              (slack-activity-feed-show)
              (funcall unavailable-callback activity)
              (funcall message-success)
              (should (eq 'ready (slack-page-state-status old-state)))
              (should (oref activity-message source-message-unavailable))
              (should (= 0 row-replacements))
              (should-not
               (oref (oref object activity-feed) activities))
              (setq slack-activity-feed-mode-show-only-unread nil)
              (slack-activity-feed-show)
              (with-current-buffer buffer
                (goto-char (point-min))
                (should (search-forward "Message unavailable." nil t))
                (goto-char (point-min))
                (should-not (search-forward "Loading message..." nil t)))))
        (when (buffer-live-p buffer)
          (kill-buffer buffer))))))

(ert-deftest slack-test-event-cache-refresh-is-debounced ()
  (slack-test-setup
    (let ((slack-activity-feed--event-refresh-time 0)
          (slack-activity-refresh-debounce 30)
          (refreshes 0))
      (slack-activity-feed--cache-put team nil nil)
      (cl-letf (((symbol-function 'slack-activity-feed--refresh-cache)
                 (lambda (_team &optional _after _quiet) (cl-incf refreshes))))
        (slack-activity-feed-refresh-cache-from-event team)
        (slack-activity-feed-refresh-cache-from-event team)
        (slack-activity-feed-refresh-cache-from-event team))
      (should (eq 1 refreshes)))))

(ert-deftest slack-test-event-cache-refresh-updates-state-without-display ()
  (slack-test-setup
    (let ((slack-activity-feed--event-refresh-time 0)
          (slack-activity-refresh-debounce 0))
      (slack-activity-feed--cache-put team '(old) nil)
      (cl-letf (((symbol-function 'slack-activity-feed-request)
                 (lambda (_team success &optional _cursor _on-error)
                   (funcall success
                            (list :items
                                  (list
                                   (list :feed_ts "1710000001.000100"
                                         :item
                                         (list :type "channel_message"
                                               :message
                                               (list
                                                :ts "1710000001.000100"
                                                :channel channel-id))))
                                  :response_metadata nil))))
                ((symbol-function
                  'slack-activity-feed--fetch-watched-activities)
                 (lambda (_team callback)
                   (funcall callback nil)))
                ((symbol-function 'slack-buffer-display)
                 (lambda (&rest _)
                   (ert-fail "event refresh must not display a buffer"))))
        (slack-activity-feed-refresh-cache-from-event team))
      (let* ((state
              (slack-team-page-state
               team (slack-activity-feed--page-key nil)))
             (activities
              (plist-get (slack-page-state-value state) :activities)))
        (should (eq 'ready (slack-page-state-status state)))
        (should (= 1 (length activities)))
        (should (equal "1710000001.000100"
                       (oref (car activities) feed-ts)))))))

(ert-deftest slack-test-event-cache-response-does-not-supersede-refresh ()
  (slack-test-setup
    (let* ((key (slack-activity-feed--page-key nil))
           (state (slack-team-page-state team key))
           event-success)
      (slack-activity-feed--cache-put team '(old) nil)
      (cl-letf (((symbol-function 'slack-activity-feed-request)
                 (lambda (_team success &optional _cursor _on-error)
                   (setq event-success success)))
                ((symbol-function 'slack-activity-feed--parse-item)
                 #'identity)
                ((symbol-function 'slack-activity-feed--merge-activities)
                 (lambda (activities extras)
                   (append activities extras)))
                ((symbol-function
                  'slack-activity-feed--fetch-watched-activities)
                 (lambda (_team callback)
                   (funcall callback nil))))
        (slack-activity-feed--refresh-cache team)
        (let* ((refresh-generation (slack-page-state-begin state t))
               (user-snapshot
                (list :activities '(user-refresh) :pagination nil)))
          (slack-page-state-commit
           state refresh-generation user-snapshot nil nil)
          (funcall event-success
                   (list :items '(delayed-event)
                         :response_metadata nil))
          (should (= refresh-generation
                     (slack-page-state-generation state)))
          (should (eq user-snapshot (slack-page-state-value state)))
          (should (eq 'ready (slack-page-state-status state))))))))

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
          (captured-error nil)
          star)
      (slack-buffer-cache-team buf-obj team)
      (oset buf-obj buf buffer)
      (setq star (slack-create-star
                  (list :saved_items nil
                        :response_metadata (list :next_cursor "cur"))))
      (oset team star star)
      (slack-page-state-store
       (slack-team-page-state team 'saved-items) star "cur" t)
      (unwind-protect
          (progn
            (cl-letf (((symbol-function 'slack-stars-list-request)
                       (lambda (_team _cursor _success &optional on-error
                                _on-primary-page)
                         (setq captured-error on-error))))
              (with-current-buffer buffer
                (slack-buffer-load-more buf-obj)
                (should slack-buffer--loading-more-p)))
            (should (functionp captured-error))
            (funcall captured-error "rate_limited")
            (with-current-buffer buffer
              (should-not slack-buffer--loading-more-p)))
        (kill-buffer buffer)))))

(ert-deftest slack-test-stars-load-more-commits-and-rejects-stale-page ()
  (slack-test-setup
    (let* ((old-item (slack-test-star-item "2.000" channel-id))
           (next-item (slack-test-star-item "1.000" channel-id))
           (stale-item (slack-test-star-item "0.000" channel-id))
           (old-star (make-instance 'slack-star
                                    :items (list old-item)
                                    :cursor "cursor-1"))
           (next-page (make-instance 'slack-star
                                     :items (list next-item)
                                     :cursor "cursor-2"))
           (stale-page (make-instance 'slack-star
                                      :items (list stale-item)
                                      :cursor "cursor-3"))
           (newer-star (make-instance 'slack-star
                                      :items (list old-item)
                                      :cursor "newer-cursor"))
           (state (slack-team-page-state team 'saved-items))
           (object (slack-create-stars-buffer team))
           buffer
           request-primary
           request-hydrated
           requested-cursor
           appended
           request-stored)
      (oset team star old-star)
      (slack-page-state-store state old-star "cursor-1" t)
      (setq buffer (slack-buffer-buffer object))
      (unwind-protect
          (cl-letf (((symbol-function 'slack-stars-list-request)
                     (lambda (_team cursor after-success _on-error
                              on-primary-page)
                       (setq requested-cursor cursor
                             request-primary on-primary-page
                             request-hydrated after-success)))
                    ((symbol-function 'slack-stars--prefetch-messages)
                     (lambda (_items _team callback)
                       (funcall callback)))
                    ((symbol-function 'slack-stars-buffer--append-items)
                     (lambda (_object items _state)
                       (setq appended (append appended items)))))
            (with-current-buffer buffer
              (slack-buffer-load-more object)
              (should slack-buffer--loading-more-p))
            (should (equal "cursor-1" requested-cursor))
            (setq request-stored
                  (make-instance 'slack-star
                                 :items (list old-item next-item)
                                 :cursor "cursor-2"))
            (oset team star request-stored)
            (funcall request-primary next-page request-stored)
            (let ((stored (slack-page-state-value state)))
              (should (equal (list old-item next-item)
                             (slack-star-items stored)))
              (should (equal "cursor-2" (oref stored cursor)))
              (should (eq stored (oref team star))))
            (funcall request-hydrated)
            (should (equal (list next-item) appended))
            (with-current-buffer buffer
              (should-not slack-buffer--loading-more-p)
              (slack-buffer-load-more object)
              (should slack-buffer--loading-more-p))
            (should (equal "cursor-2" requested-cursor))
            (slack-page-state-store state newer-star "newer-cursor" t)
            (oset team star newer-star)
            (setq request-stored
                  (make-instance 'slack-star
                                 :items (list old-item stale-item)
                                 :cursor "cursor-3"))
            (oset team star request-stored)
            (funcall request-primary stale-page request-stored)
            (funcall request-hydrated)
            (should (eq newer-star (slack-page-state-value state)))
            (should (eq newer-star (oref team star)))
            (should (equal "newer-cursor"
                           (slack-page-state-continuation state)))
            (should (equal (list next-item) appended))
            (with-current-buffer buffer
              (should-not slack-buffer--loading-more-p)))
        (when (buffer-live-p buffer)
          (kill-buffer buffer))))))

(ert-deftest slack-test-stars-load-more-preserves-newer-event-state ()
  (slack-test-setup
    (let* ((old-item (slack-test-star-item "2.000" channel-id))
           (event-item (slack-test-star-item "3.000" channel-id))
           (page-item (slack-test-star-item "1.000" channel-id))
           (old-star (make-instance 'slack-star
                                    :items (list old-item)
                                    :cursor "cursor-1"))
           (page (make-instance 'slack-star
                                :items (list page-item)
                                :cursor ""))
           (request-star (make-instance 'slack-star
                                        :items (list event-item old-item
                                                     page-item)
                                        :cursor ""))
           (state (slack-team-page-state team 'saved-items))
           (object (slack-create-stars-buffer team))
           (buffer (slack-buffer-buffer object))
           request-primary
           request-hydrated
           appended)
      (oset team star old-star)
      (slack-page-state-store state old-star "cursor-1" t)
      (unwind-protect
          (cl-letf (((symbol-function 'slack-stars-list-request)
                     (lambda (_team _cursor after-success _on-error
                              on-primary-page)
                       (setq request-primary on-primary-page
                             request-hydrated after-success)))
                    ((symbol-function 'slack-stars-buffer--append-items)
                     (lambda (&rest _args)
                       (setq appended t))))
            (with-current-buffer buffer
              (slack-buffer-load-more object))
            (push event-item (oref old-star items))
            (oset team star request-star)
            (funcall request-primary page request-star)
            (funcall request-hydrated)
            (should (eq old-star (slack-page-state-value state)))
            (should (eq old-star (oref team star)))
            (should (equal (list event-item old-item)
                           (slack-star-items old-star)))
            (should (equal "cursor-1"
                           (slack-page-state-continuation state)))
            (should-not appended)
            (with-current-buffer buffer
              (should-not slack-buffer--loading-more-p)))
        (when (buffer-live-p buffer)
          (kill-buffer buffer))))))

(ert-deftest slack-test-stars-load-more-reconciles-removal-during-hydration ()
  (slack-test-setup
    (let* ((old-item (slack-test-star-item "2.000" channel-id))
           (page-item (slack-test-star-item "1.000" channel-id))
           (old-star (make-instance 'slack-star
                                    :items (list old-item)
                                    :cursor "cursor-1"))
           (page (make-instance 'slack-star
                                :items (list page-item)
                                :cursor ""))
           (request-star (make-instance 'slack-star
                                        :items (list old-item page-item)
                                        :cursor ""))
           (state (slack-team-page-state team 'saved-items))
           (object (slack-create-stars-buffer team))
           (buffer (slack-buffer-buffer object))
           request-primary
           request-hydrated
           hydration-done
           appended
           rerendered)
      (oset team star old-star)
      (slack-page-state-store state old-star "cursor-1" t)
      (unwind-protect
          (cl-letf (((symbol-function 'slack-stars-list-request)
                     (lambda (_team _cursor after-success _on-error
                              on-primary-page)
                       (setq request-primary on-primary-page
                             request-hydrated after-success)))
                    ((symbol-function 'slack-stars--prefetch-messages)
                     (lambda (_items _team callback)
                       (setq hydration-done callback)))
                    ((symbol-function 'slack-stars-buffer--append-items)
                     (lambda (_object items _state)
                       (setq appended items)))
                    ((symbol-function 'slack-stars-buffer-render-page-state)
                     (lambda (&rest _args)
                       (setq rerendered t))))
            (with-current-buffer buffer
              (slack-buffer-load-more object))
            (oset team star request-star)
            (funcall request-primary page request-star)
            (funcall request-hydrated)
            (should (functionp hydration-done))
            (slack-event-update-star-item
             (slack-create-star-event
              (list :type "star_removed"
                    :item (list :type "message" :channel channel-id
                                :message (list :ts "1.000"))))
             team)
            (funcall hydration-done)
            (should-not appended)
            (should rerendered)
            (should (equal (list old-item)
                           (slack-star-items
                            (slack-page-state-value state))))
            (with-current-buffer buffer
              (should-not slack-buffer--loading-more-p)))
        (when (buffer-live-p buffer)
          (kill-buffer buffer))))))

(ert-deftest slack-test-stars-load-more-resets-flag-on-sync-error ()
  (slack-test-setup
    (let* ((star (make-instance 'slack-star :cursor "cursor-1"))
           (state (slack-team-page-state team 'saved-items))
           (object (slack-create-stars-buffer team))
           buffer)
      (oset team star star)
      (slack-page-state-store state star "cursor-1" t)
      (setq buffer (slack-buffer-buffer object))
      (unwind-protect
          (cl-letf (((symbol-function 'slack-stars-list-request)
                     (lambda (&rest _args)
                       (error "synchronous saved page failure"))))
            (with-current-buffer buffer
              (should-error (slack-buffer-load-more object))
              (should-not slack-buffer--loading-more-p)))
        (when (buffer-live-p buffer)
          (kill-buffer buffer))))))

(ert-deftest slack-test-stars-load-more-does-not-write-replacement-buffer ()
  (slack-test-setup
    (let* ((old-item (slack-test-star-item "2.000" channel-id))
           (next-item (slack-test-star-item "1.000" channel-id))
           (old-star (make-instance 'slack-star
                                    :items (list old-item)
                                    :cursor "cursor-1"))
           (next-page (make-instance 'slack-star
                                     :items (list next-item)
                                     :cursor ""))
           (request-star (make-instance 'slack-star
                                        :items (list old-item next-item)
                                        :cursor ""))
           (state (slack-team-page-state team 'saved-items))
           (object (slack-create-stars-buffer team))
           (original (slack-buffer-buffer object))
           (replacement (generate-new-buffer
                         " *slack-test-stars-replacement*"))
           request-primary
           request-hydrated
           appended)
      (oset team star old-star)
      (slack-page-state-store state old-star "cursor-1" t)
      (unwind-protect
          (cl-letf (((symbol-function 'slack-stars-list-request)
                     (lambda (_team _cursor after-success _on-error
                              on-primary-page)
                       (setq request-primary on-primary-page
                             request-hydrated after-success)))
                    ((symbol-function 'slack-stars--prefetch-messages)
                     (lambda (_items _team callback)
                       (funcall callback)))
                    ((symbol-function 'slack-stars-buffer--append-items)
                     (lambda (&rest _args)
                       (setq appended t))))
            (with-current-buffer original
              (slack-buffer-load-more object)
              (should slack-buffer--loading-more-p))
            (oset object buf replacement)
            (with-current-buffer replacement
              (insert "replacement sentinel"))
            (oset team star request-star)
            (funcall request-primary next-page request-star)
            (funcall request-hydrated)
            (should-not appended)
            (should (equal (list old-item next-item)
                           (slack-star-items
                            (slack-page-state-value state))))
            (with-current-buffer original
              (should-not slack-buffer--loading-more-p))
            (with-current-buffer replacement
              (should (equal "replacement sentinel" (buffer-string)))))
        (when (buffer-live-p original)
          (kill-buffer original))
        (when (buffer-live-p replacement)
          (kill-buffer replacement))))))

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

(ert-deftest slack-test-thread-reopen-renders-durable-page-and-refreshes ()
  (slack-test-setup
    (let* ((thread-ts "1710000000.000100")
           (reply-ts "1710000000.000200")
           (parent (make-instance 'slack-message
                                  :type "message"
                                  :channel channel-id
                                  :ts thread-ts
                                  :thread_ts thread-ts
                                  :text "cached parent"
                                  :reactions nil))
           (reply (make-instance 'slack-message
                                 :type "message"
                                 :channel channel-id
                                 :ts reply-ts
                                 :thread_ts thread-ts
                                 :text "cached reply"
                                 :reactions nil))
           (live-reply (make-instance 'slack-message
                                      :type "message"
                                      :channel channel-id
                                      :ts "1710000000.000300"
                                      :thread_ts thread-ts
                                      :text "live reply"
                                      :reactions nil))
           (state (slack-thread-page-state channel team thread-ts))
           (old-object (slack-create-thread-message-buffer
                        channel team thread-ts))
           (old-buffer (slack-buffer-buffer old-object))
           (requests 0)
           first-object
           second-object
           buffer)
      (slack-page-state-store state (list parent reply) "" nil)
      (kill-buffer old-buffer)
      (slack-room-clear-messages channel)
      (slack-room-set-messages channel (list live-reply) team)
      (unwind-protect
          (cl-letf (((symbol-function 'slack-buffer-display)
                     (lambda (shown)
                       (if first-object
                           (setq second-object shown)
                         (setq first-object shown
                               buffer (oref shown buf)))))
                    ((symbol-function 'slack-thread-replies)
                     (lambda (&rest _) (cl-incf requests))))
            (slack-thread-show-messages parent channel team)
            (should (= 1 requests))
            (should (eq 'refreshing (slack-page-state-status state)))
            (should (with-current-buffer buffer
                      (and (save-excursion
                             (goto-char (point-min))
                             (search-forward "cached parent" nil t))
                           (save-excursion
                             (goto-char (point-min))
                             (search-forward "cached reply" nil t))
                           (save-excursion
                             (goto-char (point-min))
                             (search-forward "live reply" nil t)))))
            (slack-thread-show-messages parent channel team)
            (should (eq first-object second-object))
            (should (= 1 requests)))
        (when (buffer-live-p buffer) (kill-buffer buffer))))))

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
                 (lambda (room _team &optional _callback)
                   (push 'display events)
                   (let* ((state (oref room history-state))
                          (generation (slack-page-state-begin state t)))
                     (setq ready
                           (lambda ()
                             (slack-page-state-commit
                              state generation nil "" nil))))))
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

(ert-deftest slack-test-open-channel-ready-starts-one-before-request ()
  (slack-test-setup
    (let* ((target-ts "1710000000.000500")
           (state (oref channel history-state))
           (slack-test-out-load-older-messages-p t)
           history-success
           before-success
           object
           (before-requests 0))
      (unwind-protect
          (cl-letf (((symbol-function 'slack-buffer-display)
                     (lambda (shown) (setq object shown)))
                    ((symbol-function 'slack-room-history-load)
                     (lambda (_room _team _generation success _error)
                       (setq history-success success)))
                    ((symbol-function 'slack-messages-before)
                     (lambda (_ts _room _team &optional success _error)
                       (cl-incf before-requests)
                       (setq before-success success)))
                    ((symbol-function 'slack-messages-after)
                     (lambda (&rest _) nil)))
            (slack-open-message--open-channel
             target-ts channel team #'ignore #'ignore)
            (should (eq 'loading (slack-page-state-status state)))
            (funcall history-success nil "" nil)
            (should (= 1 before-requests))
            (should (functionp before-success)))
        (when (and object (slot-boundp object 'buf)
                   (buffer-live-p (oref object buf)))
          (kill-buffer (oref object buf)))))))

(ert-deftest slack-test-open-channel-stale-room-waits-on-canonical-state ()
  "A deep link passed a stale room object still follows the canonical page."
  (slack-test-setup
    (let* ((stale-room (make-instance 'slack-channel
                                      :id channel-id
                                      :name "StaleChannel"))
           (canonical-state (oref channel history-state))
           (stale-state (oref stale-room history-state))
           (slack-test-out-load-older-messages-p nil)
           loaded-room
           history-success
           object
           finished)
      (unwind-protect
          (cl-letf (((symbol-function 'slack-buffer-display)
                     (lambda (shown) (setq object shown)))
                    ((symbol-function 'slack-room-history-load)
                     (lambda (room _team _generation success _error)
                       (setq loaded-room room
                             history-success success))))
            (slack-open-message--open-channel
             "1710000000.000500" stale-room team
             (lambda () (setq finished t)))
            (should (eq channel loaded-room))
            (should (eq 'loading
                        (slack-page-state-status canonical-state)))
            (should (eq 'unloaded (slack-page-state-status stale-state)))
            (funcall history-success nil "" nil)
            (should finished))
        (when (and object (slot-boundp object 'buf)
                   (buffer-live-p (oref object buf)))
          (kill-buffer (oref object buf)))))))

(ert-deftest slack-test-open-channel-coalesced-callers-each-finish-once ()
  (slack-test-setup
    (let* ((target-ts "1710000000.000500")
           (slack-test-out-load-older-messages-p t)
           history-success
           before-successes
           after-successes
           object
           finished
           (history-requests 0)
           (before-requests 0))
      (unwind-protect
          (cl-letf (((symbol-function 'slack-buffer-display)
                     (lambda (shown) (setq object shown)))
                    ((symbol-function 'slack-room-history-load)
                     (lambda (_room _team _generation success _error)
                       (cl-incf history-requests)
                       (setq history-success success)))
                    ((symbol-function 'slack-messages-before)
                     (lambda (_ts _room _team &optional success _error)
                       (cl-incf before-requests)
                       (push success before-successes)))
                    ((symbol-function 'slack-messages-after)
                     (lambda (_ts _room _team &optional success _error)
                       (push success after-successes))))
            (slack-open-message--open-channel
             target-ts channel team (lambda () (push 'first finished)))
            (slack-open-message--open-channel
             target-ts channel team (lambda () (push 'second finished)))
            (should (= 1 history-requests))
            (funcall history-success nil "" nil)
            (should (= 2 before-requests))
            (mapc #'funcall before-successes)
            (should (= 2 (length after-successes)))
            (mapc #'funcall after-successes)
            (should (equal '(first second) finished)))
        (when (and object (slot-boundp object 'buf)
                   (buffer-live-p (oref object buf)))
          (kill-buffer (oref object buf)))))))

(ert-deftest slack-test-open-channel-range-error-reaches-fallback ()
  (slack-test-setup
    (let ((target-ts "1710000000.000500")
          ready
          range-error
          (fallbacks 0))
      (cl-letf (((symbol-function 'slack-room-display)
                 (lambda (room _team &optional _callback)
                   (let* ((state (oref room history-state))
                          (generation (slack-page-state-begin state t)))
                     (setq ready
                           (lambda ()
                             (slack-page-state-commit
                              state generation nil "" nil))))))
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
                 (lambda (room _team &optional _callback)
                   (let* ((state (oref room history-state))
                          (generation (slack-page-state-begin state t)))
                     (setq ready
                           (lambda ()
                             (slack-page-state-commit
                              state generation nil "" nil))))))
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
                 (lambda (room _team &optional _callback)
                   (slack-page-state-store
                    (oref room history-state) nil "" nil))))
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
    (let ((slack-activity-feed-mode-show-only-unread nil)
          (captured-success nil))
      (slack-activity-feed--cache-put team 'all-mode-snapshot nil)
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
                  (plist-get (slack-page-state-value
                              (slack-team-page-state
                               team (slack-activity-feed--page-key nil)))
                             :activities)))
      (should (slack-page-state-value
               (slack-team-page-state
                team (slack-activity-feed--page-key t)))))))

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

(ert-deftest slack-test-pins-list-publishes-primary-before-user-hydration ()
  (slack-test-setup
    (let (request user-success primary-items hydrated-items events)
      (cl-letf (((symbol-function 'slack-request)
                 (lambda (created &rest _args)
                   (setq request created)))
                ((symbol-function 'slack-users-info-request)
                 (lambda (_ids _team &rest args)
                   (setq user-success (plist-get args :after-success))
                   (push 'users-request events))))
        (slack-pins-list
         channel team
         (lambda (items)
           (setq hydrated-items items)
           (push 'hydrated events))
         (lambda (items)
           (setq primary-items items)
           (push 'primary events)))
        (funcall (oref request success)
                 :data
                 (list :ok t
                       :items
                       (list (slack-test-pin-message-payload
                              "1710000000.000100" "primary pin" "U99999"))))
        (should (equal '(primary users-request) (reverse events)))
        (should (= 1 (length primary-items)))
        (should-not hydrated-items)
        (should (functionp user-success))
        (funcall user-success)
        (should (eq primary-items hydrated-items))
        (should (equal '(primary users-request hydrated)
                       (reverse events)))))))

(ert-deftest slack-test-pins-list-routes-api-and-transport-errors ()
  (slack-test-setup
    (let (request errors primary hydrated)
      (cl-letf (((symbol-function 'slack-request)
                 (lambda (created &rest _args)
                   (setq request created))))
        (slack-pins-list
         channel team
         (lambda (_items) (setq hydrated t))
         (lambda (_items) (setq primary t))
         (lambda (&rest values) (push values errors)))
        (funcall (oref request success)
                 :data (list :ok :json-false :error "invalid_auth"))
        (should (equal '(("invalid_auth")) errors))
        (should-not primary)
        (should-not hydrated)
        (funcall (oref request error) :error-thrown "offline")
        (should (equal '((:error-thrown "offline") ("invalid_auth"))
                       errors))))))

(ert-deftest slack-test-pinned-items-display-primary-before-user-hydration ()
  (slack-test-setup
    (let* ((source (slack-create-message-buffer channel "" team))
           events request user-success object emacs-buffer)
      (unwind-protect
          (cl-letf (((symbol-function 'slack-buffer-display)
                     (lambda (displayed)
                       (setq object displayed
                             emacs-buffer (oref displayed buf))
                       (push 'display events)))
                    ((symbol-function 'slack-request)
                     (lambda (created &rest _args)
                       (setq request created)
                       (push 'request events)))
                    ((symbol-function 'slack-users-info-request)
                     (lambda (_ids _team &rest args)
                       (setq user-success (plist-get args :after-success)))))
            (let ((returned (slack-buffer-display-pins-list source)))
              (should (eq object returned)))
            (should (equal '(display request) (nreverse events)))
            (let ((state (slack-team-page-state
                          team (list 'pins channel-id))))
              (should (eq 'loading (slack-page-state-status state)))
              (with-current-buffer emacs-buffer
                (goto-char (point-min))
                (should (search-forward "Pinned Items" nil t))
                (goto-char (point-min))
                (should (search-forward "Loading Slack data" nil t))
                (goto-char (point-min))
                (should-not (search-forward "No Pinned Items" nil t)))
              (funcall
               (oref request success)
               :data
               (list :ok t
                     :items
                     (list (slack-test-pin-message-payload
                            "1710000000.000100" "primary pin" "U99999"))))
              (should (slack-page-state-loaded-p state))
              (should (eq 'loading (slack-page-state-status state)))
              (should (= 1 (length (oref object items))))
              (with-current-buffer emacs-buffer
                (goto-char (point-min))
                (should (search-forward "primary pin" nil t)))
              (funcall user-success)
              (should (eq 'ready (slack-page-state-status state)))
              (should (eq emacs-buffer (oref object buf)))))
        (when (buffer-live-p emacs-buffer)
          (kill-buffer emacs-buffer))))))

(ert-deftest slack-test-pinned-items-room-keys-are-isolated ()
  (slack-test-setup
    (let* ((other (make-instance 'slack-channel
                                 :id "C22222" :name "OtherChannel"))
           (source-one (slack-create-message-buffer channel "" team))
           source-two callbacks object-one object-two)
      (puthash (oref other id) other (oref team channels))
      (setq source-two (slack-create-message-buffer other "" team))
      (unwind-protect
          (cl-letf (((symbol-function 'slack-buffer-display) #'ignore)
                    ((symbol-function 'slack-pins-list)
                     (lambda (room _team after-success
                                   &optional on-primary-page _on-error)
                       (push (list (oref room id)
                                   after-success on-primary-page)
                             callbacks))))
            (setq object-one (slack-buffer-display-pins-list source-one)
                  object-two (slack-buffer-display-pins-list source-two))
            (should-not (eq object-one object-two))
            (let ((state-one (slack-team-page-state
                              team (list 'pins channel-id)))
                  (state-two (slack-team-page-state
                              team (list 'pins (oref other id))))
                  (item (slack-test-pinned-item
                         channel team "1.000" "room one")))
              (should-not (eq state-one state-two))
              (funcall (nth 2 (assoc channel-id callbacks)) (list item))
              (should (equal (list item) (slack-page-state-value state-one)))
              (should-not (slack-page-state-loaded-p state-two))))
        (dolist (object (list object-one object-two))
          (when (and object
                     (slot-boundp object 'buf)
                     (buffer-live-p (oref object buf)))
            (kill-buffer (oref object buf))))))))

(ert-deftest slack-test-pinned-items-refreshes-same-buffer-to-empty ()
  (slack-test-setup
    (let* ((source (slack-create-message-buffer channel "" team))
           callbacks object emacs-buffer)
      (unwind-protect
          (cl-letf (((symbol-function 'slack-buffer-display) #'ignore)
                    ((symbol-function 'slack-pins-list)
                     (lambda (_room _team after-success
                                   &optional on-primary-page _on-error)
                       (push (cons after-success on-primary-page) callbacks))))
            (setq object (slack-buffer-display-pins-list source)
                  emacs-buffer (oref object buf))
            (let ((item (slack-test-pinned-item
                         channel team "1.000" "first pin")))
              (funcall (cdar callbacks) (list item))
              (funcall (caar callbacks) (list item)))
            (should (eq object (slack-buffer-display-pins-list source)))
            (should (eq emacs-buffer (oref object buf)))
            (funcall (cdar callbacks) nil)
            (funcall (caar callbacks) nil)
            (should-not (oref object items))
            (with-current-buffer emacs-buffer
              (goto-char (point-min))
              (should (search-forward "No Pinned Items" nil t))
              (goto-char (point-min))
              (should-not (search-forward "first pin" nil t))))
        (when (buffer-live-p emacs-buffer)
          (kill-buffer emacs-buffer))))))

(ert-deftest slack-test-pinned-items-failure-retries-in-place ()
  (slack-test-setup
    (let* ((source (slack-create-message-buffer channel "" team))
           (requests 0)
           on-error object emacs-buffer retry)
      (unwind-protect
          (cl-letf (((symbol-function 'slack-buffer-display) #'ignore)
                    ((symbol-function 'slack-pins-list)
                     (lambda (_room _team _after-success
                                   &optional _on-primary-page error)
                       (cl-incf requests)
                       (setq on-error error))))
            (setq object (slack-buffer-display-pins-list source)
                  emacs-buffer (oref object buf))
            (funcall on-error :error-thrown "offline")
            (should (eq 'failed
                        (slack-page-state-status
                         (slack-team-page-state
                          team (list 'pins channel-id)))))
            (with-current-buffer emacs-buffer
              (goto-char (point-min))
              (should (search-forward "offline" nil t))
              (setq retry slack-buffer-page-retry-function))
            (funcall retry)
            (should (= 2 requests))
            (should (eq emacs-buffer (oref object buf))))
        (when (buffer-live-p emacs-buffer)
          (kill-buffer emacs-buffer))))))

(ert-deftest slack-test-pinned-items-hydration-does-not-resurrect-killed-buffer ()
  (slack-test-setup
    (let* ((source (slack-create-message-buffer channel "" team))
           primary hydrated object emacs-buffer)
      (cl-letf (((symbol-function 'slack-buffer-display) #'ignore)
                ((symbol-function 'slack-pins-list)
                 (lambda (_room _team after-success
                               &optional on-primary-page _on-error)
                   (setq primary on-primary-page
                         hydrated after-success))))
        (setq object (slack-buffer-display-pins-list source)
              emacs-buffer (oref object buf))
        (let ((item (slack-test-pinned-item
                     channel team "1.000" "durable pin")))
          (funcall primary (list item))
          (with-current-buffer emacs-buffer
            (goto-char (point-min))
            (should (search-forward "durable pin" nil t)))
          (kill-buffer emacs-buffer)
          (funcall hydrated (list item)))
        (should-not (buffer-live-p emacs-buffer))
        (should-not (slack-buffer-find
                     'slack-pinned-items-buffer team channel))
        (let ((state (slack-team-page-state
                      team (list 'pins channel-id))))
          (should (eq 'ready (slack-page-state-status state)))
          (should (= 1 (length (slack-page-state-value state)))))))))

(ert-deftest slack-test-pinned-items-result-skips-replacement-buffer ()
  (slack-test-setup
    (let* ((source (slack-create-message-buffer channel "" team))
           primary hydrated object original replacement)
      (unwind-protect
          (cl-letf (((symbol-function 'slack-buffer-display) #'ignore)
                    ((symbol-function 'slack-pins-list)
                     (lambda (_room _team after-success
                                   &optional on-primary-page _on-error)
                       (setq primary on-primary-page
                             hydrated after-success))))
            (setq object (slack-buffer-display-pins-list source)
                  original (oref object buf)
                  replacement (generate-new-buffer
                               " *slack-pins-replacement*"))
            (oset object buf replacement)
            (with-current-buffer replacement
              (insert "replacement sentinel"))
            (let ((item (slack-test-pinned-item
                         channel team "1.000" "late pin")))
              (funcall primary (list item))
              (funcall hydrated (list item)))
            (with-current-buffer replacement
              (should (equal "replacement sentinel" (buffer-string)))))
        (when (buffer-live-p original)
          (kill-buffer original))
        (when (buffer-live-p replacement)
          (kill-buffer replacement))))))

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

(ert-deftest slack-test-saved-items-cold-open-displays-before-index ()
  (slack-test-setup
    (let (primary events)
      (cl-letf (((symbol-function 'slack-team-select) (lambda () team))
                ((symbol-function 'slack-team-ensure-conversations-loaded)
                 #'ignore)
                ((symbol-function 'slack-buffer-display)
                 (lambda (_buffer) (push 'display events)))
                ((symbol-function 'slack-stars-list-request)
                 (lambda (_team _cursor _ready _error on-primary-page)
                   (setq primary on-primary-page)
                   (push 'request events))))
        (slack-saved-items)
        (should (equal '(request display) events))
        (should (functionp primary))))))

(ert-deftest slack-test-saved-items-empty-index-reaches-ready ()
  (slack-test-setup
    (let ((empty-star (make-instance 'slack-star :items nil :cursor ""))
          request-primary
          request-hydrated
          object
          emacs-buffer)
      (unwind-protect
          (cl-letf (((symbol-function 'slack-team-select) (lambda () team))
                    ((symbol-function
                      'slack-team-ensure-conversations-loaded)
                     #'ignore)
                    ((symbol-function 'slack-buffer-display)
                     (lambda (displayed)
                       (setq object displayed
                             emacs-buffer (oref displayed buf))))
                    ((symbol-function 'slack-stars-list-request)
                     (lambda (_team _cursor after-success _on-error
                              on-primary-page)
                       (setq request-primary on-primary-page
                             request-hydrated after-success))))
            (slack-saved-items)
            (oset team star empty-star)
            (funcall request-primary empty-star empty-star)
            (should (eq 'loading
                        (slack-page-state-status
                         (slack-team-page-state team 'saved-items))))
            (funcall request-hydrated)
            (should (eq 'ready
                        (slack-page-state-status
                         (slack-team-page-state team 'saved-items))))
            (should (eq object
                        (slack-buffer-find 'slack-stars-buffer team)))
            (with-current-buffer emacs-buffer
              (goto-char (point-min))
              (should (search-forward "(no more items)" nil t))
              (goto-char (point-min))
              (should-not (search-forward "Loading Slack data" nil t))))
        (when (buffer-live-p emacs-buffer)
          (kill-buffer emacs-buffer))))))

(ert-deftest slack-test-saved-items-failure-retries-in-place ()
  (slack-test-setup
    (let* ((item (slack-test-star-item "1.000" channel-id))
           (stale-star (make-instance 'slack-star
                                      :items (list item)
                                      :cursor "cursor-1"))
           (empty-star (make-instance 'slack-star :items nil :cursor ""))
           (state (slack-team-page-state team 'saved-items))
           (object (slack-create-stars-buffer team))
           (emacs-buffer (slack-buffer-buffer object))
           request-primary
           request-hydrated
           request-error
           retry
           (request-count 0))
      (slack-page-state-store state stale-star "cursor-1" t)
      (oset team star stale-star)
      (unwind-protect
          (cl-letf (((symbol-function 'slack-buffer-display) #'ignore)
                    ((symbol-function 'slack-stars-list-request)
                     (lambda (_team _cursor after-success on-error
                              on-primary-page)
                       (cl-incf request-count)
                       (setq request-primary on-primary-page
                             request-hydrated after-success
                             request-error on-error))))
            (should (eq object (slack-stars-buffer--present team t)))
            (setq retry
                  (buffer-local-value
                   'slack-buffer-page-retry-function emacs-buffer))
            (funcall request-error "rate_limited")
            (should (eq 'failed (slack-page-state-status state)))
            (with-current-buffer emacs-buffer
              (goto-char (point-min))
              (should (search-forward "Saved message unavailable." nil t))
              (goto-char (point-min))
              (should (search-forward "Slack request failed: rate_limited"
                                      nil t))
              (should (search-forward "Retry" nil t)))
            (funcall retry)
            (should (= 2 request-count))
            (should (eq emacs-buffer (oref object buf)))
            (oset team star empty-star)
            (funcall request-primary empty-star empty-star)
            (funcall request-hydrated)
            (should (eq 'ready (slack-page-state-status state)))
            (should (eq empty-star (slack-page-state-value state)))
            (with-current-buffer emacs-buffer
              (goto-char (point-min))
              (should (search-forward "(no more items)" nil t))
              (goto-char (point-min))
              (should-not (search-forward "Slack request failed" nil t))))
        (when (buffer-live-p emacs-buffer)
          (kill-buffer emacs-buffer))))))

(ert-deftest slack-test-saved-items-reopen-renders-durable-page ()
  (slack-test-setup
    (let* ((item (slack-test-star-item "1.000" channel-id))
           (star (make-instance 'slack-star
                                :items (list item)
                                :cursor "cursor-1"))
           (state (slack-team-page-state team 'saved-items))
           (object (slack-create-stars-buffer team))
           (old-buffer (slack-buffer-buffer object))
           new-buffer
           requested)
      (slack-page-state-store state star "cursor-1" t)
      (oset team star star)
      (kill-buffer old-buffer)
      (unwind-protect
          (cl-letf (((symbol-function 'slack-buffer-display) #'ignore)
                    ((symbol-function 'slack-stars-list-request)
                     (lambda (&rest _args)
                       (setq requested t))))
            (setq object (slack-stars-buffer--present team t)
                  new-buffer (oref object buf))
            (should requested)
            (should (buffer-live-p new-buffer))
            (should-not (eq old-buffer new-buffer))
            (with-current-buffer new-buffer
              (goto-char (point-min))
              (should (search-forward "Loading saved message" nil t))
              (goto-char (point-min))
              (should (search-forward "Refreshing Slack data" nil t))))
        (when (buffer-live-p new-buffer)
          (kill-buffer new-buffer))))))

(ert-deftest slack-test-saved-items-refresh-keeps-buffer-alive ()
  (slack-test-setup
    (oset team star (make-instance 'slack-star))
    (let* ((buffer (slack-create-stars-buffer team))
           (emacs-buffer (slack-buffer-buffer buffer)))
      (unwind-protect
          (with-current-buffer emacs-buffer
            (cl-letf (((symbol-function 'slack-saved-items) #'ignore)
                      ((symbol-function 'slack-stars-buffer--present) #'ignore))
              (slack-saved-items-refresh-buffer))
            (should (buffer-live-p emacs-buffer))
            (should (eq emacs-buffer (oref buffer buf))))
        (when (buffer-live-p emacs-buffer)
          (kill-buffer emacs-buffer))))))

(ert-deftest slack-test-saved-items-primary-precedes-mixed-child-hydration ()
  (slack-test-setup
    (let* ((first (slack-test-star-item "1.000" channel-id))
           (second (slack-test-star-item "2.000" channel-id))
           (star (make-instance 'slack-star
                                :items (list first second)
                                :cursor "cursor-1"))
           (state (slack-team-page-state team 'saved-items))
           request-primary
           request-hydrated
           object
           emacs-buffer
           callbacks)
      (unwind-protect
          (cl-letf (((symbol-function 'slack-team-select) (lambda () team))
                    ((symbol-function
                      'slack-team-ensure-conversations-loaded)
                     #'ignore)
                    ((symbol-function 'slack-buffer-display)
                     (lambda (displayed)
                       (setq object displayed
                             emacs-buffer (oref displayed buf))))
                    ((symbol-function 'slack-stars-list-request)
                     (lambda (_team _cursor after-success _on-error
                              on-primary-page)
                       (setq request-primary on-primary-page
                             request-hydrated after-success)))
                    ((symbol-function 'slack-conversations-history)
                     (lambda (_room _team &rest args)
                       (push (cons (plist-get args :latest)
                                   (cons (plist-get args :after-success)
                                         (plist-get args :on-error)))
                             callbacks))))
            (slack-saved-items)
            (should (buffer-live-p emacs-buffer))
            (oset team star star)
            (funcall request-primary star star)
            (should (eq star (slack-page-state-value state)))
            (should (eq 'loading (slack-page-state-status state)))
            (should (eq object
                        (slack-buffer-find 'slack-stars-buffer team)))
            (with-current-buffer emacs-buffer
              (goto-char (point-min))
              (should (search-forward "Loading saved message" nil t)))
            (funcall request-hydrated)
            (let* ((first-request (assoc "1.000" callbacks))
                   (second-request (assoc "2.000" callbacks))
                   (message
                    (slack-message-create
                     (list :type "message"
                           :ts "1.000"
                           :text "hydrated saved body"
                           :user user-id
                           :channel channel-id)
                     team channel)))
              (funcall (cadr first-request) (list message))
              (should (eq 'loading (slack-page-state-status state)))
              (funcall (cddr second-request) "message unavailable"))
            (should (eq 'ready (slack-page-state-status state)))
            (should (eq object
                        (slack-buffer-find 'slack-stars-buffer team)))
            (with-current-buffer emacs-buffer
              (goto-char (point-min))
              (should (search-forward "hydrated saved body" nil t))
              (goto-char (point-min))
              (should (search-forward "Saved message unavailable." nil t))
              (goto-char (point-min))
              (should-not (search-forward "Loading saved message" nil t))))
        (when (buffer-live-p emacs-buffer)
          (kill-buffer emacs-buffer))))))

(ert-deftest slack-test-saved-items-hydration-skips-replaced-buffer ()
  (slack-test-setup
    (let* ((item (slack-test-star-item "1.000" channel-id))
           (star (make-instance 'slack-star :items (list item) :cursor ""))
           (state (slack-team-page-state team 'saved-items))
           request-primary
           request-hydrated
           hydration-done
           object
           old-buffer
           replacement)
      (unwind-protect
          (cl-letf (((symbol-function 'slack-team-select) (lambda () team))
                    ((symbol-function
                      'slack-team-ensure-conversations-loaded)
                     #'ignore)
                    ((symbol-function 'slack-buffer-display)
                     (lambda (displayed)
                       (setq object displayed
                             old-buffer (oref displayed buf))))
                    ((symbol-function 'slack-stars-list-request)
                     (lambda (_team _cursor after-success _on-error
                              on-primary-page)
                       (setq request-primary on-primary-page
                             request-hydrated after-success)))
                    ((symbol-function 'slack-stars--prefetch-messages)
                     (lambda (_items _team callback)
                       (setq hydration-done callback))))
            (slack-saved-items)
            (oset team star star)
            (funcall request-primary star star)
            (funcall request-hydrated)
            (kill-buffer old-buffer)
            (setq replacement (generate-new-buffer
                               " *slack-test-saved-replacement*"))
            (oset object buf replacement)
            (with-current-buffer replacement
              (insert "replacement sentinel"))
            (funcall hydration-done)
            (should (eq 'ready (slack-page-state-status state)))
            (should-not (buffer-live-p old-buffer))
            (with-current-buffer replacement
              (should (equal "replacement sentinel" (buffer-string)))))
        (when (buffer-live-p old-buffer)
          (kill-buffer old-buffer))
        (when (buffer-live-p replacement)
          (kill-buffer replacement))))))

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

(ert-deftest slack-test-channel-bookmarks-display-before-request ()
  (slack-test-setup
    (let (events success object emacs-buffer)
      (unwind-protect
          (cl-letf (((symbol-function 'slack-buffer-display)
                     (lambda (displayed)
                       (setq object displayed
                             emacs-buffer (oref displayed buf))
                       (push 'display events)))
                    ((symbol-function 'slack-bookmarks-request)
                     (lambda (requested-channel requested-team
                              &optional after-success _on-error)
                       (should (equal channel-id requested-channel))
                       (should (eq team requested-team))
                       (setq success after-success)
                       (push 'request events))))
            (let ((result (slack-show-channel-bookmarks channel-id team)))
              (should (eq object result)))
            (should (equal '(display request) (nreverse events)))
            (with-current-buffer emacs-buffer
              (goto-char (point-min))
              (should (search-forward "Loading Slack data" nil t)))
            (funcall success
                     '(:ok t :bookmarks
                       ((:title "Runbook"
                         :link "https://example.test/runbook"))))
            (should
             (eq 'ready
                 (slack-page-state-status
                  (slack-team-page-state
                   team (list 'channel-bookmarks channel-id)))))
            (with-current-buffer emacs-buffer
              (goto-char (point-min))
              (should (search-forward "Runbook" nil t))
              (goto-char (point-min))
              (should-not (search-forward "Loading Slack data" nil t))))
        (when (buffer-live-p emacs-buffer)
          (kill-buffer emacs-buffer))))))

(ert-deftest slack-test-channel-bookmarks-failure-retries-in-place ()
  (slack-test-setup
    (let ((requests 0) on-error object emacs-buffer retry)
      (unwind-protect
          (cl-letf (((symbol-function 'slack-buffer-display) #'ignore)
                    ((symbol-function 'slack-bookmarks-request)
                     (lambda (_channel-id _team
                              &optional _after-success error)
                       (cl-incf requests)
                       (setq on-error error))))
            (setq object (slack-show-channel-bookmarks channel-id team)
                  emacs-buffer (oref object buf))
            (funcall on-error "offline")
            (with-current-buffer emacs-buffer
              (goto-char (point-min))
              (should (search-forward "offline" nil t))
              (setq retry slack-buffer-page-retry-function))
            (funcall retry)
            (should (= 2 requests))
            (should (eq emacs-buffer (oref object buf))))
        (when (buffer-live-p emacs-buffer)
          (kill-buffer emacs-buffer))))))

(ert-deftest slack-test-channel-bookmarks-late-result-keeps-state-without-buffer ()
  (slack-test-setup
    (let (success object emacs-buffer)
      (cl-letf (((symbol-function 'slack-buffer-display) #'ignore)
                ((symbol-function 'slack-bookmarks-request)
                 (lambda (_channel-id _team
                          &optional after-success _on-error)
                   (setq success after-success))))
        (setq object (slack-show-channel-bookmarks channel-id team)
              emacs-buffer (oref object buf))
        (kill-buffer emacs-buffer)
        (funcall success
                 '(:ok t :bookmarks
                   ((:title "Runbook"
                     :link "https://example.test/runbook"))))
        (should-not (buffer-live-p emacs-buffer))
        (should-not
         (slack-buffer-find 'slack-channel-bookmarks-buffer team channel-id))
        (should
         (equal "Runbook"
                (plist-get
                 (car
                  (slack-page-state-value
                   (slack-team-page-state
                    team (list 'channel-bookmarks channel-id))))
                 :title)))))))

(ert-deftest slack-test-bookmarks-request-routes-api-and-transport-errors ()
  (slack-test-setup
    (dolist (failure '(api transport))
      (let (request errors success-called)
        (cl-letf (((symbol-function 'slack-request)
                   (lambda (created &rest _)
                     (setq request created))))
          (slack-bookmarks-request
           channel-id team
           (lambda (&rest _) (setq success-called t))
           (lambda (&rest reported) (push reported errors)))
          (if (eq failure 'api)
              (funcall (oref request success)
                       :data '(:ok :json-false :error "invalid_auth"))
            (funcall (oref request error)
                     :error-thrown '(error "offline")
                     :symbol-status 'error))
          (should (= 1 (length errors)))
          (should-not success-called))))))
;;; Remote dialog lifecycle

(defun slack-test-dialog-payload (title)
  "Return a valid remote dialog payload titled TITLE."
  (list :title title
        :callback_id "callback"
        :elements nil))

(defun slack-test-dialog-object (title)
  "Return a remote dialog object titled TITLE."
  (slack-dialog-create (slack-test-dialog-payload title)))

(defun slack-test-kill-dialog-buffer (team dialog-id)
  "Kill TEAM's DIALOG-ID buffer when it is live."
  (when-let* ((object (slack-buffer-find
                       'slack-dialog-buffer team dialog-id nil))
              (buffer (and (slot-boundp object 'buf) (oref object buf)))
              ((buffer-live-p buffer)))
    (kill-buffer buffer)))

(ert-deftest slack-test-dialog-get-displays-shell-before-request ()
  "A dialog shell is displayed before its schema request starts."
  (slack-test-setup
    (oset team id "T99999")
    (oset team name "TestTeam")
    (let (events request object buffer)
      (unwind-protect
          (cl-letf (((symbol-function 'slack-buffer-display)
                     (lambda (_object)
                       (setq events (append events '(display)))))
                    ((symbol-function 'slack-request)
                     (lambda (req &rest _args)
                       (setq request req
                             events (append events '(request))))))
            (slack-dialog-get "D1" team)
            (setq object (slack-buffer-find
                          'slack-dialog-buffer team "D1" nil)
                  buffer (and object (oref object buf)))
            (should (equal '(display request) events))
            (should (buffer-live-p buffer))
            (should (string-match-p "D1" (buffer-name buffer)))
            (should-not (oref object dialog))
            (should (with-current-buffer buffer
                      (string-match-p
                       "Loading Slack data"
                       (buffer-substring-no-properties
                        (point-min) (point-max)))))
            (funcall (oref request success)
                     :data (list :ok t
                                 :dialog (slack-test-dialog-payload "Loaded")))
            (should (eq buffer (oref object buf)))
            (should (equal "Loaded" (oref (oref object dialog) title)))
            (should (eq 'ready
                        (slack-page-state-status
                         (slack-team-page-state
                          team (list 'dialog "D1")))))
            (should (with-current-buffer buffer
                      (string-match-p
                       "Loaded"
                       (buffer-substring-no-properties
                        (point-min) (point-max))))))
        (slack-test-kill-dialog-buffer team "D1")))))

(ert-deftest slack-test-dialog-get-coalesces-duplicate-schema-requests ()
  "Opening one dialog twice while loading starts one schema request."
  (slack-test-setup
    (oset team id "T99999")
    (oset team name "TestTeam")
    (let ((request-count 0))
      (unwind-protect
          (cl-letf (((symbol-function 'slack-buffer-display) #'ignore)
                    ((symbol-function 'slack-request)
                     (lambda (_req &rest _args) (cl-incf request-count))))
            (slack-dialog-get "D1" team)
            (slack-dialog-get "D1" team)
            (should (= 1 request-count)))
        (slack-test-kill-dialog-buffer team "D1")))))

(ert-deftest slack-test-dialog-get-preserves-cached-schema-on-refresh-error ()
  "A failed refresh leaves the cached dialog visible with retry."
  (slack-test-setup
    (oset team id "T99999")
    (oset team name "TestTeam")
    (let* ((state (slack-team-page-state team (list 'dialog "D1")))
           (cached (slack-test-dialog-object "Cached"))
           displayed-text
           request)
      (slack-page-state-store state cached nil nil)
      (unwind-protect
          (cl-letf (((symbol-function 'slack-buffer-display)
                     (lambda (object)
                       (setq displayed-text
                             (with-current-buffer (oref object buf)
                               (buffer-substring-no-properties
                                (point-min) (point-max))))))
                    ((symbol-function 'slack-request)
                     (lambda (req &rest _args) (setq request req))))
            (slack-dialog-get "D1" team)
            (should (string-match-p "Cached" displayed-text))
            (should (eq 'refreshing (slack-page-state-status state)))
            (funcall (oref request error)
                     :error-thrown '(error "offline")
                     :symbol-status 'error
                     :response nil
                     :data nil)
            (should (eq 'failed (slack-page-state-status state)))
            (should (eq cached (slack-page-state-value state)))
            (let* ((object (slack-buffer-find
                            'slack-dialog-buffer team "D1" nil))
                   (text (with-current-buffer (oref object buf)
                           (buffer-substring-no-properties
                            (point-min) (point-max)))))
              (should (string-match-p "Cached" text))
              (should (string-match-p "Retry" text))))
        (slack-test-kill-dialog-buffer team "D1")))))

(ert-deftest slack-test-dialog-get-retry-rejects-stale-success ()
  "A response from a failed dialog generation cannot replace its retry."
  (slack-test-setup
    (oset team id "T99999")
    (oset team name "TestTeam")
    (let (requests object buffer state)
      (unwind-protect
          (cl-letf (((symbol-function 'slack-buffer-display) #'ignore)
                    ((symbol-function 'slack-request)
                     (lambda (req &rest _args)
                       (setq requests (append requests (list req))))))
            (slack-dialog-get "D1" team)
            (setq object (slack-buffer-find
                          'slack-dialog-buffer team "D1" nil)
                  buffer (oref object buf)
                  state (slack-team-page-state team (list 'dialog "D1")))
            (funcall (oref (car requests) error)
                     :error-thrown '(error "offline")
                     :symbol-status 'error
                     :response nil
                     :data nil)
            (with-current-buffer buffer
              (funcall slack-buffer-page-retry-function))
            (should (= 2 (length requests)))
            (funcall (oref (car requests) success)
                     :data (list :ok t
                                 :dialog (slack-test-dialog-payload "Stale")))
            (should (eq 'loading (slack-page-state-status state)))
            (should-not (slack-page-state-value state))
            (funcall (oref (cadr requests) success)
                     :data (list :ok t
                                 :dialog (slack-test-dialog-payload "Current")))
            (should (equal "Current"
                           (oref (slack-page-state-value state) title))))
        (slack-test-kill-dialog-buffer team "D1")))))

(ert-deftest slack-test-dialog-get-routes-api-and-schema-errors ()
  "Dialog API and schema failures both become visible page failures."
  (dolist (data (list (list :ok :json-false :error "invalid_auth")
                      (list :ok t
                            :dialog
                            (list :title "Broken"
                                  :callback_id "callback"
                                  :elements
                                  (list (list :type "unknown"))))))
    (slack-test-setup
      (oset team id "T99999")
      (oset team name "TestTeam")
      (let (request)
        (unwind-protect
            (cl-letf (((symbol-function 'slack-buffer-display) #'ignore)
                      ((symbol-function 'slack-request)
                       (lambda (req &rest _args) (setq request req))))
              (slack-dialog-get "D1" team)
              (funcall (oref request success) :data data)
              (let ((state (slack-team-page-state
                            team (list 'dialog "D1"))))
                (should (eq 'failed (slack-page-state-status state)))
                (should (slack-page-state-error state))))
          (slack-test-kill-dialog-buffer team "D1"))))))

(ert-deftest slack-test-dialog-get-late-schema-does-not-recreate-killed-buffer ()
  "A late schema commits durably without recreating its killed buffer."
  (slack-test-setup
    (oset team id "T99999")
    (oset team name "TestTeam")
    (let (request object buffer)
      (cl-letf (((symbol-function 'slack-buffer-display) #'ignore)
                ((symbol-function 'slack-request)
                 (lambda (req &rest _args) (setq request req))))
        (slack-dialog-get "D1" team)
        (setq object (slack-buffer-find
                      'slack-dialog-buffer team "D1" nil)
              buffer (oref object buf))
        (kill-buffer buffer)
        (funcall (oref request success)
                 :data (list :ok t
                             :dialog (slack-test-dialog-payload "Late")))
        (should-not (buffer-live-p buffer))
        (should-not (slack-buffer-find
                     'slack-dialog-buffer team "D1" nil))
        (should (equal
                 "Late"
                 (oref (slack-page-state-value
                        (slack-team-page-state team (list 'dialog "D1")))
                       title)))))))

(ert-deftest slack-test-dialog-get-reopens-from-durable-schema ()
  "Killing an unconsumed dialog keeps its schema for immediate reopen."
  (slack-test-setup
    (oset team id "T99999")
    (oset team name "TestTeam")
    (let* ((state (slack-team-page-state team (list 'dialog "D1")))
           (cached (slack-test-dialog-object "Cached"))
           old-object old-buffer displayed-text)
      (slack-page-state-store state cached nil nil)
      (setq old-object (slack-create-dialog-buffer "D1" cached team)
            old-buffer (slack-buffer-buffer old-object))
      (kill-buffer old-buffer)
      (should (eq state (gethash (list 'dialog "D1")
                                 (oref team page-states))))
      (unwind-protect
          (cl-letf (((symbol-function 'slack-buffer-display)
                     (lambda (object)
                       (setq displayed-text
                             (with-current-buffer (oref object buf)
                               (buffer-substring-no-properties
                                (point-min) (point-max))))))
                    ((symbol-function 'slack-request) #'ignore))
            (slack-dialog-get "D1" team)
            (should (string-match-p "Cached" displayed-text))
            (let ((new-object (slack-buffer-find
                               'slack-dialog-buffer team "D1" nil)))
              (should-not (eq old-object new-object))
              (should-not (eq old-buffer (oref new-object buf)))))
        (slack-test-kill-dialog-buffer team "D1")))))

(ert-deftest slack-test-dialog-submit-success-consumes-page-state ()
  "A successful dialog submission removes its consumed schema state."
  (slack-test-setup
    (oset team id "T99999")
    (oset team name "TestTeam")
    (let* ((key (list 'dialog "D1"))
           (state (slack-team-page-state team key))
           (dialog (slack-test-dialog-object "Submit"))
           (object (slack-create-dialog-buffer "D1" dialog team))
           (buffer (slack-buffer-buffer object))
           callback)
      (slack-page-state-store state dialog nil nil)
      (cl-letf (((symbol-function 'slack-dialog--submit)
                 (lambda (_dialog _id _team _params &optional after-success)
                   (setq callback after-success))))
        (slack-dialog-buffer--submit object))
      (should (eq state (gethash key (oref team page-states))))
      (funcall callback (list :ok t))
      (should-not (gethash key (oref team page-states)))
      (should-not (buffer-live-p buffer)))))

(ert-deftest slack-test-dialog-submit-error-retains-page-state ()
  "A rejected dialog submission retains its schema and live buffer."
  (slack-test-setup
    (oset team id "T99999")
    (oset team name "TestTeam")
    (let* ((key (list 'dialog "D1"))
           (state (slack-team-page-state team key))
           (dialog (slack-test-dialog-object "Submit"))
           (object (slack-create-dialog-buffer "D1" dialog team))
           (buffer (slack-buffer-buffer object))
           callback)
      (slack-page-state-store state dialog nil nil)
      (unwind-protect
          (progn
            (cl-letf (((symbol-function 'slack-dialog--submit)
                       (lambda (_dialog _id _team _params
                                &optional after-success)
                         (setq callback after-success))))
              (slack-dialog-buffer--submit object))
            (funcall callback
                     (list :ok t :error "Correct the fields"
                           :dialog_errors nil))
            (should (eq state (gethash key (oref team page-states))))
            (should (buffer-live-p buffer))
            (should (equal "Correct the fields"
                           (oref dialog error-message))))
        (when (buffer-live-p buffer) (kill-buffer buffer))))))

(ert-deftest slack-test-dialog-cancel-consumes-page-state ()
  "Explicit cancellation removes dialog state before closing the buffer."
  (slack-test-setup
    (oset team id "T99999")
    (oset team name "TestTeam")
    (let* ((key (list 'dialog "D1"))
           (state (slack-team-page-state team key))
           (dialog (slack-test-dialog-object "Cancel"))
           (object (slack-create-dialog-buffer "D1" dialog team))
           (buffer (slack-buffer-buffer object))
           notified)
      (slack-page-state-store state dialog nil nil)
      (cl-letf (((symbol-function 'slack-dialog-notify-cancel)
                 (lambda (&rest _args) (setq notified t))))
        (with-current-buffer buffer
          (slack-dialog-buffer-cancel)))
      (should notified)
      (should-not (gethash key (oref team page-states)))
      (should-not (buffer-live-p buffer)))))

(ert-deftest slack-test-dialog-loading-cancel-consumes-page-state ()
  "A schema-less dialog shell can be explicitly cancelled locally."
  (slack-test-setup
    (oset team id "T99999")
    (oset team name "TestTeam")
    (let* ((key (list 'dialog "D1"))
           (state (slack-team-page-state team key))
           (object (slack-create-dialog-buffer "D1" nil team))
           (buffer (slack-buffer-buffer object))
           notified)
      (slack-page-state-begin state)
      (cl-letf (((symbol-function 'slack-dialog-notify-cancel)
                 (lambda (&rest _args) (setq notified t))))
        (with-current-buffer buffer
          (slack-dialog-buffer-cancel)))
      (should-not notified)
      (should-not (gethash key (oref team page-states)))
      (should-not (buffer-live-p buffer)))))
;;; File-list lifecycle

(defun slack-test-file-list-file (id created &optional user)
  "Return a file-list test file identified by ID and CREATED.
USER defaults to the fixture user's id."
  (slack-file-create (slack-test-file-list-payload id created user)))

(defun slack-test-file-list-payload (id created &optional user)
  "Return a file-list API payload identified by ID and CREATED.
USER defaults to the fixture user's id."
  (list :id id
        :created created
        :name (concat id ".txt")
        :title (concat "File " id)
        :filetype "text"
        :mimetype "text/plain"
        :user (or user "U11111")
        :preview ""
        :permalink ""
        :public t
        :username ""
        :channels nil
        :groups nil
        :ims nil
        :timestamp created))

(defun slack-test-kill-file-list-buffer (team)
  "Kill TEAM's file-list buffer when it is live."
  (when-let* ((object (slack-buffer-find 'slack-file-list-buffer team))
              (buffer (and (slot-boundp object 'buf) (oref object buf)))
              ((buffer-live-p buffer)))
    (kill-buffer buffer)))

(ert-deftest slack-test-file-list-displays-before-request ()
  "The stable file-list shell is displayed before files.list starts."
  (slack-test-setup
    (oset team name "Test Team")
    (let (events)
      (unwind-protect
          (cl-letf (((symbol-function 'slack-team-select)
                     (lambda (&optional _no-default) team))
                    ((symbol-function 'slack-buffer-display)
                     (lambda (_object) (push 'display events)))
                    ((symbol-function 'slack-file-list-request)
                     (lambda (&rest _args) (push 'request events))))
            (slack-file-list)
            (should (equal '(request display) events)))
        (slack-test-kill-file-list-buffer team)))))

(ert-deftest slack-test-file-list-request-parses-primary-before-users ()
  "files.list publishes parsed files before missing-user hydration."
  (slack-test-setup
    (let (request hydration-success primary-args primary-files ready-args events)
      (cl-letf (((symbol-function 'slack-team-missing-user-ids)
                 (lambda (&rest _args) '("U-MISSING")))
                ((symbol-function 'slack-users-info-request)
                 (lambda (_ids _team &rest args)
                   (push 'hydration-request events)
                   (setq hydration-success
                         (plist-get args :after-success))))
                ((symbol-function 'slack-request)
                 (lambda (value &rest _args) (setq request value))))
        (slack-file-list-request
         team
         :on-primary-page
         (lambda (page pages files)
           (setq primary-args (list page pages)
                 primary-files files)
           (push 'primary events))
         :after-success
         (lambda (page pages)
           (setq ready-args (list page pages))
           (push 'ready events)))
        (funcall
         (oref request success)
         :data
         (list :ok t
               :files
               (list (list :id "F1" :created 100 :name "one.txt"
                           :title "One" :filetype "text" :user "U-MISSING"
                           :preview "" :permalink "" :public t
                           :username "" :channels nil :groups nil :ims nil
                           :timestamp 100))
               :paging (list :page 1 :pages 3)))
        (should (equal '(hydration-request primary) events))
        (should (equal '(1 3) primary-args))
        (should (equal '("F1") (mapcar #'slack-file-id primary-files)))
        (should-not (slack-file-find "F1" team))
        (should (functionp hydration-success))
        (funcall hydration-success)
        (should (equal '(ready hydration-request primary) events))
        (should (equal primary-args ready-args))))))

(ert-deftest slack-test-file-list-request-forwards-api-and-transport-errors ()
  "files.list failures skip primary and hydrated success callbacks."
  (slack-test-setup
    (dolist (failure '(api transport))
      (let (request primary-called ready-called errors)
        (cl-letf (((symbol-function 'slack-request)
                   (lambda (value &rest _args) (setq request value))))
          (slack-file-list-request
           team
           :on-primary-page (lambda (&rest _) (setq primary-called t))
           :after-success (lambda (&rest _) (setq ready-called t))
           :on-error (lambda (&rest args) (push args errors)))
          (if (eq failure 'api)
              (funcall (oref request success)
                       :data '(:ok :json-false :error "invalid_auth"))
            (funcall (oref request error)
                     :error-thrown '(error "offline")
                     :symbol-status 'error
                     :response nil
                     :data nil))
          (should (= 1 (length errors)))
          (should-not primary-called)
          (should-not ready-called))))))

(ert-deftest slack-test-file-list-primary-stores-durable-pagination ()
  "The first parsed file page is durable before identity hydration."
  (slack-test-setup
    (oset team name "Test Team")
    (let (request hydration-success)
      (unwind-protect
          (cl-letf (((symbol-function 'slack-team-select)
                     (lambda (&optional _no-default) team))
                    ((symbol-function 'slack-buffer-display) #'ignore)
                    ((symbol-function 'slack-team-missing-user-ids)
                     (lambda (&rest _args) '("U-MISSING")))
                    ((symbol-function 'slack-users-info-request)
                     (lambda (_ids _team &rest args)
                       (setq hydration-success
                             (plist-get args :after-success))))
                    ((symbol-function 'slack-request)
                     (lambda (value &rest _args) (setq request value))))
            (slack-file-list)
            (funcall
             (oref request success)
             :data
             (list :ok t
                   :files
                   (list (list :id "F1" :created 100 :name "one.txt"
                               :title "One" :filetype "text"
                               :user "U-MISSING" :preview "" :permalink ""
                               :public t :username "" :channels nil
                               :groups nil :ims nil :timestamp 100))
                   :paging (list :page 1 :pages 3)))
            (let ((state (slack-team-page-state team 'file-list)))
              (should (= 1 (plist-get (slack-page-state-value state) :page)))
              (should (= 3 (plist-get (slack-page-state-value state) :pages)))
              (should (equal 2 (slack-page-state-continuation state)))
              (should (slack-page-state-has-more state))
              (should (eq 'loading (slack-page-state-status state)))
              (should (slack-file-find "F1" team)))
            (funcall hydration-success)
            (should (eq 'ready
                        (slack-page-state-status
                         (slack-team-page-state team 'file-list)))))
        (slack-test-kill-file-list-buffer team)))))

(ert-deftest slack-test-file-list-reopen-renders-durable-page ()
  "Killing and reopening the file list retains its durable page."
  (slack-test-setup
    (oset team name "Test Team")
    (let* ((file (slack-test-file-list-file "F1" 100))
           (state (slack-team-page-state team 'file-list))
           first-buffer second-object second-buffer displayed-text)
      (slack-team-set-files team (list file))
      (slack-page-state-store
       state (list :files (list file) :page 1 :pages 3) 2 t)
      (unwind-protect
          (cl-letf (((symbol-function 'slack-team-select)
                     (lambda (&optional _no-default) team))
                    ((symbol-function 'slack-file-list-request) #'ignore)
                    ((symbol-function 'slack-buffer-display)
                     (lambda (object)
                       (setq second-object object
                             displayed-text
                             (with-current-buffer (oref object buf)
                               (buffer-substring-no-properties
                                (point-min) (point-max)))))))
            (slack-file-list)
            (setq first-buffer (oref second-object buf))
            (kill-buffer first-buffer)
            (slack-file-list)
            (setq second-buffer (oref second-object buf))
            (should-not (eq first-buffer second-buffer))
            (should (string-match-p "File F1" displayed-text))
            (should (= 1 (oref second-object page)))
            (should (= 3 (oref second-object pages)))
            (should (= 1 (plist-get (slack-page-state-value state) :page)))
            (should (= 3 (plist-get (slack-page-state-value state) :pages))))
        (slack-test-kill-file-list-buffer team)))))

(ert-deftest slack-test-file-list-coalesces-and-keeps-stable-live-buffer ()
  "Repeated opens share one request and one live file-list buffer."
  (slack-test-setup
    (oset team name "Test Team")
    (let ((requests 0) objects buffers)
      (unwind-protect
          (cl-letf (((symbol-function 'slack-team-select)
                     (lambda (&optional _no-default) team))
                    ((symbol-function 'slack-file-list-request)
                     (lambda (&rest _args) (cl-incf requests)))
                    ((symbol-function 'slack-buffer-display)
                     (lambda (object)
                       (push object objects)
                       (push (oref object buf) buffers))))
            (slack-file-list)
            (slack-file-list)
            (should (= 1 requests))
            (should (eq (car objects) (cadr objects)))
            (should (eq (car buffers) (cadr buffers)))
            (should (buffer-live-p (car buffers))))
        (slack-test-kill-file-list-buffer team)))))

(ert-deftest slack-test-file-list-reinvocation-refreshes-ready-buffer ()
  "Reinvoking the command refreshes ready data in the same live buffer."
  (slack-test-setup
    (oset team name "Test Team")
    (let* ((file (slack-test-file-list-file "F1" 100))
           (state (slack-team-page-state team 'file-list))
           (object (slack-create-file-list-buffer 1 1 team))
           (buffer (slack-buffer-buffer object))
           (requests 0)
           displayed)
      (slack-team-set-files team (list file))
      (slack-page-state-store
       state (list :files (list file) :page 1 :pages 1) nil nil)
      (unwind-protect
          (cl-letf (((symbol-function 'slack-team-select)
                     (lambda (&optional _no-default) team))
                    ((symbol-function 'slack-buffer-display)
                     (lambda (value) (setq displayed value)))
                    ((symbol-function 'slack-file-list-request)
                     (lambda (&rest _args) (cl-incf requests))))
            (slack-file-list)
            (should (= 1 requests))
            (should (eq object displayed))
            (should (eq buffer (oref displayed buf)))
            (should (eq 'refreshing (slack-page-state-status state))))
        (when (buffer-live-p buffer) (kill-buffer buffer))))))

(ert-deftest slack-test-file-list-stale-response-does-not-mutate-cache ()
  "A superseded first page cannot mutate the team file cache."
  (slack-test-setup
    (oset team name "Test Team")
    (let* ((first (slack-test-file-list-file "F1" 200))
           (state (slack-team-page-state team 'file-list))
           request)
      (slack-team-set-files team (list first))
      (unwind-protect
          (cl-letf (((symbol-function 'slack-team-select)
                     (lambda (&optional _no-default) team))
                    ((symbol-function 'slack-buffer-display) #'ignore)
                    ((symbol-function 'slack-request)
                     (lambda (value &rest _args) (setq request value))))
            (slack-file-list)
            (let ((replacement (slack-page-state-restart state)))
              (slack-page-state-commit
               state replacement
               (list :files (list first) :page 1 :pages 1) nil nil))
            (funcall
             (oref request success)
             :data
             (list :ok t
                   :files (list (slack-test-file-list-payload "F2" 100))
                   :paging (list :page 1 :pages 1)))
            (should (equal '("F1") (oref team file-ids)))
            (should (slack-file-find "F1" team))
            (should-not (slack-file-find "F2" team)))
        (slack-test-kill-file-list-buffer team)))))

(ert-deftest slack-test-file-list-page-preserves-shared-cache-objects ()
  "An accepted list page preserves unrelated and richer cached files."
  (slack-test-setup
    (let* ((rich (slack-test-file-list-file "F1" 200))
           (unrelated (slack-test-file-list-file "F-UNRELATED" 100))
           (content (make-instance 'slack-file-content :content "complete"))
           (state (slack-team-page-state team 'file-list))
           request)
      (oset rich content content)
      (slack-team-set-files team (list rich unrelated))
      (slack-page-state-store
       state (list :files (list rich) :page 1 :pages 1) nil nil)
      (unwind-protect
          (cl-letf (((symbol-function 'slack-team-select)
                     (lambda (&optional _no-default) team))
                    ((symbol-function 'slack-buffer-display) #'ignore)
                    ((symbol-function 'slack-request)
                     (lambda (value &rest _args) (setq request value))))
            (slack-file-list)
            (funcall
             (oref request success)
             :data
             (list :ok t
                   :files (list (slack-test-file-list-payload "F1" 200))
                   :paging (list :page 1 :pages 1)))
            (should (eq rich (slack-file-find "F1" team)))
            (should (eq content (oref rich content)))
            (should (eq unrelated (slack-file-find "F-UNRELATED" team)))
            (should (eq rich
                        (car (plist-get (slack-page-state-value state)
                                        :files)))))
        (slack-test-kill-file-list-buffer team)))))

(ert-deftest slack-test-file-list-refresh-reconciles-live-deletion ()
  "A stale refresh cannot resurrect a file deleted after it started."
  (slack-test-setup
    (let* ((file (slack-test-file-list-file "F1" 200))
           (state (slack-team-page-state team 'file-list))
           request)
      (slack-team-set-files team (list file))
      (slack-page-state-store
       state (list :files (list file) :page 1 :pages 1) nil nil)
      (unwind-protect
          (cl-letf (((symbol-function 'slack-team-select)
                     (lambda (&optional _no-default) team))
                    ((symbol-function 'slack-buffer-display) #'ignore)
                    ((symbol-function 'slack-request)
                     (lambda (value &rest _args) (setq request value))))
            (slack-file-list)
            (slack-ws-handle-file-deleted '(:file_id "F1") team)
            (funcall
             (oref request success)
             :data
             (list :ok t
                   :files (list (slack-test-file-list-payload "F1" 200))
                   :paging (list :page 1 :pages 1)))
            (should-not (slack-file-find "F1" team))
            (should-not (plist-get (slack-page-state-value state) :files)))
        (slack-test-kill-file-list-buffer team)))))

(ert-deftest slack-test-file-list-refresh-reconciles-live-creation ()
  "A stale refresh cannot erase a file created after it started."
  (slack-test-setup
    (let* ((old (slack-test-file-list-file "F-OLD" 100))
           (state (slack-team-page-state team 'file-list))
           list-request info-request)
      (slack-team-set-files team (list old))
      (slack-page-state-store
       state (list :files (list old) :page 1 :pages 1) nil nil)
      (unwind-protect
          (cl-letf (((symbol-function 'slack-team-select)
                     (lambda (&optional _no-default) team))
                    ((symbol-function 'slack-buffer-display) #'ignore)
                    ((symbol-function 'slack-request)
                     (lambda (value &rest _args)
                       (if (string= (oref value url) slack-file-list-url)
                           (setq list-request value)
                         (setq info-request value)))))
            (slack-file-list)
            (slack-ws-handle-file-created '(:file (:id "F-NEW")) team)
            (funcall
             (oref info-request success)
             :data
             (list :ok t
                   :file (slack-test-file-list-payload "F-NEW" 300)
                   :comments nil))
            (funcall
             (oref list-request success)
             :data
             (list :ok t
                   :files (list (slack-test-file-list-payload "F-OLD" 100))
                   :paging (list :page 1 :pages 1)))
            (should (slack-file-find "F-NEW" team))
            (should (member "F-NEW"
                            (mapcar #'slack-file-id
                                    (plist-get
                                     (slack-page-state-value state) :files)))))
        (slack-test-kill-file-list-buffer team)))))

(ert-deftest slack-test-file-created-request-rejects-later-deletion ()
  "Late files.info from a create event cannot undo a later deletion."
  (slack-test-setup
    (let* ((object (slack-create-file-list-buffer 1 1 team))
           (buffer (slack-buffer-buffer object))
           request)
      (unwind-protect
          (cl-letf (((symbol-function 'slack-request)
                     (lambda (value &rest _args) (setq request value))))
            (slack-ws-handle-file-created '(:file (:id "F1")) team)
            (slack-ws-handle-file-deleted '(:file_id "F1") team)
            (funcall
             (oref request success)
             :data
             (list :ok t
                   :file (slack-test-file-list-payload "F1" 200)
                   :comments nil))
            (should-not (slack-file-find "F1" team)))
        (when (buffer-live-p buffer) (kill-buffer buffer))))))

(ert-deftest slack-test-file-created-failure-releases-mutation-snapshot ()
  "A failed files.info request releases its mutation journal snapshot."
  (slack-test-setup
    (let* ((object (slack-create-file-list-buffer 1 1 team))
           (buffer (slack-buffer-buffer object))
           request)
      (unwind-protect
          (cl-letf (((symbol-function 'slack-request)
                     (lambda (value &rest _args) (setq request value))))
            (slack-ws-handle-file-created '(:file (:id "F1")) team)
            (should (= 1 (length (oref team file-list-mutation-snapshots))))
            (should (= 1 (hash-table-count (oref team file-list-mutations))))
            (funcall (oref request error) "offline")
            (should-not (oref team file-list-mutation-snapshots))
            (should (= 0 (hash-table-count (oref team file-list-mutations)))))
        (when (buffer-live-p buffer) (kill-buffer buffer))))))

(ert-deftest slack-test-file-unshared-dispatches-to-refresh-handler ()
  "A file unshare event is not dispatched as a file deletion."
  (slack-test-setup
    (let ((ws (make-instance 'slack-team-ws))
          (frame (make-websocket-frame
                  :opcode 'text :payload "frame" :completep t))
          refreshed deleted)
      (cl-letf (((symbol-function 'slack-request-parse-payload) #'identity)
                ((symbol-function 'slack-decode)
                 (lambda (_)
                   '(:type "file_unshared" :file_id "F1")))
                ((symbol-function 'slack-team-event-log-enabledp)
                 (lambda (_) nil))
                ((symbol-function 'slack-ws-handle-file-unshared)
                 (lambda (payload request-team)
                   (setq refreshed (list payload request-team))))
                ((symbol-function 'slack-ws-handle-file-deleted)
                 (lambda (&rest _) (setq deleted t))))
        (slack-ws-on-message ws frame team)
        (should (equal (car refreshed)
                       '(:type "file_unshared" :file_id "F1")))
        (should (eq team (cadr refreshed)))
        (should-not deleted)))))

(ert-deftest slack-test-file-unshared-refresh-reconciles-stale-list-response ()
  "An accessible unshared file survives and supersedes a stale list page."
  (slack-test-setup
    (let* ((old-payload (slack-test-file-list-payload "F1" 200))
           (_ (plist-put old-payload :channels '("C1")))
           (file (slack-file-create old-payload))
           (state (slack-team-page-state team 'file-list))
           list-request info-request)
      (slack-team-set-files team (list file))
      (slack-page-state-store
       state (list :files (list file) :page 1 :pages 1) nil nil)
      (unwind-protect
          (cl-letf (((symbol-function 'slack-team-select)
                     (lambda (&optional _no-default) team))
                    ((symbol-function 'slack-buffer-display) #'ignore)
                    ((symbol-function 'slack-request)
                     (lambda (request &rest _)
                       (if (string= (oref request url) slack-file-list-url)
                           (setq list-request request)
                         (setq info-request request)))))
            (slack-file-list)
            (slack-ws-handle-file-unshared '(:file_id "F1") team)
            (should (eq file (slack-file-find "F1" team)))
            (let ((fresh-payload (slack-test-file-list-payload "F1" 200)))
              (plist-put fresh-payload :channels nil)
              (plist-put fresh-payload :title "Unshared F1")
              (funcall (oref info-request success)
                       :data (list :ok t :file fresh-payload :comments nil)))
            (funcall
             (oref list-request success)
             :data
             (list :ok t
                   :files (list old-payload)
                   :paging (list :page 1 :pages 1)))
            (should (eq file (slack-file-find "F1" team)))
            (should-not (oref file channels))
            (should (equal "Unshared F1" (oref file title)))
            (should (eq file
                        (car (plist-get (slack-page-state-value state)
                                        :files))))
            (should-not (oref team file-list-mutation-snapshots))
            (should (= 0 (hash-table-count
                          (oref team file-list-mutations)))))
        (slack-test-kill-file-list-buffer team)))))

(ert-deftest slack-test-file-unshared-terminal-errors-remove-file ()
  "Explicit files.info inaccessibility removes an unshared file."
  (dolist (terminal-error
           '("access_denied" "file_deleted" "file_not_found" "not_visible"))
    (slack-test-setup
      (let* ((file (slack-test-file-list-file "F1" 200))
             (state (slack-team-page-state team 'file-list))
             request)
        (slack-team-set-files team (list file))
        (slack-page-state-store
         state (list :files (list file) :page 1 :pages 1) nil nil)
        (cl-letf (((symbol-function 'slack-request)
                   (lambda (created &rest _) (setq request created))))
          (slack-ws-handle-file-unshared '(:file_id "F1") team)
          (funcall (oref request success)
                   :data (list :ok :json-false :error terminal-error))
          (should-not (slack-file-find "F1" team))
          (should-not (plist-get (slack-page-state-value state) :files))
          (should-not (oref team file-list-mutation-snapshots))
          (should (= 0 (hash-table-count
                        (oref team file-list-mutations)))))))))

(ert-deftest slack-test-file-unshared-nonterminal-failure-preserves-file ()
  "A non-terminal files.info failure preserves an unshared file."
  (slack-test-setup
    (let* ((file (slack-test-file-list-file "F1" 200))
           (state (slack-team-page-state team 'file-list))
           request)
      (slack-team-set-files team (list file))
      (slack-page-state-store
       state (list :files (list file) :page 1 :pages 1) nil nil)
      (cl-letf (((symbol-function 'slack-request)
                 (lambda (created &rest _) (setq request created))))
        (slack-ws-handle-file-unshared '(:file_id "F1") team)
        (funcall (oref request success)
                 :data '(:ok :json-false :error "invalid_auth"))
        (should (eq file (slack-file-find "F1" team)))
        (should (equal (list file)
                       (plist-get (slack-page-state-value state) :files)))
        (should-not (oref team file-list-mutation-snapshots))
        (should (= 0 (hash-table-count
                      (oref team file-list-mutations))))))))

(ert-deftest slack-test-file-list-mutation-journal-prunes-by-oldest-snapshot ()
  "Mutation entries live exactly as long as an active request needs them."
  (slack-test-setup
    (let ((oldest (slack-file-list--start-mutation-snapshot team)))
      (slack-file-list--record-mutation team "F1" 'delete nil)
      (let ((newest (slack-file-list--start-mutation-snapshot team)))
        (slack-file-list--record-mutation team "F2" 'delete nil)
        (should (= 2 (hash-table-count (oref team file-list-mutations))))
        (slack-file-list--release-mutation-snapshot team oldest)
        (should-not (gethash "F1" (oref team file-list-mutations)))
        (should (gethash "F2" (oref team file-list-mutations)))
        (slack-file-list--release-mutation-snapshot team newest)
        (should-not (oref team file-list-mutation-snapshots))
        (should (= 0 (hash-table-count (oref team file-list-mutations))))))))

(ert-deftest slack-test-file-list-malformed-success-fails-without-cache-mutation ()
  "Malformed success data preserves cache and exposes an in-buffer retry."
  (slack-test-setup
    (oset team name "Test Team")
    (let* ((first (slack-test-file-list-file "F1" 200))
           (state (slack-team-page-state team 'file-list))
           request buffer signaled)
      (slack-team-set-files team (list first))
      (unwind-protect
          (cl-letf (((symbol-function 'slack-team-select)
                     (lambda (&optional _no-default) team))
                    ((symbol-function 'slack-buffer-display)
                     (lambda (object) (setq buffer (oref object buf))))
                    ((symbol-function 'slack-request)
                     (lambda (value &rest _args) (setq request value))))
            (slack-file-list)
            (condition-case error
                (funcall
                 (oref request success)
                 :data
                 (list :ok t
                       :files
                       (list (slack-test-file-list-payload "F2" 100) nil)
                       :paging (list :page 1 :pages 1)))
              (error (setq signaled error)))
            (should-not signaled)
            (should (eq 'failed (slack-page-state-status state)))
            (should (equal '("F1") (oref team file-ids)))
            (should (slack-file-find "F1" team))
            (should-not (slack-file-find "F2" team))
            (with-current-buffer buffer
              (goto-char (point-min))
              (should (search-forward "Slack request failed" nil t))
              (should (search-forward "Retry" nil t))))
        (slack-test-kill-file-list-buffer team)))))

(ert-deftest slack-test-file-list-late-page-does-not-recreate-killed-buffer ()
  "A late file page updates state without resurrecting its killed buffer."
  (slack-test-setup
    (oset team name "Test Team")
    (let (primary hydrated killed-buffer buffers-after-kill)
      (cl-letf (((symbol-function 'slack-team-select)
                 (lambda (&optional _no-default) team))
                ((symbol-function 'slack-buffer-display) #'ignore)
                ((symbol-function 'slack-file-list-request)
                 (cl-function
                  (lambda (_team &key on-primary-page after-success
                          &allow-other-keys)
                    (setq primary on-primary-page
                          hydrated after-success)))))
        (slack-file-list)
        (setq killed-buffer
              (oref (slack-buffer-find 'slack-file-list-buffer team) buf))
        (kill-buffer killed-buffer)
        (setq buffers-after-kill (buffer-list))
        (funcall primary 1 1 (list (slack-test-file-list-file "F1" 100)))
        (funcall hydrated 1 1)
        (should-not (buffer-live-p killed-buffer))
        (should (equal buffers-after-kill (buffer-list)))
        (should-not (slack-buffer-find 'slack-file-list-buffer team))
        (should (eq 'ready
                    (slack-page-state-status
                     (slack-team-page-state team 'file-list))))))))

(ert-deftest slack-test-file-list-late-update-keeps-state-without-recreation ()
  "A captured file update changes durable data without reviving a dead buffer."
  (slack-test-setup
    (oset team name "Test Team")
    (let* ((first (slack-test-file-list-file "F1" 200))
           (second (slack-test-file-list-file "F2" 100))
           (state (slack-team-page-state team 'file-list))
           (object (slack-create-file-list-buffer 1 1 team))
           (buffer (slack-buffer-buffer object))
           replacement replacement-buffer)
      (slack-team-set-files team (list first))
      (slack-page-state-store
       state (list :files (list first) :page 1 :pages 1) nil nil)
      (kill-buffer buffer)
      (setq replacement (slack-create-file-list-buffer 1 1 team)
            replacement-buffer (slack-buffer-buffer replacement))
      (with-current-buffer replacement-buffer
        (slack-file-list-buffer-render-page-state replacement state))
      (slack-team-set-files team (list second))
      (slack-buffer-update object second)
      (should-not (buffer-live-p buffer))
      (should (eq replacement
                  (slack-buffer-find 'slack-file-list-buffer team)))
      (should (eq replacement-buffer (oref replacement buf)))
      (with-current-buffer replacement-buffer
        (goto-char (point-min))
        (should (search-forward "File F2" nil t)))
      (should (equal '("F2" "F1")
                     (mapcar #'slack-file-id
                             (plist-get (slack-page-state-value state)
                                        :files))))
      (kill-buffer replacement-buffer))))

(ert-deftest slack-test-file-list-replace-does-not-insert-unlisted-cache-file ()
  "A detail update cannot leak an unrelated cached file into list state."
  (slack-test-setup
    (let* ((listed (slack-test-file-list-file "F1" 200))
           (unlisted (slack-test-file-list-file "F2" 100))
           (state (slack-team-page-state team 'file-list))
           (object (slack-create-file-list-buffer 1 1 team))
           (buffer (slack-buffer-buffer object)))
      (slack-team-set-files team (list listed unlisted))
      (slack-page-state-store
       state (list :files (list listed) :page 1 :pages 1) nil nil)
      (unwind-protect
          (progn
            (with-current-buffer buffer
              (slack-file-list-buffer-render-page-state object state))
            (slack-buffer-update object unlisted :replace t)
            (should (equal '("F1")
                           (mapcar #'slack-file-id
                                   (plist-get
                                    (slack-page-state-value state) :files)))))
        (when (buffer-live-p buffer) (kill-buffer buffer))))))

(ert-deftest slack-test-file-deleted-updates-cache-state-and-live-buffer ()
  "A file deletion removes every cached and rendered reference to its id."
  (slack-test-setup
    (oset team name "Test Team")
    (let* ((first (slack-test-file-list-file "F1" 200))
           (second (slack-test-file-list-file "F2" 100))
           (state (slack-team-page-state team 'file-list))
           (object (slack-create-file-list-buffer 1 1 team))
           (buffer (slack-buffer-buffer object)))
      (slack-team-set-files team (list first second))
      (slack-page-state-store
       state (list :files (list second first) :page 1 :pages 1) nil nil)
      (unwind-protect
          (with-current-buffer buffer
            (slack-file-list-buffer-render-page-state object state)
            (slack-ws-handle-file-deleted '(:file_id "F1") team)
            (should-not (slack-file-find "F1" team))
            (should (equal '("F2") (oref team file-ids)))
            (should (equal '("F2")
                           (mapcar #'slack-file-id
                                   (plist-get
                                    (slack-page-state-value state) :files))))
            (should-not (memq nil (slack-team-files team)))
            (goto-char (point-min))
            (should-not (search-forward "File F1" nil t))
            (goto-char (point-min))
            (should (search-forward "File F2" nil t)))
        (when (buffer-live-p buffer) (kill-buffer buffer))))))

(ert-deftest slack-test-file-list-failure-renders-retry-in-place ()
  "A file-list failure is visible and retry reuses the exact buffer."
  (slack-test-setup
    (oset team name "Test Team")
    (let ((requests 0) error-callback object buffer)
      (unwind-protect
          (cl-letf (((symbol-function 'slack-team-select)
                     (lambda (&optional _no-default) team))
                    ((symbol-function 'slack-buffer-display)
                     (lambda (value)
                       (setq object value
                             buffer (oref value buf))))
                    ((symbol-function 'slack-file-list-request)
                     (cl-function
                      (lambda (_team &key on-error &allow-other-keys)
                        (cl-incf requests)
                        (setq error-callback on-error)))))
            (slack-file-list)
            (funcall error-callback "offline")
            (with-current-buffer buffer
              (goto-char (point-min))
              (should (search-forward "Slack request failed" nil t))
              (should (search-forward "Retry" nil t))
              (slack-buffer-page-retry))
            (should (= 2 requests))
            (should (eq buffer (oref object buf)))
            (should (eq 'loading
                        (slack-page-state-status
                         (slack-team-page-state team 'file-list)))))
        (slack-test-kill-file-list-buffer team)))))

(ert-deftest slack-test-file-list-failure-releases-mutation-snapshot ()
  "A failed list request releases its mutation journal snapshot."
  (slack-test-setup
    (let (error-callback)
      (unwind-protect
          (cl-letf (((symbol-function 'slack-team-select)
                     (lambda (&optional _no-default) team))
                    ((symbol-function 'slack-buffer-display) #'ignore)
                    ((symbol-function 'slack-file-list-request)
                     (cl-function
                      (lambda (_team &key on-error &allow-other-keys)
                        (setq error-callback on-error)))))
            (slack-file-list)
            (should (= 1 (length (oref team file-list-mutation-snapshots))))
            (slack-file-list--record-mutation team "F1" 'delete nil)
            (funcall error-callback "offline")
            (should-not (oref team file-list-mutation-snapshots))
            (should (= 0 (hash-table-count (oref team file-list-mutations)))))
        (slack-test-kill-file-list-buffer team)))))

(ert-deftest slack-test-file-list-load-more-updates-same-state ()
  "File-list load-more appends its primary page to the durable state."
  (slack-test-setup
    (oset team name "Test Team")
    (let* ((first (slack-test-file-list-file "F1" 200))
           (second (slack-test-file-list-file "F2" 100))
           (state (slack-team-page-state team 'file-list))
           (object (slack-create-file-list-buffer 1 3 team))
           buffer primary hydrated requested-page
           (requests 0))
      (slack-team-set-files team (list first))
      (slack-page-state-store
       state (list :files (list first) :page 1 :pages 3) 2 t)
      (setq buffer (slack-buffer-buffer object))
      (unwind-protect
          (cl-letf (((symbol-function 'slack-file-list-request)
                     (cl-function
                      (lambda (_team &key page on-primary-page after-success
                              &allow-other-keys)
                        (cl-incf requests)
                        (setq requested-page page
                              primary on-primary-page
                              hydrated after-success)))))
            (with-current-buffer buffer
              (slack-file-list-buffer-render-page-state object state)
              (slack-buffer-load-more object)
              (slack-buffer-load-more object)
              (should slack-buffer--loading-more-p))
            (should (= 1 requests))
            (should (equal "2" requested-page))
            (funcall primary 2 3 (list second))
            (let ((value (slack-page-state-value state)))
              (should (= 2 (plist-get value :page)))
              (should (= 3 (plist-get value :pages)))
              (should (equal '("F2" "F1")
                             (mapcar #'slack-file-id
                                     (plist-get value :files))))
              (should (equal 3 (slack-page-state-continuation state)))
              (should (slack-page-state-has-more state)))
            (funcall hydrated 2 3)
            (with-current-buffer buffer
              (should-not slack-buffer--loading-more-p)
              (goto-char (point-min))
              (should (search-forward "File F2" nil t))))
        (when (buffer-live-p buffer) (kill-buffer buffer))))))

(ert-deftest slack-test-file-list-load-more-reconciles-cache-and-deletion ()
  "Load-more preserves shared cache data and rejects a later deletion."
  (slack-test-setup
    (let* ((first (slack-test-file-list-file "F1" 300))
           (deleted (slack-test-file-list-file "F2" 200))
           (unrelated (slack-test-file-list-file "F-UNRELATED" 100))
           (state (slack-team-page-state team 'file-list))
           (object (slack-create-file-list-buffer 1 2 team))
           (buffer (slack-buffer-buffer object))
           request)
      (slack-team-set-files team (list first deleted unrelated))
      (slack-page-state-store
       state (list :files (list first) :page 1 :pages 2) 2 t)
      (unwind-protect
          (cl-letf (((symbol-function 'slack-request)
                     (lambda (value &rest _args) (setq request value))))
            (with-current-buffer buffer
              (slack-file-list-buffer-render-page-state object state)
              (slack-buffer-load-more object))
            (slack-ws-handle-file-deleted '(:file_id "F2") team)
            (funcall
             (oref request success)
             :data
             (list :ok t
                   :files (list (slack-test-file-list-payload "F2" 200))
                   :paging (list :page 2 :pages 2)))
            (should-not (slack-file-find "F2" team))
            (should (eq unrelated (slack-file-find "F-UNRELATED" team)))
            (should (equal '("F1")
                           (mapcar #'slack-file-id
                                   (plist-get
                                    (slack-page-state-value state) :files)))))
        (when (buffer-live-p buffer) (kill-buffer buffer))))))

(ert-deftest slack-test-file-list-load-more-rejects-stale-state ()
  "A superseded file-list page cannot extend state or mutate its buffer."
  (slack-test-setup
    (oset team name "Test Team")
    (let* ((first (slack-test-file-list-file "F1" 200))
           (second (slack-test-file-list-file "F2" 100))
           (state (slack-team-page-state team 'file-list))
           (object (slack-create-file-list-buffer 1 2 team))
           buffer primary)
      (slack-team-set-files team (list first))
      (slack-page-state-store
       state (list :files (list first) :page 1 :pages 2) 2 t)
      (setq buffer (slack-buffer-buffer object))
      (unwind-protect
          (cl-letf (((symbol-function 'slack-file-list-request)
                     (cl-function
                      (lambda (_team &key on-primary-page &allow-other-keys)
                        (setq primary on-primary-page)))))
            (with-current-buffer buffer
              (slack-file-list-buffer-render-page-state object state)
              (slack-buffer-load-more object))
            (let ((replacement-generation (slack-page-state-restart state)))
              (slack-page-state-commit
               state replacement-generation
               (list :files (list first) :page 1 :pages 1) nil nil))
            (funcall primary 2 2 (list second))
            (should (= 1 (plist-get (slack-page-state-value state) :page)))
            (should-not (slack-page-state-continuation state))
            (with-current-buffer buffer
              (goto-char (point-min))
              (should-not (search-forward "File F2" nil t))
              (should-not slack-buffer--loading-more-p)))
        (when (buffer-live-p buffer) (kill-buffer buffer))))))

(ert-deftest slack-test-file-list-load-more-rejects-live-replaced-owner ()
  "A live replaced owner cannot publish its late file page."
  (slack-test-setup
    (oset team name "Test Team")
    (let* ((first (slack-test-file-list-file "F1" 200))
           (state (slack-team-page-state team 'file-list))
           (object (slack-create-file-list-buffer 1 2 team))
           (original (slack-buffer-buffer object))
           (replacement (generate-new-buffer
                         " *slack-test-file-list-replacement*"))
           request)
      (slack-team-set-files team (list first))
      (slack-page-state-store
       state (list :files (list first) :page 1 :pages 2) 2 t)
      (unwind-protect
          (cl-letf (((symbol-function 'slack-request)
                     (lambda (value &rest _args) (setq request value))))
            (with-current-buffer original
              (slack-file-list-buffer-render-page-state object state)
              (slack-buffer-load-more object))
            (oset object buf replacement)
            (with-current-buffer replacement
              (insert "replacement sentinel"))
            (funcall
             (oref request success)
             :data
             (list :ok t
                   :files (list (slack-test-file-list-payload "F2" 100))
                   :paging (list :page 2 :pages 2)))
            (should (equal '("F1")
                           (mapcar #'slack-file-id
                                   (plist-get
                                    (slack-page-state-value state) :files))))
            (should (equal '("F1") (oref team file-ids)))
            (with-current-buffer replacement
              (should (equal "replacement sentinel" (buffer-string))))
            (with-current-buffer original
              (should-not slack-buffer--loading-more-p)))
        (when (buffer-live-p original) (kill-buffer original))
        (when (buffer-live-p replacement) (kill-buffer replacement))))))

(ert-deftest slack-test-file-list-load-more-killed-origin-commits-durable-page ()
  "A killed origin still allows its accepted page to update durable state."
  (slack-test-setup
    (oset team name "Test Team")
    (let* ((first (slack-test-file-list-file "F1" 200))
           (state (slack-team-page-state team 'file-list))
           (object (slack-create-file-list-buffer 1 2 team))
           (original (slack-buffer-buffer object))
           request buffers-after-kill)
      (slack-team-set-files team (list first))
      (slack-page-state-store
       state (list :files (list first) :page 1 :pages 2) 2 t)
      (cl-letf (((symbol-function 'slack-request)
                 (lambda (value &rest _args) (setq request value))))
        (with-current-buffer original
          (slack-file-list-buffer-render-page-state object state)
          (slack-buffer-load-more object))
        (kill-buffer original)
        (setq buffers-after-kill (buffer-list))
        (funcall
         (oref request success)
         :data
         (list :ok t
               :files (list (slack-test-file-list-payload "F2" 100))
               :paging (list :page 2 :pages 2)))
        (should (equal '("F2" "F1")
                       (mapcar #'slack-file-id
                               (plist-get
                                (slack-page-state-value state) :files))))
        (should (equal '("F1" "F2") (oref team file-ids)))
        (should-not (buffer-live-p original))
        (should (equal buffers-after-kill (buffer-list)))))))

(ert-deftest slack-test-all-request-backed-entry-points-display-before-request ()
  "Every stable request-backed entry point displays before starting its request."
  (slack-test-setup
    (oset team mark-as-read-immediately nil)
    (let* ((source (slack-create-message-buffer channel "" team))
           (deep-link-channel
            (make-instance 'slack-channel
                           :id "C-DEEP-LINK"
                           :name "DeepLinkChannel"))
           (thread-ts "1710000000.000100")
           (deep-link-ts "1710000000.000300")
           (parent (make-instance 'slack-message
                                  :type "message"
                                  :channel channel-id
                                  :ts thread-ts
                                  :thread_ts thread-ts
                                  :text "parent"
                                  :reactions nil))
           displayed-objects
           events
           (rows
            (list
             (cons
              'room-history
              (lambda ()
                (cl-letf (((symbol-function 'slack-conversations-history)
                           (lambda (&rest _args) (push 'request events))))
                  (slack-room-display channel team))))
             (cons
              'room-deep-link
              (lambda ()
                (cl-letf (((symbol-function 'slack-conversations-history)
                           (lambda (&rest _args) (push 'request events))))
                  (slack-open-message
                   team deep-link-channel "1710000000.000200" nil))))
             (cons
              'existing-thread
              (lambda ()
                (cl-letf (((symbol-function 'slack-thread-replies)
                           (lambda (&rest _args) (push 'request events))))
                  (slack-thread-show-messages parent channel team))))
             (cons
              'thread-deep-link
              (lambda ()
                (cl-letf (((symbol-function 'slack-thread-replies)
                           (lambda (&rest _args) (push 'request events))))
                  (slack-open-message
                   team channel deep-link-ts deep-link-ts deep-link-ts))))
             (cons
              'activity-feed
              (lambda ()
                (cl-letf (((symbol-function 'slack-activity-feed--selected-team)
                           (lambda () team))
                          ((symbol-function 'slack-activity-feed-request)
                           (lambda (&rest _args) (push 'request events))))
                  (slack-activity-feed-show))))
             (cons
              'saved-items
              (lambda ()
                (cl-letf (((symbol-function 'slack-team-select)
                           (lambda (&optional _no-default) team))
                          ((symbol-function
                            'slack-team-ensure-conversations-loaded)
                           #'ignore)
                          ((symbol-function 'slack-stars-list-request)
                           (lambda (&rest _args) (push 'request events))))
                  (slack-saved-items))))
             (cons
              'file-list
              (lambda ()
                (cl-letf (((symbol-function 'slack-team-select)
                           (lambda (&optional _no-default) team))
                          ((symbol-function 'slack-file-list-request)
                           (lambda (&rest _args) (push 'request events))))
                  (slack-file-list))))
             (cons
              'message-search
              (lambda ()
                (cl-letf (((symbol-function 'slack-search-query-params)
                           (lambda (&optional _query)
                             (list team "scope messages" "timestamp" "desc")))
                          ((symbol-function 'slack-search-request)
                           (lambda (&rest _args) (push 'request events))))
                  (slack-search-from-messages nil))))
             (cons
              'file-search
              (lambda ()
                (cl-letf (((symbol-function 'slack-search-query-params)
                           (lambda (&optional _query)
                             (list team "scope files" "timestamp" "desc")))
                          ((symbol-function 'slack-search-request)
                           (lambda (&rest _args) (push 'request events))))
                  (slack-search-from-files))))
             (cons
              'all-threads
              (lambda ()
                (cl-letf (((symbol-function 'slack-team-select)
                           (lambda (&optional _no-default) team))
                          ((symbol-function
                            'slack-subscriptions-thread-get-view)
                           (lambda (&rest _args) (push 'request events))))
                  (slack-all-threads))))
             (cons
              'scheduled-messages
              (lambda ()
                (cl-letf (((symbol-function 'slack-scheduled-messages--team)
                           (lambda (&optional _selected) team))
                          ((symbol-function
                            'slack-list-scheduled-messages-request)
                           (lambda (&rest _args) (push 'request events))))
                  (slack-scheduled-messages-show team))))
             (cons
              'pinned-items
              (lambda ()
                (cl-letf (((symbol-function 'slack-pins-list)
                           (lambda (&rest _args) (push 'request events))))
                  (slack-buffer-display-pins-list source))))
             (cons
              'file-detail
              (lambda ()
                (cl-letf (((symbol-function 'slack-file-request-info)
                           (lambda (&rest _args) (push 'request events))))
                  (slack-buffer-display-file source "F-SCOPE"))))
             (cons
              'remote-dialog
              (lambda ()
                (cl-letf (((symbol-function 'slack-dialog-get-request)
                           (lambda (&rest _args) (push 'request events))))
                  (slack-dialog-get "D-SCOPE" team))))
             (cons
              'channel-bookmarks
              (lambda ()
                (cl-letf (((symbol-function 'slack-bookmarks-request)
                           (lambda (&rest _args) (push 'request events))))
                  (slack-show-channel-bookmarks channel-id team)))))))
      (puthash (oref deep-link-channel id)
               deep-link-channel (oref team channels))
      (slack-room-set-messages channel (list parent) team)
      (unwind-protect
          (cl-letf (((symbol-function 'slack-buffer-display)
                     (lambda (object)
                       (push object displayed-objects)
                       (push 'display events))))
            (should (= 15 (length rows)))
            (dolist (row rows)
              (setq events nil)
              (funcall (cdr row))
              (should (equal '(display request) (nreverse events)))))
        (dolist (object (cons source displayed-objects))
          (when (and (eieio-object-p object)
                     (slot-boundp object 'buf)
                     (buffer-live-p (oref object buf)))
            (kill-buffer (oref object buf))))))))

(ert-deftest slack-test-initial-page-adapters-route-normalization-errors ()
  "Malformed successful responses reach each adapter's error continuation."
  (slack-test-setup
    (let* ((message-search
            (make-instance
             'slack-search-result
             :query "needle" :sort "timestamp" :sort-dir "desc"
             :total 0 :matches nil
             :pagination (slack-test-search-pagination 0 0 0 0)))
           (file-search
            (make-instance
             'slack-file-search-result
             :query "needle" :sort "timestamp" :sort-dir "desc"
             :total 0 :matches nil
             :pagination (slack-test-search-pagination 0 0 0 0)))
           (cases
            (list
             (list
              'scheduled
              (lambda (ready error)
                (funcall
                 (slack-scheduled-messages-buffer--page-loader team)
                 1 ready error))
              '(:ok t :drafts ((:id "D1" :date_scheduled "tomorrow"))))
             (list
              'pins
              (lambda (ready error)
                (slack-pins-list channel team ready nil error))
              '(:ok t :items ((:type 7))))
             (list
              'message-search
              (lambda (ready error)
                (slack-search-request message-search ready team 1 error))
              (list :ok t :query "needle"
                    :messages
                    (list :total 1
                          :matches
                          (list (list :channel nil :user user-id
                                      :text "broken" :ts "1.000"))
                          :pagination
                          (list :total_count 1 :page 1 :per_page 20
                                :page_count 1 :first 1 :last 1))))
             (list
              'file-search
              (lambda (ready error)
                (slack-search-request file-search ready team 1 error))
              '(:ok t :query "needle"
                :files
                (:total 1 :matches ((:id "F1" :mimetype 4))
                 :pagination
                 (:total_count 1 :page 1 :per_page 20
                  :page_count 1 :first 1 :last 1))))
             (list
              'saved-items
              (lambda (ready error)
                (slack-stars-list-request team nil ready error))
              (list :ok t
                    :saved_items
                    (list
                     (list :item_id channel-id :item_type "message"
                           :message
                           (list :type "message" :subtype 4
                                 :user user-id :text "broken"
                                 :ts "1.000")))
                    :response_metadata (list :next_cursor "")))
             (list
              'replies
              (lambda (ready error)
                (slack-conversations-replies
                 channel "1.000" team
                 :after-success ready :on-error error))
              (list :ok t :messages
                    (list (list :type "message" :subtype 4
                                :user user-id :text "broken"
                                :ts "1.000"))))
             (list
              'history
              (lambda (ready error)
                (slack-conversations-history
                 channel team :after-success ready :on-error error))
              (list :ok t :messages
                    (list (list :type "message" :subtype 4
                                :user user-id :text "broken"
                                :ts "1.000"))))))
           escaped missing-error published no-request)
      (dolist (case cases)
        (let ((name (nth 0 case))
              (start (nth 1 case))
              (payload (nth 2 case))
              request errors ready-called)
          (cl-letf (((symbol-function 'slack-request)
                     (lambda (created &rest _args)
                       (setq request created))))
            (funcall start
                     (lambda (&rest _args) (setq ready-called t))
                     (lambda (&rest values) (push values errors)))
            (if request
                (let ((raised
                       (condition-case response-error
                           (progn
                             (funcall (oref request success) :data payload)
                             nil)
                         (error response-error))))
                  (when raised (push name escaped)))
              (push name no-request)))
          (unless (= 1 (length errors))
            (push name missing-error))
          (when ready-called (push name published))))
      (should-not no-request)
      (should-not escaped)
      (should-not missing-error)
      (should-not published))))

(ert-deftest slack-test-saved-items-normalization-failure-preserves-caches ()
  "Rejected saved-item responses do not partially publish cache changes."
  (slack-test-setup
    (let* ((original-star
            (make-instance 'slack-star :items nil :cursor "original"))
           (valid-item
            (list :item_id channel-id :item_type "message"
                  :message
                  (list :type "message" :user user-id :text "valid"
                        :ts "9.001")))
           (malformed-item
            (list :item_id channel-id :item_type "message"
                  :message
                  (list :type "message" :subtype 4 :user user-id
                        :text "broken" :ts "9.002")))
           request errors)
      (oset team star original-star)
      (cl-letf (((symbol-function 'slack-request)
                 (lambda (created &rest _args) (setq request created))))
        (slack-stars-list-request
         team nil #'ignore
         (lambda (&rest values) (push values errors)))
        (funcall
         (oref request success)
         :data
         (list :ok t :saved_items (list valid-item malformed-item)
               :response_metadata (list :next_cursor ""))))
      (should (= 1 (length errors)))
      (should (eq original-star (oref team star)))
      (should-not (slack-room-find-message channel "9.001"))
      (setq request nil
            errors nil)
      (cl-letf (((symbol-function 'slack-request)
                 (lambda (created &rest _args) (setq request created)))
                ((symbol-function 'slack-team-missing-user-ids)
                 (lambda (&rest _args) (error "user normalization failed"))))
        (slack-stars-list-request
         team nil #'ignore
         (lambda (&rest values) (push values errors)))
        (funcall
         (oref request success)
         :data
         (list :ok t :saved_items (list valid-item)
               :response_metadata (list :next_cursor ""))))
      (should (= 1 (length errors)))
      (should (eq original-star (oref team star)))
      (should-not (slack-room-find-message channel "9.001")))))

(ert-deftest slack-test-saved-items-cache-publication-prepares-all-rooms ()
  "Embedded saved messages publish only after every room update is valid."
  (slack-test-setup
    (let* ((other-channel
            (make-instance 'slack-channel :id "C22222" :name "Other"))
           (original-star
            (make-instance 'slack-star :items nil :cursor "original"))
           (first-messages (oref channel messages))
           (first-revisions (oref channel message-revisions))
           (second-messages (oref other-channel messages))
           (second-revisions (oref other-channel message-revisions))
           (first (make-instance 'slack-message
                                 :type "message" :channel channel-id
                                 :ts "9.001" :reactions nil))
           (second (make-instance 'slack-message
                                  :type "message" :channel "C22222"
                                  :ts "9.002" :reactions nil))
           (original-ts (symbol-function 'slack-ts)))
      (puthash (oref other-channel id) other-channel (oref team channels))
      (oset team star original-star)
      (cl-letf (((symbol-function 'slack-ts)
                 (lambda (message)
                   (if (eq message second)
                       (error "second room publication failed")
                     (funcall original-ts message)))))
        (should-error
         (slack-star-cache-embedded-messages
          (list (cons channel first) (cons other-channel second)) team)))
      (should-not (slack-room-find-message channel "9.001"))
      (should-not (slack-room-find-message other-channel "9.002"))
      (should (eq first-messages (oref channel messages)))
      (should (eq first-revisions (oref channel message-revisions)))
      (should (eq second-messages (oref other-channel messages)))
      (should (eq second-revisions (oref other-channel message-revisions)))
      (should (eq original-star (oref team star)))
      (should-not (oref channel message-ids))
      (should-not (oref other-channel message-ids))
      (should (= 0 (oref channel message-revision)))
      (should (= 0 (oref other-channel message-revision))))))

(ert-deftest slack-test-initial-page-adapters-preserve-consumer-errors ()
  "Adapter error handlers do not claim errors raised by page consumers."
  (slack-test-setup
    (let* ((message-search
            (make-instance
             'slack-search-result
             :query "needle" :sort "timestamp" :sort-dir "desc"
             :total 0 :matches nil
             :pagination (slack-test-search-pagination 0 0 0 0)))
           (state (slack-team-page-state team 'consumer-error-test))
           (activity-buffer
            (slack-create-activity-feed-buffer
             (make-instance 'slack-activity-feed) team))
           (cases
            (list
             (list
              'scheduled
              (lambda (consumer error)
                (funcall
                 (slack-scheduled-messages-buffer--page-loader team)
                 1 consumer error))
              '(:ok t :drafts nil))
             (list
              'pins
              (lambda (consumer error)
                (slack-pins-list channel team #'ignore consumer error))
              '(:ok t :items nil))
             (list
              'search
              (lambda (consumer error)
                (slack-search-request
                 message-search #'ignore team 1 error consumer))
              (list :ok t :query "needle"
                    :messages
                    (list :total 0 :matches nil
                          :pagination
                          (list :total_count 0 :page 1 :per_page 20
                                :page_count 1 :first 0 :last 0))))
             (list
              'saved-items
              (lambda (consumer error)
                (slack-stars-list-request
                 team nil #'ignore error consumer))
              '(:ok t :saved_items nil
                :response_metadata (:next_cursor "")))
             (list
              'replies
              (lambda (consumer error)
                (slack-conversations-replies
                 channel "1.000" team :after-success #'ignore
                 :on-primary-page consumer :on-error error))
              '(:ok t :messages nil))
             (list
              'history
              (lambda (consumer error)
                (slack-conversations-history
                 channel team :after-success #'ignore
                 :on-primary-page consumer :on-error error))
              '(:ok t :messages nil))
             (list
              'all-threads
              (lambda (consumer error)
                (slack-subscriptions-thread-get-view
                 team nil #'ignore consumer error))
              '(:ok t :threads nil :has_more :json-false))
             (list
              'file-list
              (lambda (consumer error)
                (slack-file-list-request
                 team :after-success #'ignore
                 :on-primary-page consumer :on-error error))
              '(:ok t :files nil :paging (:page 1 :pages 1)))
             (list
              'activity-feed
              (lambda (consumer error)
                (funcall
                 (slack-activity-feed--page-loader
                  activity-buffer team state nil)
                 1 consumer error))
              '(:ok t :items nil :response_metadata (:next_cursor nil)))
             (list
              'bookmarks
              (lambda (consumer error)
                (funcall
                 (slack-channel-bookmarks-buffer--page-loader channel-id team)
                 1 consumer error))
              '(:ok t :bookmarks nil))
             (list
              'dialog
              (lambda (consumer error)
                (slack-dialog-get-request "D1" team consumer error))
              '(:ok t :dialog (:title "Dialog" :elements nil)))))
           swallowed reclassified no-request)
      (unwind-protect
          (dolist (case cases)
            (let ((name (nth 0 case))
                  (start (nth 1 case))
                  (payload (nth 2 case))
                  request errors)
              (cl-letf (((symbol-function 'slack-request)
                         (lambda (created &rest _args)
                           (setq request created)))
                        ((symbol-function 'slack-team-missing-user-ids)
                         (lambda (&rest _args) nil)))
                (funcall start
                         (lambda (&rest _args) (error "consumer boom"))
                         (lambda (&rest values) (push values errors)))
                (if request
                    (unless
                        (condition-case nil
                            (progn
                              (funcall (oref request success) :data payload)
                              nil)
                          (error t))
                      (push name swallowed))
                  (push name no-request)))
              (when errors (push name reclassified))))
        (when (and (slot-boundp activity-buffer 'buf)
                   (buffer-live-p (oref activity-buffer buf)))
          (kill-buffer (oref activity-buffer buf))))
      (should-not no-request)
      (should-not swallowed)
      (should-not reclassified))))

(if noninteractive
    (ert-run-tests-batch-and-exit)
  (ert t))
