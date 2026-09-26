;;; slack-block-layout.el --- Block Kit layout blocks  -*- lexical-binding: t; -*-

;; Copyright (C) 2026

;; Author:  Andrea <andrea-dev@hotmail.com>
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
;; along with this program.  If not, see <https://www.gnu.org/licenses/>.


;;; Commentary:

;; Block Kit layout blocks.

;;; Code:

(require 'eieio)
(require 'lui)
(require 'slack-util)
(require 'slack-block-util)
(require 'slack-image)
(require 'slack-block-composition)
(require 'slack-block-element)
(require 'slack-mrkdwn)

;; Layout Blocks
;; [Reference: Message layout blocks | Slack](https://api.slack.com/reference/messaging/blocks)
(defclass slack-layout-block ()
  ((type :initarg :type :type string)
   (block-id :initarg :block_id :type (or string null) :initform nil)
   (payload :initarg :payload :initform nil)))

(cl-defmethod slack-block-find-action ((_this slack-layout-block) _action-id)
  "Return the action-id matching element in the layout block, or nil."
  nil)

(cl-defmethod slack-block-to-string ((this slack-layout-block) &optional _option)
  "Render the layout block as a propertized string.
THIS is the slack-layout-block instance."
  (format "Implement `slack-block-to-string' for %S" (oref this payload)))

(cl-defmethod slack-block-to-mrkdwn ((_this slack-layout-block) &optional _option)
  "Return nil: a layout block has no Slack-flavoured Markdown form.
Subclasses that can be written as Markdown override this; callers such as
`slack-message-get-text' treat nil as a block they cannot reproduce."
  nil)

;; Rich Text Blocks
;; [Changes to message objects on the way to support WYSIWYG | Slack](https://api.slack.com/changelog/2019-09-what-they-see-is-what-you-get-and-more-and-less)
(defclass slack-layout-header-block ()
  ((type :initarg :type :type string)
   (block-id :initarg :block_id :type string)
   (text :initarg :text :type slack-text-message-composition-object)
   ))

(cl-defmethod slack-block-to-string ((this slack-layout-header-block) &optional _option)
  "Render the layout header block as a propertized string.
THIS is the slack-layout-header-block instance."
  ;; 1.2x height matches Slack's header block styling
  (propertize (slack-block-to-string (oref this text)) 'face '(:weight bold :height 1.2)))

(cl-defmethod slack-block-to-mrkdwn ((this slack-layout-header-block) &optional _option)
  "Render the layout header block as Slack-flavoured Markdown text.
THIS is the slack-layout-header-block instance."
  (format "# %s" (slack-block-to-string (oref this text))))

(defun slack-create-layout-header-block (payload)
  "Create and return a new layout header block instance from PAYLOAD."
  (make-instance 'slack-layout-header-block
                 :type (plist-get payload :type)
                 :block_id (plist-get payload :block_id)
                 :text (slack-create-text-message-composition-object
                        (plist-get payload :text))))

(defclass slack-call-layout-block ()
  ((type :initarg :type :type string)
   (block-id :initarg :block_id :type string)
   (join-url :initarg :join_url :type string)))

(cl-defmethod slack-block-to-string ((this slack-call-layout-block) &optional _option)
  "Render the call layout block as a propertized string.
THIS is the slack-call-layout-block instance."
  (concat "Join URL: " (oref this join-url)))

(defun slack-create-call-layout-block (payload)
  "Create and return a new call layout block instance from PAYLOAD."
  (make-instance
   'slack-call-layout-block
   :type (plist-get payload :type)
   ;; Possibly only works for Zoom extension. Don't know about other
   ;; "call" blocks.
   :join_url (thread-first
               payload
               (plist-get :call)
               (plist-get :v1)
               (plist-get :join_url))))

;; Input block: collects information from users via interactive elements
;; https://api.slack.com/reference/block-kit/blocks#input
(defclass slack-input-layout-block ()
  ((type :initarg :type :type string)
   (block-id :initarg :block_id :type (or string null) :initform nil)
   (label :initarg :label :type slack-text-message-composition-object)
   (element :initarg :element :initform nil)
   (hint :initarg :hint :type (or null slack-text-message-composition-object) :initform nil)
   (optional-p :initarg :optional-p :type boolean :initform nil)
   (dispatch-action :initarg :dispatch-action :type boolean :initform nil)))

(cl-defmethod slack-block-to-string ((this slack-input-layout-block) &optional _option)
  "Render the input layout block as a propertized string.
THIS is the slack-input-layout-block instance."
  (let ((label-str (slack-block-to-string (oref this label)))
        (hint-str (when (oref this hint)
                    (slack-block-to-string (oref this hint)))))
    (concat (propertize label-str 'face '(:weight bold))
            (when (oref this optional-p) " (optional)")
            (when hint-str (concat "\n" (propertize hint-str 'face 'font-lock-comment-face)))
            "\n[interactive input]")))

(cl-defmethod slack-block-to-mrkdwn ((this slack-input-layout-block) &optional _option)
  "Render the input layout block as Slack-flavoured Markdown text.
THIS is the slack-input-layout-block instance."
  (let ((label-str (slack-block-to-string (oref this label))))
    (concat "*" label-str "*"
            (when (oref this optional-p) " (optional)")
            "\n[interactive input]")))

(defun slack-create-input-layout-block (payload)
  "Create and return a new input layout block instance from PAYLOAD."
  (make-instance 'slack-input-layout-block
                 :type (plist-get payload :type)
                 :block_id (plist-get payload :block_id)
                 :label (slack-create-text-message-composition-object
                         (plist-get payload :label))
                 :element (plist-get payload :element)
                 :hint (when (plist-get payload :hint)
                         (slack-create-text-message-composition-object
                          (plist-get payload :hint)))
                 :optional-p (eq t (plist-get payload :optional))
                 :dispatch-action (eq t (plist-get payload :dispatch_action))))

(defclass slack-section-layout-block (slack-layout-block)
  ((type :initarg :type :type string :initform "section")
   (text :initarg :text :type (or null slack-text-message-composition-object) :initform nil)
   (fields :initarg :fields :type (or list null) :initform nil) ;; list of slack-text-message-composition-object
   (accessory :initarg :accessory :initform nil :type (or null slack-block-element))))

(defun slack-create-section-layout-block (payload)
  "Create and return a new section layout block instance from PAYLOAD."
  (let ((accessory (slack-create-block-element
                    (plist-get payload :accessory)
                    (plist-get payload :block_id))))
    (make-instance 'slack-section-layout-block
                   :text (slack-create-text-message-composition-object
                          (plist-get payload :text))
                   :block_id (plist-get payload :block_id)
                   :fields (mapcar #'slack-create-text-message-composition-object
                                   (plist-get payload :fields))
                   :accessory accessory)))

(cl-defmethod slack-block-to-string ((this slack-section-layout-block) &optional _option)
  "Render the section layout block as a propertized string.
THIS is the slack-section-layout-block instance."
  (with-slots (fields accessory text) this
    (slack-format-message (slack-block-to-string text)
                          (mapconcat #'identity
                                     (mapcar #'slack-block-to-string
                                             fields)
                                     "\n")
                          (slack-block-to-string accessory))))

(cl-defmethod slack-block-find-action ((this slack-section-layout-block) action-id)
  "Return the ACTION-ID matching element in the section layout block, or nil.
THIS is the slack-section-layout-block instance."
  (with-slots (accessory) this
    (when (and accessory
               (string= (slack-block-action-id accessory)
                        action-id))
      accessory)))

(defclass slack-divider-layout-block (slack-layout-block)
  ((type :initarg :type :type string :initform "divider")))

(defun slack-create-divider-layout-block (payload)
  "Create and return a new divider layout block instance from PAYLOAD."
  (make-instance 'slack-divider-layout-block
                 :block_id (plist-get payload :block_id)))

(cl-defmethod slack-block-to-string ((_this slack-divider-layout-block) &optional _option)
  "Render the divider layout block as a propertized string."
  (let ((columns (or lui-fill-column
                     0)))
    (make-string columns ?-)))

(defclass slack-image-layout-block (slack-layout-block)
  ((type :initarg :type :type string :initform "image")
   (image-url :initarg :image_url :type string)
   (alt-text :initarg :alt_text :type string)
   (title :initarg :title :initform nil (or null slack-text-message-composition-object))
   (image-height :initarg :image_height :type (or null number) :initform nil)
   (image-width :initarg :image_width :type (or null number) :initform nil)
   (image-bytes :initarg :image_bytes :type (or null number) :initform nil)))

(defun slack-create-image-layout-block (payload)
  "Create and return a new image layout block instance from PAYLOAD."
  (make-instance 'slack-image-layout-block
                 :image_url (plist-get payload :image_url)
                 :alt_text (plist-get payload :alt_text)
                 :title (slack-create-text-message-composition-object
                         (plist-get payload :title))
                 :block_id (plist-get payload :block_id)
                 :image_width (plist-get payload :image_width)
                 :image_height (plist-get payload :image_height)
                 :image_bytes (plist-get payload :image_bytes)))

(cl-defmethod slack-block-to-string ((this slack-image-layout-block) &optional _option)
  "Render the image layout block as a propertized string.
THIS is the slack-image-layout-block instance."
  (with-slots (image-url alt-text title image-height image-width image-bytes) this
    (let ((spec (list image-url
                      image-width
                      image-height
                      slack-image-max-height)))
      ;; Use SI kB (1000) not KiB (1024), matching Slack's web UI
      (slack-format-message (if image-bytes
                                (format "%s (%s kB)" alt-text
                                        (round (/ image-bytes 1000.0)))
                              (format "%s" alt-text))
                            (slack-image-string spec)))))

(defclass slack-actions-layout-block (slack-layout-block)
  ((type :initarg :type :type string :initform "actions")
   (elements :initarg :elements :type list) ;; max 5 elements
   ))

(defun slack-create-actions-layout-block (payload)
  "Create and return a new actions layout block instance from PAYLOAD."
  (make-instance 'slack-actions-layout-block
                 :elements (mapcar #'(lambda (e)
                                       (slack-create-block-element
                                        e (plist-get payload :block_id)))
                                   (plist-get payload :elements))
                 :block_id (plist-get payload :block_id)))

(cl-defmethod slack-block-to-string ((this slack-actions-layout-block) &optional _option)
  "Render the actions layout block as a propertized string.
THIS is the slack-actions-layout-block instance."
  (with-slots (elements) this
    (mapconcat #'identity
               (mapcar #'slack-block-to-string
                       elements)
               " ")))

(cl-defmethod slack-block-find-action ((this slack-actions-layout-block) action-id)
  "Return the ACTION-ID matching element in the actions layout block, or nil.
THIS is the slack-actions-layout-block instance."
  (with-slots (elements) this
    (cl-find-if #'(lambda (e) (string= action-id (slack-block-action-id e)))
                elements)))

(defclass slack-context-layout-block (slack-layout-block)
  ((type :initarg :type :type string :initform "context")
   (elements :initarg :elements :type list)))

(defun slack-create-context-layout-block (payload)
  "Create and return a new context layout block instance from PAYLOAD."
  (make-instance 'slack-context-layout-block
                 :elements (mapcar #'(lambda (e)
                                       (or (if (string= "image" (plist-get e :type))
                                               (slack-create-block-element e (plist-get payload :block_id))
                                             (slack-create-text-message-composition-object  e))))
                                   (plist-get payload :elements))
                 :block_id (plist-get payload :block_id)))

(cl-defmethod slack-block-to-string ((this slack-context-layout-block) &optional _option)
  "Render the context layout block as a propertized string.
THIS is the slack-context-layout-block instance."
  (with-slots (elements) this
    (mapconcat #'identity
               ;; Context block images are small inline thumbnails (30px cap)
               (mapcar #'(lambda (e) (slack-block-to-string e '(:max-image-height 30 :max-image-width 30)))
                       elements)
               " ")))

(cl-defmethod slack-block-find-action ((this slack-context-layout-block) action-id)
  "Return the ACTION-ID matching element in the context layout block, or nil.
THIS is the slack-context-layout-block instance."
  (with-slots (elements) this
    (cl-find-if #'(lambda (e) (string= action-id (slack-block-action-id e)))
                elements)))

;; File block: references a remote file shared in Slack
;; https://api.slack.com/reference/block-kit/blocks#file
(defclass slack-file-layout-block ()
  ((type :initarg :type :type string)
   (block-id :initarg :block_id :type (or string null) :initform nil)
   (external-id :initarg :external_id :type string)
   (source :initarg :source :type string)))

(cl-defmethod slack-block-to-string ((this slack-file-layout-block) &optional _option)
  "Render THIS file layout block as a propertized string."
  (propertize (format "[File: %s]" (oref this external-id))
              'face 'slack-mrkdwn-code-face))

(cl-defmethod slack-block-to-mrkdwn ((this slack-file-layout-block) &optional _option)
  "Render THIS file layout block as Slack-flavoured Markdown text."
  (format "[File: %s]" (oref this external-id)))

(defun slack-create-file-layout-block (payload)
  "Create and return a new file layout block instance from PAYLOAD."
  (make-instance 'slack-file-layout-block
                 :type (plist-get payload :type)
                 :block_id (plist-get payload :block_id)
                 :external_id (or (plist-get payload :external_id) "unknown")
                 :source (or (plist-get payload :source) "remote")))

(provide 'slack-block-layout)
;;; slack-block-layout.el ends here
