;;; test-message-images.el --- Message image boundaries -*- lexical-binding: t; -*-

(require 'ert)
(require 'cl-lib)
(require 'slack-message-buffer)

(ert-deftest slack-test-images-stay-within-message ()
  "Image lookup must ignore adjacent messages regardless of timestamp order."
  (dolist (next-ts '("1" "3"))
    (with-temp-buffer
      (let ((own '(image :type gif :file "own.gif"))
            (other '(image :type gif :file "other.gif")))
        (insert (propertize "own" 'ts "2" 'slack-image-display own))
        (insert (propertize "other" 'ts next-ts 'slack-image-display other))
        (cl-letf (((symbol-function 'slack-buffer-room) (lambda (_) t)))
          (should (equal (slack-buffer-get-images "2") (list own)))
          (should-not (slack-buffer-get-images "missing"))
          (should-not (slack-buffer-get-images nil)))))))

(ert-deftest slack-test-images-skip-unchanged-property-runs ()
  "Long messages must not require per-character image-property lookups."
  (with-temp-buffer
    (let ((first '(image :type gif :file "first.gif"))
          (second '(image :type gif :file "second.gif"))
          (third '(image :type gif :file "third.gif"))
          (reads 0)
          (original (symbol-function 'get-text-property)))
      (insert (propertize (make-string 10000 ?x) 'ts "2"))
      (put-text-property 1 10001 'slack-image-display first)
      (put-text-property 100 200 'emojify-display (list '(raise 0.2) second))
      (put-text-property 9000 10001 'slack-image-display third)
      (cl-letf (((symbol-function 'slack-buffer-room) (lambda (_) t))
                ((symbol-function 'get-text-property)
                 (lambda (position property &optional object)
                   (when (memq property '(slack-image-display emojify-display))
                     (cl-incf reads)
                     (when (> reads 30)
                       (ert-fail "Image lookup scans individual characters")))
                   (funcall original position property object))))
        (let ((images (slack-buffer-get-images "2")))
          (should (= 3 (length images)))
          (dolist (image (list first second third))
            (should (member image images))))))))

;;; test-message-images.el ends here
