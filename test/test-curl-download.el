;;; test-curl-download.el --- Curl input transport -*- lexical-binding: t; -*-

(require 'ert)
(require 'cl-lib)
(require 'slack-request)

(ert-deftest slack-test-curl-downloader-closes-pipe-input ()
  "Downloads must finish with closed pipe input even when Emacs prefers PTYs."
  (let* ((dir (make-temp-file "slack-curl-test-" t))
         (source (expand-file-name "source" dir))
         (target (expand-file-name "target" dir))
         (process-connection-type t)
         (start (symbol-function 'start-process))
         proc done failure)
    (unwind-protect
        (progn
          (with-temp-file source (insert "avatar payload"))
          (cl-letf (((symbol-function 'start-process)
                     (lambda (&rest args)
                       (setq proc (apply start args)))))
            (slack-curl-downloader
             (concat "file://" source) target nil
             :success (lambda () (setq done t))
             :error (lambda (&rest args) (setq failure args))))
          (should-not (process-tty-name proc))
          (let ((deadline (+ (float-time) 5)))
            (while (and (not done) (not failure) (< (float-time) deadline))
              (accept-process-output proc 0.05)))
          (should-not failure)
          (should done)
          (should (equal "avatar payload"
                         (with-temp-buffer
                           (insert-file-contents target)
                           (buffer-string))))
          (should (equal '("source" "target")
                         (directory-files dir nil "^[^.]"))))
      (when (processp proc)
        (set-process-sentinel proc #'ignore)
        (when (process-live-p proc) (delete-process proc)))
      (delete-directory dir t))))

;;; test-curl-download.el ends here
