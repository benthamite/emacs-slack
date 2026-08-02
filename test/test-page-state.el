;;; test-page-state.el --- async page-state tests  -*- lexical-binding: t; -*-

(require 'ert)
(require 'slack-page-state)
(require 'slack-team)

(defclass slack-test-page-buffer (slack-buffer) ())

(cl-defmethod slack-buffer-name ((_this slack-test-page-buffer))
  "Return the test page buffer name."
  " *slack test page*")

(cl-defmethod slack-buffer-key ((_this slack-test-page-buffer))
  "Return the test page buffer key."
  'test-page)

(cl-defmethod slack-team-buffer-key ((_this slack-test-page-buffer))
  "Return the test page's team buffer key."
  'slack-message-buffer)

(ert-deftest slack-test-async-barrier-finishes-zero-work-immediately ()
  (let ((calls 0))
    (slack-async-barrier-create 0 (lambda () (cl-incf calls)))
    (should (= 1 calls))))

(ert-deftest slack-test-async-barrier-counts-success-and-failure-completions ()
  (let* ((calls 0)
         (barrier (slack-async-barrier-create
                   2 (lambda () (cl-incf calls))))
         (success (lambda () (slack-async-barrier-done barrier)))
         (failure (lambda () (slack-async-barrier-done barrier))))
    (funcall failure)
    (should (= 0 calls))
    (funcall success)
    (should (= 1 calls))))

(ert-deftest slack-test-async-barrier-finishes-exactly-once ()
  (let* ((calls 0)
         (barrier (slack-async-barrier-create
                   2 (lambda () (cl-incf calls)))))
    (slack-async-barrier-done barrier)
    (should (= 0 calls))
    (slack-async-barrier-done barrier)
    (slack-async-barrier-done barrier)
    (should (= 1 calls))))

(ert-deftest slack-test-async-barrier-rejects-invalid-counts ()
  (dolist (count '(-1 1.5 nil))
    (should-error (slack-async-barrier-create count #'ignore)
                  :type 'wrong-type-argument)))

(ert-deftest slack-test-page-state-commits-an-empty-page ()
  (let* ((state (slack-page-state-create))
         (generation (slack-page-state-begin state)))
    (should (eq 'loading (slack-page-state-status state)))
    (should (slack-page-state-commit state generation nil "next" t))
    (should (slack-page-state-loaded-p state))
    (should (eq 'ready (slack-page-state-status state)))
    (should-not (slack-page-state-value state))
    (should (equal "next" (slack-page-state-continuation state)))
    (should (slack-page-state-has-more state))))

(ert-deftest slack-test-page-state-coalesces-and-rejects-stale-results ()
  (let* ((state (slack-page-state-create))
         (first (slack-page-state-begin state)))
    (should-not (slack-page-state-begin state))
    (let ((second (slack-page-state-restart state)))
      (should (< first second))
      (should-not (slack-page-state-commit state first '(old) nil nil))
      (should (slack-page-state-commit state second '(new) nil nil))
      (should (equal '(new) (slack-page-state-value state))))))

(ert-deftest slack-test-page-state-refresh-preserves-stale-value-on-failure ()
  (let* ((state (slack-page-state-create))
         (first (slack-page-state-begin state)))
    (slack-page-state-commit state first '(cached) nil nil)
    (let ((second (slack-page-state-begin state t)))
      (should (eq 'refreshing (slack-page-state-status state)))
      (should (slack-page-state-fail state second 'network-error))
      (should (eq 'failed (slack-page-state-status state)))
      (should (equal '(cached) (slack-page-state-value state)))
      (should (slack-page-state-loaded-p state)))))

(ert-deftest slack-test-page-state-primary-commit-precedes-ready ()
  (let* ((state (slack-page-state-create))
         (events nil)
         (generation (slack-page-state-begin state)))
    (slack-page-state-on-commit
     state (lambda (_state) (push 'commit events)))
    (slack-page-state-on-ready
     state (lambda (_state) (push 'ready events)))
    (slack-page-state-commit state generation '(value) nil nil t)
    (should (equal '(commit) events))
    (should (slack-page-state-in-flight-p state))
    (slack-page-state-ready state generation)
    (slack-page-state-ready state generation)
    (should (equal '(ready commit) events))))

(ert-deftest slack-test-page-state-immediate-commit-is-ready-before-notify ()
  (let* ((state (slack-page-state-create))
         (generation (slack-page-state-begin state))
         events)
    (slack-page-state-on-commit
     state
     (lambda (committed-state)
       (setq events
             (append events
                     (list (list 'commit
                                 (slack-page-state-status committed-state)))))))
    (slack-page-state-on-ready
     state
     (lambda (ready-state)
       (setq events
             (append events
                     (list (list 'ready
                                 (slack-page-state-status ready-state)))))))
    (slack-page-state-commit state generation '(value) nil nil)
    (should (equal '((commit ready) (ready ready)) events))))

(ert-deftest slack-test-page-state-ready-registered-during-commit-waits ()
  (let* ((state (slack-page-state-create))
         (generation (slack-page-state-begin state))
         events)
    (slack-page-state-on-ready
     state (lambda (_state) (setq events (append events '(ready-existing)))))
    (slack-page-state-on-commit
     state (lambda (_state) (setq events (append events '(commit-other)))))
    (slack-page-state-on-commit
     state
     (lambda (committed-state)
       (setq events (append events '(commit-add)))
       (slack-page-state-on-ready
        committed-state
        (lambda (_state)
          (setq events (append events '(ready-nested)))))))
    (slack-page-state-commit state generation '(value) nil nil)
    (should (equal '(commit-add commit-other ready-nested ready-existing)
                   events))))

(ert-deftest slack-test-page-state-does-not-reuse-completed-error-waiter ()
  (let* ((state (slack-page-state-create))
         (errors 0)
         (first (slack-page-state-begin state)))
    (slack-page-state-on-error
     state (lambda (_state _error) (cl-incf errors)))
    (slack-page-state-commit state first '(value) nil nil)
    (let ((second (slack-page-state-begin state t)))
      (slack-page-state-fail state second 'network-error))
    (should (= 0 errors))))

(ert-deftest slack-test-page-state-terminal-results-are-final ()
  (let* ((state (slack-page-state-create))
         (first (slack-page-state-begin state)))
    (slack-page-state-commit state first '(ready) nil nil)
    (should-not (slack-page-state-fail state first 'late-error))
    (should (eq 'ready (slack-page-state-status state)))
    (let ((second (slack-page-state-restart state)))
      (slack-page-state-fail state second 'network-error)
      (should-not (slack-page-state-ready state second))
      (should-not
       (slack-page-state-commit state second '(late-value) nil nil))
      (should (eq 'failed (slack-page-state-status state)))
      (should (eq 'network-error (slack-page-state-error state)))
      (should (equal '(ready) (slack-page-state-value state))))))

(ert-deftest slack-test-page-state-does-not-retain-inactive-waiters ()
  (let ((state (slack-page-state-create)))
    (slack-page-state-on-commit state #'ignore)
    (slack-page-state-on-ready state #'ignore)
    (slack-page-state-on-error state #'ignore)
    (should-not (slack-page-state-commit-waiters state))
    (should-not (slack-page-state-ready-waiters state))
    (should-not (slack-page-state-error-waiters state))
    (let ((generation (slack-page-state-begin state)))
      (slack-page-state-commit state generation '(value) nil nil))
    (slack-page-state-on-error state #'ignore)
    (should-not (slack-page-state-error-waiters state))))

(ert-deftest slack-test-page-state-failure-discards-waiters-before-notifying ()
  (let* ((state (slack-page-state-create))
         (generation (slack-page-state-begin state))
         waiters-at-error)
    (slack-page-state-on-commit state #'ignore)
    (slack-page-state-on-ready state #'ignore)
    (slack-page-state-on-error
     state
     (lambda (failed-state _error)
       (setq waiters-at-error
             (list (slack-page-state-commit-waiters failed-state)
                   (slack-page-state-ready-waiters failed-state)
                   (slack-page-state-error-waiters failed-state)))))
    (slack-page-state-fail state generation 'network-error)
    (should (equal '(nil nil nil) waiters-at-error))))

(ert-deftest slack-test-page-state-ignores-non-function-callbacks ()
  (let ((state (slack-page-state-create)))
    (should-not (slack-page-state-on-commit state nil))
    (should-not (slack-page-state-on-ready state 'not-a-function))
    (should-not (slack-page-state-on-error state nil))
    (let ((generation (slack-page-state-begin state)))
      (should-not (slack-page-state-on-commit state nil))
      (should-not (slack-page-state-on-ready state 'not-a-function))
      (should-not (slack-page-state-on-error state nil))
      (should-not (slack-page-state-commit-waiters state))
      (should-not (slack-page-state-ready-waiters state))
      (should-not (slack-page-state-error-waiters state))
      (should (slack-page-state-commit state generation '(value) nil nil))
      (should-not (slack-page-state-on-commit state nil))
      (should-not (slack-page-state-on-ready state 'not-a-function)))
    (let ((generation (slack-page-state-restart state)))
      (should-not (slack-page-state-on-error state nil))
      (should (slack-page-state-fail state generation 'network-error))
      (should-not (slack-page-state-on-error state 'not-a-function)))))

(ert-deftest slack-test-page-state-rejects-pristine-ready ()
  (let ((state (slack-page-state-create)))
    (should-not (slack-page-state-ready state 0))
    (should (eq 'unloaded (slack-page-state-status state)))
    (should (= 0 (slack-page-state-ready-generation state)))))

(ert-deftest slack-test-page-state-commit-callback-error-does-not-stop-ready ()
  (let* ((state (slack-page-state-create))
         (generation (slack-page-state-begin state))
         (commits 0)
         (ready 0)
         warnings)
    (cl-letf (((symbol-function 'display-warning)
               (lambda (source message &optional level _buffer-name)
                 (push (list source message level) warnings))))
      (slack-page-state-on-commit
       state (lambda (_state) (cl-incf commits)))
      (slack-page-state-on-commit
       state (lambda (_state) (error "commit callback boom")))
      (slack-page-state-on-ready
       state (lambda (_state) (cl-incf ready)))
      (should (slack-page-state-commit state generation '(value) nil nil)))
    (should (= 1 commits))
    (should (= 1 ready))
    (should (eq 'ready (slack-page-state-status state)))
    (should (= 1 (length warnings)))
    (should (eq 'slack-page-state (caar warnings)))
    (should (string-match-p "commit callback boom" (cadar warnings)))
    (should (eq :error (caddar warnings)))))

(ert-deftest slack-test-page-state-ready-and-error-callbacks-are-isolated ()
  (let* ((state (slack-page-state-create))
         (generation (slack-page-state-begin state))
         (ready 0)
         (errors 0)
         warnings)
    (cl-letf (((symbol-function 'display-warning)
               (lambda (source message &optional level _buffer-name)
                 (push (list source message level) warnings))))
      (slack-page-state-on-ready
       state (lambda (_state) (cl-incf ready)))
      (slack-page-state-on-ready
       state (lambda (_state) (error "ready callback boom")))
      (slack-page-state-commit state generation '(value) nil nil)
      (setq generation (slack-page-state-restart state))
      (slack-page-state-on-error
       state (lambda (_state _error) (cl-incf errors)))
      (slack-page-state-on-error
       state (lambda (_state _error) (error "error callback boom")))
      (slack-page-state-fail state generation 'network-error))
    (should (= 1 ready))
    (should (= 1 errors))
    (should (= 2 (length warnings)))))

(ert-deftest slack-test-page-state-error-waiters-share-terminal-error ()
  (let* ((state (slack-page-state-create))
         (generation (slack-page-state-begin state))
         received-error)
    (slack-page-state-on-error
     state (lambda (_state error) (setq received-error error)))
    (slack-page-state-on-error
     state (lambda (failed-state _error)
             (slack-page-state-restart failed-state)))
    (slack-page-state-fail state generation 'network-error)
    (should (eq 'network-error received-error))
    (should (= 2 (slack-page-state-generation state)))
    (should-not (slack-page-state-error state))))

(ert-deftest slack-test-present-page-displays-before-loader ()
  (let* ((state (slack-page-state-create))
         (events nil)
         (team (make-instance 'slack-team))
         (object (make-instance 'slack-test-page-buffer :team-id nil)))
    (slack-buffer-cache-team object team)
    (unwind-protect
        (cl-letf (((symbol-function 'slack-buffer-display)
                   (lambda (_object) (push 'display events))))
          (slack-buffer-present-page
           object state
           (lambda (_generation _success _error) (push 'request events))
           (lambda (_object _state) (push 'render events))))
      (when (buffer-live-p (oref object buf))
        (kill-buffer (oref object buf))))
    (should (equal '(request display render) events))))

(ert-deftest slack-test-present-page-does-not-resurrect-killed-buffer ()
  (let* ((state (slack-page-state-create))
         (team (make-instance 'slack-team))
         (object (make-instance 'slack-test-page-buffer :team-id nil))
         success)
    (slack-buffer-cache-team object team)
    (cl-letf (((symbol-function 'slack-buffer-display) #'ignore))
      (slack-buffer-present-page
       object state
       (lambda (_generation on-success _on-error)
         (setq success on-success))
       #'ignore))
    (let ((dead (oref object buf)))
      (kill-buffer dead)
      (funcall success '(value) nil nil)
      (should-not (buffer-live-p dead))
      (should (eq dead (oref object buf))))))

(ert-deftest slack-test-present-page-supersedes-different-state-in-same-buffer ()
  (let* ((first-state (slack-page-state-create))
         (second-state (slack-page-state-create))
         (team (make-instance 'slack-team))
         (object (make-instance 'slack-test-page-buffer :team-id nil))
         first-generation
         first-success
         first-error
         second-success
         events)
    (slack-buffer-cache-team object team)
    (unwind-protect
        (cl-letf (((symbol-function 'slack-buffer-display) #'ignore))
          (slack-buffer-present-page
           object first-state
           (lambda (generation success error)
             (setq first-generation generation
                   first-success success
                   first-error error))
           (lambda (_object rendered-state)
             (setq events
                   (append events
                           (list (list 'render rendered-state
                                       (slack-page-state-status rendered-state)
                                       (slack-page-state-value
                                        rendered-state))))))
           nil
           (lambda (_state)
             (setq events (append events '((ready first)))))
           (lambda (_state _error)
             (setq events (append events '((error first))))))
          (let ((captured (oref object buf)))
            (slack-buffer-present-page
             object second-state
             (lambda (_generation success _error)
               (setq second-success success))
             (lambda (_object rendered-state)
               (setq events
                     (append events
                             (list (list 'render rendered-state
                                         (slack-page-state-status
                                          rendered-state)
                                         (slack-page-state-value
                                          rendered-state))))))
             nil
             (lambda (_state)
               (setq events (append events '((ready second)))))
             (lambda (_state _error)
               (setq events (append events '((error second))))))
            (should (eq captured (oref object buf)))
            (setq events nil)
            (funcall first-success '(first-primary) nil nil t)
            (slack-page-state-ready first-state first-generation)
            (funcall first-error 'late-first-error)
            (should-not events)
            (funcall second-success '(second-value) nil nil)
            (should
             (equal (list (list 'render second-state 'ready
                                '(second-value))
                          '(ready second))
                    events))))
      (when (buffer-live-p (oref object buf))
        (kill-buffer (oref object buf))))))

(ert-deftest slack-test-present-page-newer-same-state-supersedes-callbacks ()
  (let* ((state (slack-page-state-create))
         (team (make-instance 'slack-team))
         (object (make-instance 'slack-test-page-buffer :team-id nil))
         success
         events)
    (slack-buffer-cache-team object team)
    (unwind-protect
        (cl-letf (((symbol-function 'slack-buffer-display) #'ignore))
          (slack-buffer-present-page
           object state
           (lambda (_generation on-success _error)
             (setq success on-success))
           (lambda (_object _state)
             (setq events (append events '((render first)))))
           nil
           (lambda (_state)
             (setq events (append events '((ready first))))))
          (slack-buffer-present-page
           object state
           (lambda (&rest _arguments)
             (ert-fail "Coalesced presentation started another request"))
           (lambda (_object _state)
             (setq events (append events '((render second)))))
           nil
           (lambda (_state)
             (setq events (append events '((ready second))))))
          (setq events nil)
          (funcall success '(value) nil nil)
          (should (equal '((render second) (ready second)) events)))
      (when (buffer-live-p (oref object buf))
        (kill-buffer (oref object buf))))))

(ert-deftest slack-test-present-page-rerenders-before-deferred-ready-callback ()
  (let* ((state (slack-page-state-create))
         (team (make-instance 'slack-team))
         (object (make-instance 'slack-test-page-buffer :team-id nil))
         generation
         success
         events)
    (slack-buffer-cache-team object team)
    (unwind-protect
        (cl-letf (((symbol-function 'slack-buffer-display) #'ignore))
          (slack-buffer-present-page
           object state
           (lambda (request-generation on-success _on-error)
             (setq generation request-generation
                   success on-success))
           (lambda (_object rendered-state)
             (setq events
                   (append events
                           (list (list 'render
                                       (current-buffer)
                                       (slack-page-state-status rendered-state)
                                       (slack-page-state-value
                                        rendered-state))))))
           nil
           (lambda (ready-state)
             (setq events (append events (list (list 'ready ready-state))))))
          (let ((captured (oref object buf)))
            (setq events nil)
            (funcall success '(primary) nil nil t)
            (should (equal (list (list 'render captured 'loading '(primary)))
                           events))
            (setf (slack-page-state-value state) '(hydrated))
            (slack-page-state-ready state generation)
            (should
             (equal (list (list 'render captured 'loading '(primary))
                          (list 'render captured 'ready '(hydrated))
                          (list 'ready state))
                    events))))
      (when (buffer-live-p (oref object buf))
        (kill-buffer (oref object buf))))))

(ert-deftest slack-test-present-page-immediate-commit-renders-once ()
  (let* ((state (slack-page-state-create))
         (team (make-instance 'slack-team))
         (object (make-instance 'slack-test-page-buffer :team-id nil))
         success
         events)
    (slack-buffer-cache-team object team)
    (unwind-protect
        (cl-letf (((symbol-function 'slack-buffer-display) #'ignore))
          (slack-buffer-present-page
           object state
           (lambda (_generation on-success _error)
             (setq success on-success))
           (lambda (_object rendered-state)
             (setq events
                   (append events
                           (list (list 'render
                                       (slack-page-state-status rendered-state)
                                       (slack-page-state-value
                                        rendered-state))))))
           nil
           (lambda (_state)
             (setq events (append events '((ready))))))
          (setq events nil)
          (funcall success '(value) nil nil)
          (should (equal '((render ready (value)) (ready)) events)))
      (when (buffer-live-p (oref object buf))
        (kill-buffer (oref object buf))))))

(ert-deftest slack-test-present-page-ready-cache-renders-once ()
  (let* ((state (slack-page-state-create))
         (generation (slack-page-state-begin state))
         (team (make-instance 'slack-team))
         (object (make-instance 'slack-test-page-buffer :team-id nil))
         (renders 0)
         (ready-calls 0))
    (slack-page-state-commit state generation '(cached) nil nil)
    (slack-buffer-cache-team object team)
    (unwind-protect
        (cl-letf (((symbol-function 'slack-buffer-display) #'ignore))
          (slack-buffer-present-page
           object state
           (lambda (&rest _arguments)
             (ert-fail "Ready cache started another request"))
           (lambda (_object rendered-state)
             (should (eq 'ready
                         (slack-page-state-status rendered-state)))
             (cl-incf renders))
           nil
           (lambda (_state) (cl-incf ready-calls)))
          (should (= 1 renders))
          (should (= 1 ready-calls)))
      (when (buffer-live-p (oref object buf))
        (kill-buffer (oref object buf))))))

(ert-deftest slack-test-present-page-ready-does-not-resurrect-killed-buffer ()
  (let* ((state (slack-page-state-create))
         (team (make-instance 'slack-team))
         (object (make-instance 'slack-test-page-buffer :team-id nil))
         generation
         success
         (renders 0)
         (ready-calls 0))
    (slack-buffer-cache-team object team)
    (cl-letf (((symbol-function 'slack-buffer-display) #'ignore))
      (slack-buffer-present-page
       object state
       (lambda (request-generation on-success _on-error)
         (setq generation request-generation
               success on-success))
       (lambda (_object _state) (cl-incf renders))
       nil
       (lambda (_state) (cl-incf ready-calls))))
    (funcall success '(primary) nil nil t)
    (let ((dead (oref object buf)))
      (kill-buffer dead)
      (setq renders 0)
      (let ((buffers-after-kill (buffer-list)))
        (slack-page-state-ready state generation)
        (should (equal buffers-after-kill (buffer-list))))
      (should (= 0 renders))
      (should (= 0 ready-calls))
      (should (eq dead (oref object buf)))
      (should-not (buffer-live-p dead)))))

(ert-deftest slack-test-present-page-ready-skips-replaced-buffer ()
  (let* ((state (slack-page-state-create))
         (team (make-instance 'slack-team))
         (object (make-instance 'slack-test-page-buffer :team-id nil))
         generation
         success
         renders
         (ready-calls 0))
    (slack-buffer-cache-team object team)
    (unwind-protect
        (cl-letf (((symbol-function 'slack-buffer-display) #'ignore))
          (slack-buffer-present-page
           object state
           (lambda (request-generation on-success _on-error)
             (setq generation request-generation
                   success on-success))
           (lambda (_object _state) (push (current-buffer) renders))
           nil
           (lambda (_state) (cl-incf ready-calls)))
          (funcall success '(primary) nil nil t)
          (let ((captured (oref object buf))
                (replacement (generate-new-buffer
                              " *slack test replacement*")))
            (unwind-protect
                (progn
                  (oset object buf replacement)
                  (setq renders nil)
                  (let ((buffers-before-ready (buffer-list)))
                    (slack-page-state-ready state generation)
                    (should (equal buffers-before-ready (buffer-list))))
                  (should-not renders)
                  (should (= 0 ready-calls))
                  (should (buffer-live-p captured))
                  (should (eq replacement (oref object buf))))
              (kill-buffer replacement)
              (kill-buffer captured))))
      (when (buffer-live-p (oref object buf))
        (kill-buffer (oref object buf))))))

(ert-deftest slack-test-present-page-ready-callback-survives-renderer-error ()
  (let* ((state (slack-page-state-create))
         (team (make-instance 'slack-team))
         (object (make-instance 'slack-test-page-buffer :team-id nil))
         generation
         success
         events
         warnings)
    (slack-buffer-cache-team object team)
    (unwind-protect
        (cl-letf (((symbol-function 'slack-buffer-display) #'ignore)
                  ((symbol-function 'display-warning)
                   (lambda (&rest arguments) (push arguments warnings))))
          (slack-buffer-present-page
           object state
           (lambda (request-generation on-success _error)
             (setq generation request-generation
                   success on-success))
           (lambda (_object rendered-state)
             (when (eq 'ready (slack-page-state-status rendered-state))
               (setq events (append events '((render ready))))
               (error "Ready renderer boom")))
           nil
           (lambda (_state)
             (setq events (append events '((ready callback))))))
          (funcall success '(primary) nil nil t)
          (setq events nil)
          (slack-page-state-ready state generation)
          (should (equal '((render ready) (ready callback)) events))
          (should (= 1 (length warnings))))
      (when (buffer-live-p (oref object buf))
        (kill-buffer (oref object buf))))))

(ert-deftest slack-test-present-page-error-callback-survives-renderer-error ()
  (let* ((state (slack-page-state-create))
         (team (make-instance 'slack-team))
         (object (make-instance 'slack-test-page-buffer :team-id nil))
         failure
         events
         warnings)
    (slack-buffer-cache-team object team)
    (unwind-protect
        (cl-letf (((symbol-function 'slack-buffer-display) #'ignore)
                  ((symbol-function 'display-warning)
                   (lambda (&rest arguments) (push arguments warnings))))
          (slack-buffer-present-page
           object state
           (lambda (_generation _success on-error)
             (setq failure on-error))
           (lambda (_object rendered-state)
             (when (eq 'failed (slack-page-state-status rendered-state))
               (setq events (append events '((render failed))))
               (error "Error renderer boom")))
           nil nil
           (lambda (_state error)
             (setq events (append events (list (list 'error error))))))
          (funcall failure 'network-error)
          (should (equal '((render failed) (error network-error)) events))
          (should (= 1 (length warnings))))
      (when (buffer-live-p (oref object buf))
        (kill-buffer (oref object buf))))))

(ert-deftest slack-test-present-page-normalizes-loader-errors ()
  (let* ((api-error (list 'api-error (concat "invalid" "_auth")))
         (response (list 'request-response (make-string 200 ?x)))
         (cases (list (list (list api-error) api-error t)
                      (list (list :error-thrown '(error . "timeout")
                                  :symbol-status 'error
                                  :response response
                                  :data '(:large "ignored"))
                            "timeout" nil)
                      (list nil "Slack transport error" nil))))
    (dolist (case cases)
      (pcase-let* ((`(,arguments ,expected ,same-object-p) case)
                   (state (slack-page-state-create))
                   (team (make-instance 'slack-team))
                   (object (make-instance 'slack-test-page-buffer
                                          :team-id nil))
                   (failure nil))
        (slack-buffer-cache-team object team)
        (unwind-protect
            (cl-letf (((symbol-function 'slack-buffer-display) #'ignore))
              (slack-buffer-present-page
               object state
               (lambda (_generation _success on-error)
                 (setq failure on-error))
               (lambda (rendered-object rendered-state)
                 (let ((inhibit-read-only t))
                   (erase-buffer)
                   (slack-buffer-insert-page-status
                    rendered-object rendered-state))))
              (apply failure arguments)
              (should (eq 'failed (slack-page-state-status state)))
              (should (equal expected (slack-page-state-error state)))
              (when same-object-p
                (should (eq expected (slack-page-state-error state))))
              (with-current-buffer (oref object buf)
                (should (string-match-p "Retry" (buffer-string)))
                (should-not
                 (string-match-p "request-response" (buffer-string)))))
          (when (buffer-live-p (oref object buf))
            (kill-buffer (oref object buf))))))))

(ert-deftest slack-test-present-page-initial-renderer-failure-is-terminal ()
  (let* ((state (slack-page-state-create))
         (team (make-instance 'slack-team))
         (object (make-instance 'slack-test-page-buffer :team-id nil))
         (renders 0)
         (loads 0)
         (errors 0))
    (slack-buffer-cache-team object team)
    (unwind-protect
        (cl-letf (((symbol-function 'slack-buffer-display) #'ignore))
          (should-error
           (slack-buffer-present-page
            object state
            (lambda (_generation success _error)
              (cl-incf loads)
              (funcall success '(value) nil nil))
            (lambda (_object _state)
              (when (= 1 (cl-incf renders))
                (error "Initial renderer boom")))
            nil nil
            (lambda (_state _error) (cl-incf errors))))
          (should (eq 'failed (slack-page-state-status state)))
          (should (= 1 errors))
          (should-not (slack-page-state-commit-waiters state))
          (should-not (slack-page-state-ready-waiters state))
          (should-not (slack-page-state-error-waiters state))
          (with-current-buffer (oref object buf)
            (slack-buffer-page-retry))
          (should (= 1 loads))
          (should (= 2 (slack-page-state-generation state)))
          (should (eq 'ready (slack-page-state-status state))))
      (when (buffer-live-p (oref object buf))
        (kill-buffer (oref object buf))))))

(ert-deftest slack-test-present-page-display-failure-is-terminal ()
  (let* ((state (slack-page-state-create))
         (team (make-instance 'slack-team))
         (object (make-instance 'slack-test-page-buffer :team-id nil))
         (displays 0)
         (loads 0)
         (errors 0))
    (slack-buffer-cache-team object team)
    (unwind-protect
        (cl-letf (((symbol-function 'slack-buffer-display)
                   (lambda (_object)
                     (when (= 1 (cl-incf displays))
                       (error "Display boom")))))
          (should-error
           (slack-buffer-present-page
            object state
            (lambda (_generation success _error)
              (cl-incf loads)
              (funcall success '(value) nil nil))
            #'ignore nil nil
            (lambda (_state _error) (cl-incf errors))))
          (should (eq 'failed (slack-page-state-status state)))
          (should (= 1 errors))
          (should-not (slack-page-state-commit-waiters state))
          (should-not (slack-page-state-ready-waiters state))
          (should-not (slack-page-state-error-waiters state))
          (with-current-buffer (oref object buf)
            (slack-buffer-page-retry))
          (should (= 1 loads))
          (should (= 2 (slack-page-state-generation state)))
          (should (eq 'ready (slack-page-state-status state))))
      (when (buffer-live-p (oref object buf))
        (kill-buffer (oref object buf))))))

(ert-deftest slack-test-present-page-loader-signal-is-terminal ()
  (let* ((state (slack-page-state-create))
         (team (make-instance 'slack-team))
         (object (make-instance 'slack-test-page-buffer :team-id nil))
         (loads 0)
         (errors 0))
    (slack-buffer-cache-team object team)
    (unwind-protect
        (cl-letf (((symbol-function 'slack-buffer-display) #'ignore))
          (should-error
           (slack-buffer-present-page
            object state
            (lambda (_generation success _error)
              (if (= 1 (cl-incf loads))
                  (error "Loader boom")
                (funcall success '(value) nil nil)))
            #'ignore nil nil
            (lambda (_state _error) (cl-incf errors))))
          (should (eq 'failed (slack-page-state-status state)))
          (should (= 1 errors))
          (should-not (slack-page-state-commit-waiters state))
          (should-not (slack-page-state-ready-waiters state))
          (should-not (slack-page-state-error-waiters state))
          (with-current-buffer (oref object buf)
            (slack-buffer-page-retry))
          (should (= 2 loads))
          (should (= 2 (slack-page-state-generation state)))
          (should (eq 'ready (slack-page-state-status state))))
      (when (buffer-live-p (oref object buf))
        (kill-buffer (oref object buf))))))

(ert-deftest slack-test-present-page-coalesces-exact-buffer-subscribers ()
  (let* ((state (slack-page-state-create))
         (team (make-instance 'slack-team))
         (first (make-instance 'slack-test-page-buffer :team-id nil))
         (second (make-instance 'slack-test-page-buffer :team-id nil))
         (killed (make-instance 'slack-test-page-buffer :team-id nil))
         (requests 0)
         success
         renders)
    (unwind-protect
        (cl-letf (((symbol-function 'slack-buffer-display) #'ignore))
          (dolist (object (list first second killed))
            (slack-buffer-cache-team object team)
            (slack-buffer-present-page
             object state
             (lambda (_generation on-success _on-error)
               (cl-incf requests)
               (setq success on-success))
             (lambda (rendered-object rendered-state)
               (push (list rendered-object
                           (current-buffer)
                           (slack-page-state-status rendered-state)
                           (slack-page-state-value rendered-state))
                     renders))))
          (let ((first-buffer (oref first buf))
                (second-buffer (oref second buf))
                (dead (oref killed buf)))
            (kill-buffer dead)
            (setq renders nil)
            (funcall success '(value) nil nil)
            (should (= 1 requests))
            (should (= 2 (length renders)))
            (should (member (list first first-buffer 'ready '(value))
                            renders))
            (should (member (list second second-buffer 'ready '(value))
                            renders))
            (should (eq dead (oref killed buf)))
            (should-not (buffer-live-p dead))))
      (dolist (object (list first second killed))
        (when (buffer-live-p (oref object buf))
          (kill-buffer (oref object buf)))))))

(ert-deftest slack-test-page-status-shows-loading-and-refreshing ()
  (let ((buffer (generate-new-buffer " *slack-test-page-status*"))
        (object (make-instance 'slack-test-page-buffer))
        (state (slack-page-state-create)))
    (unwind-protect
        (with-current-buffer buffer
          (slack-page-state-begin state)
          (slack-buffer-insert-page-status object state)
          (should (equal "Loading Slack data…\n" (buffer-string)))
          (should (eq 'loading
                      (get-text-property (point-min) 'slack-page-status)))
          (slack-page-state-commit state 1 '(cached) nil nil)
          (slack-page-state-begin state t)
          (goto-char (point-max))
          (slack-buffer-insert-page-status object state)
          (should (equal "Refreshing Slack data…\n" (buffer-string)))
          (should (eq 'refreshing
                      (get-text-property (point-min) 'slack-page-status))))
      (kill-buffer buffer))))

(ert-deftest slack-test-page-status-failure-preserves-payload-and-offers-retry ()
  (let ((buffer (generate-new-buffer " *slack-test-page-failure*"))
        (object (make-instance 'slack-test-page-buffer))
        (state (slack-page-state-create)))
    (unwind-protect
        (with-current-buffer buffer
          (insert "Existing Slack payload\n")
          (let ((generation (slack-page-state-begin state)))
            (slack-page-state-fail state generation '(http-error 503)))
          (slack-buffer-insert-page-status object state)
          (let* ((text (buffer-string))
                 (retry-position (string-match "Retry" text))
                 (button (and retry-position
                              (button-at (1+ retry-position)))))
            (should (string-match-p "Existing Slack payload" text))
            (should (string-match-p "Slack request failed" text))
            (should (string-match-p "(http-error 503)" text))
            (should button)
            (should (eq #'slack-buffer-page-retry
                        (button-get button 'action)))
            (should (button-get button 'follow-link))
            (should (eq 'failed
                        (get-text-property (1- (point-max))
                                           'slack-page-status)))))
      (kill-buffer buffer))))

(ert-deftest slack-test-remove-page-status-removes-only-status-regions ()
  (let ((buffer (generate-new-buffer " *slack-test-remove-status*"))
        (object (make-instance 'slack-test-page-buffer)))
    (unwind-protect
        (with-current-buffer buffer
          (insert "alpha\n")
          (insert (propertize "Loading Slack data…\n"
                              'slack-page-status 'loading))
          (insert (propertize "beta\n" 'face 'bold))
          (insert (propertize "Slack request failed  Retry\n"
                              'slack-page-status 'failed))
          (insert "gamma\n")
          (slack-buffer-remove-page-status object)
          (should (equal "alpha\nbeta\ngamma\n" (buffer-string)))
          (should (eq 'bold (get-text-property 7 'face)))
          (should-not (text-property-not-all
                       (point-min) (point-max) 'slack-page-status nil)))
      (kill-buffer buffer))))

(ert-deftest slack-test-page-retry-uses-current-buffer-closure ()
  (let ((buffer (generate-new-buffer " *slack-test-page-retry*"))
        (calls 0))
    (unwind-protect
        (with-current-buffer buffer
          (setq-local slack-buffer-page-retry-function
                      (lambda () (cl-incf calls)))
          (slack-buffer-page-retry)
          (should (= 1 calls))
          (setq-local slack-buffer-page-retry-function nil)
          (should-error (slack-buffer-page-retry) :type 'user-error))
      (kill-buffer buffer))))

(ert-deftest slack-test-team-page-state-survives-buffer-cache-removal ()
  (let* ((team (make-instance 'slack-team))
         (first (slack-team-page-state
                 team '(search messages "query" recent desc))))
    (oset team slack-message-buffer (make-hash-table :test 'equal))
    (puthash 'temporary-buffer t (oref team slack-message-buffer))
    (clrhash (oref team slack-message-buffer))
    (should (eq first
                (slack-team-page-state
                 team '(search messages "query" recent desc))))))

(provide 'test-page-state)
;;; test-page-state.el ends here
