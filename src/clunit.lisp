(in-package :check-it)

(defun test-group (test generator n)
  "Test a group of assertions. TEST is a function that represents the
   group. It should be called with the generated values and a number
   which specifies which assertions to run. If the number is zero run all
   of the assertions. If the number is less than N, then it designates a
   specific assertion to run."
  (let ((error-reporting-test
         (wrap-test-for-error-reporting test))
        (shrink-test
         (wrap-test-for-shrinking test)))
    (let ((*random-state* (make-random-state t)))
      (loop for test-num from 1 to n do
        (loop named trial-run
              finally (clunit::signal-assertion :pass)
              repeat *num-trials* do
          (generate generator)
           ;; produce readable representation before anybody mutates this value
          (let ((stringified-value (format nil "~S" (cached-value generator)))
                (result (funcall error-reporting-test (cached-value generator) test-num)))
            (flet ((do-shrink ()
                     (let ((shrunk (shrink generator (rcurry shrink-test test-num))))
                       (format *check-it-output* "~&Shrunken failure case: ~A~%" shrunk)
                       (format *check-it-output* "~&Testing all assertions that fail the shrunken case:~%")
                       (handler-case
                         (handler-bind ((clunit::assertion-failed (compose #'continue #'print-assertion))
                                        (clunit::assertion-passed #'continue))
                           (funcall test shrunk 0))
                         (error (c)
                           (format *check-it-output*
                                   "~&Error ~A signaled. Aborting rest of test.~%"
                                   c)))
                       (terpri *check-it-output*))))
              (typecase result
                (clunit::assertion-failed
                 ;; Resignal the condition so other places can use it
                 ;; to log information. Since we are using signal and
                 ;; not error, the debugger will not be entered if it
                 ;; isn't handled.
                 (signal result)
                 (print-assertion result)
                 (format *check-it-output* "~&Failed case: ~A~%" stringified-value)
                 (do-shrink)
                 (return-from trial-run nil))
                (reified-error
                 (format *check-it-output*
                         "~&Test signaled error ~A with arg ~A~%"
                         (wrapped-error result)
                         stringified-value)
                 (do-shrink)
                 (return-from trial-run nil))))))))))

(defun print-assertion (c)
  (format *check-it-output* "~&Assertion ~A failed.~%" (slot-value c 'clunit::expression)))

;; use-debugger need to be bound to true. Otherwise when we resignal
;; the error, the code that catches it will invoke the skip-test
;; restart. It would normally invoke the skip-assertion-restart but
;; since the stack has already been unwound a bit, that point has
;; already been unwound.
(defun run-test (test &rest args)
  (apply #'clunit:run-test test :use-debugger t :report-progress nil args))

(defun run-suite (suite &rest args)
  (apply #'clunit:run-suite suite :use-debugger t :report-progress nil args))
