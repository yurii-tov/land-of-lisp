(defun runner (tests
               failures 
               count-passed 
               count-failed)
  "(tests        => '((test-sexp-1 expected-sexp-1) 
                      (test-sexp-2 expected-sexp-2)
                      ...)
    failures     => ((test-sexp-1 expected-sexp-1 actual-value-1)
                     (test-sexp-1 expected-sexp-1 actual-value-1)
                     ...)
    count-passed
    count-failed)"
  (if tests
    (let* ((test     (caar tests))
           (expected (cadar tests))
           (actual   (eval test))
           (passedp (equal expected actual)))
      (if passedp
        (runner (cdr tests)
                failures
                (1+ count-passed)
                count-failed)
        (runner (cdr tests)
                (append failures (list (list test expected actual)))
                count-passed
                (1+ count-failed))))
    failures))

(defmacro run-tests (&rest tests)
  (labels ((pairs (xs)
             (when xs
               (cons (list (car xs) (cadr xs))
                     (pairs (cddr xs))))))
    (let* ((suite (pairs tests))
           (failures (runner suite nil 0 0))
           (count-failed (length failures))
           (count-total (length suite)))
      `(let ((failures ',failures))
         (if failures
           (progn
             (format t "tests failed!~%")
               (loop for (test expected actual) in failures
                 do (format t "expected: ~a => ~a, actual: ~a~%"
                      test expected actual))
               (format t "total failed tests: ~a/~a~%" 
                 ,count-failed 
                 ,count-total
                 failures))
           (format t "all tests passed! (~a)~%" ,count-total))))))
