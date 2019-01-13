(defun print-tag (name alst closingp)
  (princ #\<)
  (when closingp
    (princ #\/))
  (princ (string-downcase name))
  (mapc (lambda (att)
          (format t " ~a=\"~a\""
            (string-downcase (car att))
            (cdr att)))
    alst)
  (princ #\>))

(defun pairs (xs)
  (when xs
    (cons (cons (car xs) (cadr xs))
          (pairs (cddr xs)))))

(defmacro tag (name attrs &body body)
  `(progn
     (print-tag 
       ',name
       (list ,@(mapcar 
                 (lambda (x) `(cons ',(car x) ,(cdr x)))
                 (pairs attrs)))
       nil)
     ,@body
     (print-tag ',name nil t)))
