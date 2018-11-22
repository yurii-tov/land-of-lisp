; =================
; examples 'engine'
; =================

(defparameter *decorate-line-limit* 60)

(defparameter *topics* nil)

(defun make-lines (words line)
  (let ((word (car words)))
    (if word
      (let ((word-length (length word)))
        (if (> word-length *decorate-line-limit*)
          (make-lines
            (cons 
              (substring word 0 *decorate-line-limit*)
              (cons (substring word 
                      *decorate-line-limit* 
                      word-length)
                (cdr words)))
            line)
          (let ((line-appended 
                 (string-trim " " (format nil "~A ~A" line word))))
            (if (<= (length line-appended) 
                  *decorate-line-limit*)
              (make-lines (cdr words) line-appended)
              (cons line (make-lines (cdr words) word))))))
      (list line))))

(defun decorate (text letter)
  (let* ((words (REGEXP:REGEXP-SPLIT " " (substitute #\space #\newline text)))
         (lines (make-lines words ""))
         (decoration 
          (reduce 
            (lambda (s x) (concatenate 'string s x)) 
            (loop for x from 1 
              to (reduce (lambda (s x) (if (> x s) x s)) 
                   (mapcar #'length lines))
              collect letter)))
         (text-formatted 
          (reduce (lambda (a x) (format nil "~A~%~A" a x)) lines)))
    (format nil "~A~%~A~%~A" 
      decoration 
      text-formatted 
      decoration)))

(defun show-example (example)
  "Arguments: list of (description form)
  Prints explanation of given example"
  (let* ((stdout-stream (make-string-output-stream))
         (eval-result
          (with-open-stream 
            (*standard-output* stdout-stream)
            (eval (cadr example))))
         (out (get-output-stream-string stdout-stream)))
    (format t "~A~%==> ~S~%==> ~A~%" 
      (decorate (car example) "-")
      (cadr example)
      eval-result)
    (when (> (length out) 0)
      (format t "stdout:~%~A~%" out))
    (format t "~%")))

(defun parse-examples (examples)
  (when examples
    (let ((description (car examples))
          (form (cadr examples)))
      (assert (and description form))
      (cons (list description form) 
        (parse-examples (cddr examples))))))

(defun enum-examples (examples)
  (mapcar #'cons 
    (loop for i from 1 to (length examples) collect i) 
    examples))

(defmacro deftopic (title &rest examples)
  "Creates function which prints examples on given topic.
  Syntax is: 
  (deftopic topic-title 
    description form 
    description form etc ...)"
  (assert examples)
  (let 
    ((examples-enumerated (enum-examples (parse-examples examples)))
     (fn-name (read-from-string 
                (format nil "explain-~A" 
                  (prin1-to-string title)))))
    (push 
      (list
       title
       (lambda ()
         (format t "~A~2%" (decorate (format nil "~A" title) "="))
         (loop for example in examples-enumerated
           do
           (format t "Example ~A~%" (car example)) 
           (show-example (cdr example))))) 
      *topics*)
    `(defun ,fn-name (&rest args)
       (funcall (cadr (assoc (quote ,title) *topics*))))))

(defun explain-all ()
  (loop for example in *topics*
    do (funcall (cadr example))))

; ======
; topics
; ======

(deftopic arrays
  "You can create arrays with make-array function"
  (make-array 3 :initial-contents '(a b c))

  "Without any key parameters, array appears filled with NIL's..."
  (make-array 5))

(deftopic lambda
  "LAMBDA macro creates an anonymous function. For example, this is a function that returns square of a number"
  (lambda (x) (* x x)))

(deftopic circular-list
  "Circular list is created when we assoc last cons cell to list itself"
  (let ((list '(a b c d)))
    (progn
      (setf *print-circle* t)
      (setf (cddddr list) list)
      list)))

(deftopic format
  "% means newline" 
  (format t "hello with 1 newline~%")

  "2% means 2 newlines" 
  (format t "hello with two newlines~2%")

  "S stands for 'standard'. It formats any objects in machine-readable view (like prin1)"
  (format t 
    "this is a list: ~S, and this is a string: ~S. This is a symbol: ~S" 
    '(a b c d) "a string" 'dog-tag)

  "A stands for 'aestetic'. It formats any objects in human-readable view (like princ)"
  (format t 
    "this is a list: ~A, and this is a string: ~A. This is a symbol: ~A" 
    '(a b c d) "a string" 'dog-tag))

(deftopic printing
  "PRIN1 function prints data in machine-readable format (i.e. its output is available for the Reader)"
  (prin1 "PRIN1 function prints data in machine-readable format (i.e. its output is available for the Reader)")

  "PRINT is like PRIN1, but it: 1. starts printing on new line; 2. Adds space character on the end of output"
  (print "PRINT is like PRIN1, but it: 1. starts printing on new line; 2. Adds space character on the end of output")

  "PRINC function prints data in human-readable format"
  (princ "PRINC function prints data in human-readable format"))

(deftopic coercion
  "COERCE function converts its argument to given type"
  (coerce "that string is gonna blow!" 'list))

(deftopic streams
  "Streams is abstractions for dealing with various sources of input/output.
  For example, WITH-OPEN-STREAM macro binds stream to given name, performs any operations using that name, and then close stream"
  (let ((my-stream (make-string-output-stream)))
    (with-open-stream (s my-stream)
      (format s "just write to your stream"))
    (get-output-stream-string my-stream)))