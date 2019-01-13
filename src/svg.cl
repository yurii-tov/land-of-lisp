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

(defmacro tag (name-and-attrs &body body)
  (let* ((has-attrs-p (listp name-and-attrs))
         (name (if has-attrs-p (car name-and-attrs) name-and-attrs))
         (attrs (when has-attrs-p (cdr name-and-attrs))))
    `(progn
       (print-tag 
         ',name
         (list ,@(mapcar 
                   (lambda (x) `(cons ',(car x) ,(cdr x)))
                   (pairs attrs)))
         nil)
       ,@body
       (print-tag ',name nil t))))

(defmacro html (&body body)
  `(tag html ,@body))

(defmacro body (&body body)
  `(tag body ,@body))

; =======
; svg DSL
; =======

(defmacro svg (&body body)
  `(tag (svg xmlns "http://www.w3.org/2000/svg"
             "xmlns:xlink" "http://www.w3.org/1999/xlink")
     ,@body))

(defun brightness (col amt)
  (mapcar (lambda (x) 
            (min 255 (max 0 (+ x amt))))
    col))

(defun svg-style (color)
  (format nil
    "~{fill:rgb(~a,~a,~a);stroke:rgb(~a,~a,~a)~}"
    (append color (brightness color -100))))

(defun circle (center radius color)
  (tag (circle cx (car center)
               cy (cdr center)
               r radius
               style (svg-style color))))
