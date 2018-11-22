(defmacro doc (fn)
  `(documentation (quote ,fn) 'function))

(defun slurp (stream)
  "Read stream contents as string"
  (reduce
    (lambda (a s) 
      (format nil "~A~%~A" a s))
    (loop for line = (read-line stream nil)
      while line 
      collect line)))