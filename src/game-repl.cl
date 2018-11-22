(defun game-repl ()
  (let ((cmd (game-read)))
    (unless (eq (car cmd) 'quit)
      (game-print (game-eval cmd))
      (game-repl))))

(defun game-read ()
  (let* ((parsed (read-from-string
                   (string-concat
                    "(" (read-line) ")")))
         (command (car parsed)))
    (cons command
      (mapcar
        (lambda (x) (list 'quote x)) 
        (cdr parsed)))))

(defun game-eval (form)
  (if (find (car form) *allowed-commands*)
    (eval form)
    `(allowed commands is - ,@*allowed-commands*)))

(defun tweak-chars (lst caps lit)
  (when lst
    (let ((item (car lst))
          (rest (cdr lst)))
      (cond
        ((eq item #\space)
         (cons item (tweak-chars rest caps lit)))
        ((find item '(#\! #\? #\.))
         (cons item (tweak-chars rest t lit)))
        ((eq item #\")
         (tweak-chars rest caps (not lit)))
        (lit 
         (cons item (tweak-chars rest nil lit)))
        (caps
         (cons (char-upcase item) (tweak-chars rest nil nil)))
        (t (cons (char-downcase item) (tweak-chars rest nil nil)))))))

(defun list-to-sentence (data)
  (let* 
    ((chars (coerce 
              (string-trim "() " (prin1-to-string data)) 
              'list))
     (tweaked-chars (tweak-chars chars t nil)))
    (coerce tweaked-chars 'string)))

(defun game-print (data)
  (format t "~A~%" (list-to-sentence data)))