(defparameter *num-players* 2)

(defparameter *max-dice* 3)

(defparameter *board-size* 2)

(defparameter *board-hexnum* (* *board-size* *board-size*))

(defun board-array (lst)
  (make-array *board-hexnum* :initial-contents lst))

(defun gen-board ()
  (board-array 
    (loop for n below *board-hexnum*
      collect (list (random *num-players*)
                    (1+ (random *max-dice*))))))

(defun player-letter (n)
  (code-char (+ 97 n)))

(defun draw-board (board)
  (loop for y below *board-size*
    do (fresh-line)
       (loop repeat (- *board-size* y)
         do (princ "  "))
       (loop for x below *board-size*
         for hex = (aref board (+ x (* *board-size* y)))
         do (format t "~a-~a "
              (player-letter (first hex))
              (second hex)))))
