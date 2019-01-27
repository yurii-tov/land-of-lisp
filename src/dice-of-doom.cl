(load "src/lazy.cl")

(defparameter *num-players* 2)

(defparameter *max-dice* 3)

(defparameter *board-size* 4)

(defparameter *board-hexnum* (* *board-size* *board-size*))

(defparameter *ai-level* 4)

;; =====
;; board
;; =====

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

;; =========
;; game tree
;; =========

(labels 
  ((old-game-tree (board player spare-dice first-move)
     (list 
       player
       board
       (add-passing-move 
         board
         player
         spare-dice
         first-move
         (attacking-moves board player spare-dice)))))
  (let ((previous (make-hash-table :test #'equalp)))
    (defun game-tree (&rest rest)
      (or (gethash rest previous)
        (setf (gethash rest previous) (apply #'old-game-tree rest))))))

(defun limit-tree-depth (tree depth)
  (list 
   (car tree)
   (cadr tree)
   (if (zerop depth)
     (lazy-nil)
     (lazy-mapcar
       (lambda (move)
         (list 
          (car move)
          (limit-tree-depth (cadr move) (1- depth))))
       (caddr tree)))))

(defun add-passing-move (board player spare-dice first-move moves)
  (if first-move
    moves
    (lazy-cons 
      (list nil 
        (game-tree 
          (add-new-dice board player (1- spare-dice))
          (mod (1+ player) *num-players*)
          0
          t))
      moves)))

(defun attacking-moves (board cur-player spare-dice)
  (labels ((player (pos)
             (car (aref board pos)))
           (dice (pos)
             (cadr (aref board pos))))
    (lazy-mapcan 
      (lambda (src)
        (if (eq (player src) cur-player)
          (lazy-mapcan 
            (lambda (dst)
              (if (and (not (eq (player dst) cur-player))
                       (> (dice src) (dice dst)))
                (lazy-list
                  (list 
                   (list src dst)
                   (game-tree
                     (board-attack board cur-player src dst (dice src))
                     cur-player
                     (+ spare-dice (dice dst))
                     nil)))
                (lazy-nil)))
            (make-lazy (neighbours src)))
          (lazy-nil)))
      (make-lazy
        (loop for n below *board-hexnum*
          collect n)))))

(labels 
  ((old-neighbours (pos)
     (let ((up (- pos *board-size*))
           (down (+ pos *board-size*)))
       (remove-if-not
         (lambda (p) (and (>= p 0) (< p *board-hexnum*)))
         (append 
           (list up down)
           (unless (zerop (mod pos *board-size*))
             (list (1- up) (1- pos)))
           (unless (zerop (mod (1+ pos) *board-size*))
             (list (1+ pos) (1+ down))))))))
  (let ((previous (make-hash-table)))
    (defun neighbours (pos)
      (or (gethash pos previous)
        (setf (gethash pos previous)
          (old-neighbours pos))))))

(defun board-attack (board player src dst dice)
  (board-array
    (loop for pos
          for hex across board
          collect
          (cond ((eq pos src) (list player 1))
                ((eq pos dst) (list player (1- dice)))
                (t hex)))))

(defun add-new-dice (board player spare-dice)
  (labels 
    ((f (lst n acc)
       (cond ((null lst) (reverse acc))
             ((zerop n) (append (reverse acc) lst))
             (t (let ((cur-player (caar lst))
                      (cur-dice (cadar lst)))
                  (if (and (eq cur-player player)
                           (< cur-dice *max-dice*))
                    (f (cdr lst) (1- n) 
                      (cons (list cur-player (1+ cur-dice)) acc))
                    (f (cdr lst) n (cons (car lst) acc))))))))
    (board-array (f (coerce board 'list) spare-dice nil))))

;; ==============
;; user interface
;; ==============

(defun play-vs-human (tree)
  (print-info tree)
  (if (not (lazy-null (caddr tree)))
    (play-vs-human (handle-human tree))
    (announce-winner (cadr tree))))

(defun print-info (tree)
  (fresh-line)
  (format t "current player = ~a" (player-letter (car tree)))
  (draw-board (cadr tree)))

(defun print-action (action)
  (if action
    (format t "~a -> ~a" (car action) (cadr action))
    (princ "end turn")))

(defun handle-human (tree)
  (fresh-line)
  (princ "choose your move:")
  (let ((moves (caddr tree)))
    (labels ((print-moves (moves n)
               (unless (lazy-null moves)
                 (let* ((move (lazy-car moves))
                        (action (car move)))
                   (fresh-line)
                   (format t "~a. " n)
                   (print-action action))
                 (print-moves (lazy-cdr moves) (1+ n)))))
      (print-moves moves 1))
    (fresh-line)
    (cadr (lazy-nth (1- (read)) moves))))

(defun winners (board)
  (let* ((tally (loop for hex across board
                  collect (car hex)))
         (totals (mapcar (lambda (player)
                           (cons player (count player tally)))
                   (remove-duplicates tally)))
         (best (apply #'max (mapcar #'cdr totals))))
    (mapcar #'car
      (remove-if (lambda (x) (not (eq (cdr x) best)))
        totals))))

(defun announce-winner (board)
  (fresh-line)
  (let ((w (winners board)))
    (if (> (length w) 1)
      (format t "The game is a tie between ~a" (mapcar #'player-letter w))
      (format t "The winner is ~a" (player-letter (car w))))))

;; ==
;; AI
;; ==

(defun rate-position (tree player)
  (let ((moves (caddr tree)))
    (if (not (lazy-null moves))
      (apply (if (eq (car tree) player) #'max #'min)
        (get-ratings tree player))
      (score-board (cadr tree) player))))

(defun threatened (pos board)
  (let* ((hex (aref board pos))
         (player (car hex))
         (dice (cadr hex)))
    (loop for n in (neighbours pos)
      do (let* ((nhex (aref board n))
                (nplayer (car nhex))
                (ndice (cadr nhex)))
           (when (and (not (eq player nplayer)) (> ndice dice))
             (return t))))))

(defun score-board (board player)
  (loop for hex across board
        for pos from 0
        sum (if (eq (car hex) player)
              (if (threatened pos board) 1 2)
              -1)))

(defun get-ratings (tree player)
  (take-all
    (lazy-mapcar 
      (lambda (move) 
        (rate-position (cadr move) player))
      (caddr tree))))

(defun handle-computer (tree)
  (let ((ratings (get-ratings (limit-tree-depth tree *ai-level*) (car tree))))
    (lazy-nth 
      (position (apply #'max ratings) ratings) 
      (caddr tree))))

(defun play-vs-computer (tree)
  (print-info tree)
  (cond ((lazy-null (caddr tree)) (announce-winner (cadr tree)))
        ((zerop (car tree)) (play-vs-computer (handle-human tree)))
        (t (destructuring-bind (action tree-1) (handle-computer tree)
             (fresh-line)
             (print-action action)
             (play-vs-computer tree-1)))))

(defun new-game-vs-computer ()
  (play-vs-computer (game-tree (gen-board) (random 2) 0 t)))
