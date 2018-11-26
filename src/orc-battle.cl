(defparameter *player-health* nil)

(defparameter *player-agility* nil)

(defparameter *player-strength* nil)

(defparameter *monsters* nil)

(defparameter *monster-builders* nil)

(defparameter *monster-num* 12)

(defun orc-battle ()
  (init-monsters)
  (init-player)
  (game-loop)
  (when (player-dead)
    (princ "You have been killed. Game Over."))
  (when (monsters-dead)
    (princ "Congratulations! You have vanquished all of your foes.")))

(defun game-loop ()
  (unless (or (player-dead) (monsters-dead))
    (show-player)
    (dotimes (k (1+ (truncate (/ (max 0 *player-agility*) 15))))
      (unless (monsters-dead)
        (show-monsters)
        (player-attack)))
    (fresh-line)
    (map 'list
      (lambda (m)
        (or (monster-dead m) (monster-attack m)))
      *monsters*)
    (game-loop)))

; ===========================
; Player management functions
; ===========================

(defun init-player ()
  (setf *player-health* 30)
  (setf *player-agility* 30)
  (setf *player-strength* 30))

(defun player-dead ()
  (<= *player-health* 0))

(defun show-player ()
  (format t
    "You are a valiant knight wint a health of ~A, an agility of ~A, and a strength of ~A~%"
    *player-health*
    *player-agility*
    *player-strength*))

(defun player-attack ()
  (format t "Attack style: [s]tab [d]ouble [r]oundhouse:")
  (case (read)
    (s (monster-hit 
         (pick-monster)
         (+ 2 (randval (ash *player-strength* -1)))))
    (d (let ((x (randval (truncate (/ *player-strength* 6)))))
         (princ "Your double swing has a strength of ")
         (princ x)
         (fresh-line)
         (monster-hit (pick-monster) x)))
    (otherwise
      (dotimes (x (1+ (randval (truncate (/ *player-strength* 3)))))
        (unless (monsters-dead)
          (monster-hit (random-monster) 1))))))

(defun randval (n)
  (1+ (random (max 1 n))))

(defun random-monster ()
  (let ((m (aref *monsters* (random (length *monsters*)))))
    (if (monster-dead m)
      (random-monster) m)))

(defun pick-monster ()
  (fresh-line)
  (princ "Monster #:")
  (let ((x (read)))
    (if (not (and (integerp x) (>= x 1) (<= x *monster-num*)))
      (progn
        (princ "That is not a valid monster number.")
        (pick-monster))
      (let ((m (aref *monsters* (1- x))))
        (if (monster-dead m)
          (progn
            (princ "That monster is already dead.")
            (pick-monster))
          m)))))

; ============================
; Monster management functions
; ============================

(defun init-monsters ()
  (setf *monsters*
    (map 'vector
      (lambda (x)
        (funcall 
          (nth (random (length *monster-builders*)) 
            *monster-builders*)))
      (make-array *monster-num*))))

(defun monster-dead (m)
  (<= (monster-health m) 0))

(defun monsters-dead ()
  (every #'monster-dead *monsters*))

(defun show-monsters ()
  (fresh-line)
  (princ "Your foes:")
  (let ((x 0))
    (map 'list
      (lambda (m)
        (fresh-line)
        (format t "  ~A. " (incf x))
        (if (monster-dead m)
          (princ "**dead**")
          (progn
            (format t "(Health=~A) " (monster-health m))
            (monster-show m))))
      *monsters*)))

; ========
; Monsters
; ========

(defstruct monster
  (health (randval 10)))

(defmethod monster-hit (m x)
  (decf (monster-health m) x)
  (if (monster-dead m)
    (format t "You killed the ~A! " (type-of m))
    (format t "You hit the ~A, knocking off ~A health points! "
      (type-of m) x)))

(defmethod monster-show (m)
  (format t "A fierce ~A" (type-of m)))

(defmethod monster-attack (m))