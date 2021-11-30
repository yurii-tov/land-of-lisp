(defparameter *player-health* nil)

(defparameter *player-agility* nil)

(defparameter *player-strength* nil)

(defparameter *monsters* nil)

(defparameter *monster-builders* nil)

(defun orc-battle (&key (player-health 30)
                     (player-agility 30)
                     (player-strength 30)
                     (monster-num 10))
  (let ((*player-health* player-health)
        (*player-agility* player-agility)
        (*player-strength* player-strength)
        (*monsters* (make-monsters monster-num)))
    (game-loop)
    (when (player-dead)
      (format t "You have been killed. Game Over.~%"))
    (when (monsters-dead)
      (format t "Congratulations! You have vanquished all of your foes.~%"))))

(defun game-loop ()
  (unless (or (player-dead) (monsters-dead))
    (show-player)
    (dotimes (k (1+ (truncate (/ (max 0 *player-agility*) 15))))
      (unless (monsters-dead)
        (show-monsters)
        (player-attack)))
    (fresh-line)
    (loop for m across *monsters*
          unless (player-dead)
          do (or (monster-dead m)
                 (monster-attack m)))
    (game-loop)))

;; ===========================
;; Player management functions
;; ===========================

(defun player-dead ()
  (<= *player-health* 0))

(defun show-player ()
  (format t
          "~%You are a valiant knight with a health of ~A, an agility of ~A, and a strength of ~A~%"
          *player-health*
          *player-agility*
          *player-strength*))

(defun player-attack ()
  (fresh-line)
  (format t "Attack style: [s]tab [d]ouble [r]oundhouse:")
  (force-output)
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
  (force-output)
  (let ((x (read)))
    (if (not (and (integerp x) (>= x 1) (<= x (length *monsters*))))
        (progn
          (princ "That is not a valid monster number.")
          (pick-monster))
        (let ((m (aref *monsters* (1- x))))
          (if (monster-dead m)
              (progn
                (princ "That monster is already dead.")
                (pick-monster))
              m)))))

;; ============================
;; Monster management functions
;; ============================

(defun make-monsters (monster-num)
  (map 'vector
       (lambda (x)
         (funcall
          (nth (random (length *monster-builders*))
               *monster-builders*)))
       (make-array monster-num)))

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

;; ========
;; Monsters
;; ========

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

;; orc

(defstruct (orc (:include monster))
  (club-level (randval 8)))

(push #'make-orc *monster-builders*)

(defmethod monster-show ((m orc))
  (format t "A wicked orc with a level ~A club"
          (orc-club-level m)))

(defmethod monster-attack ((m orc))
  (let ((x (randval (orc-club-level m))))
    (decf *player-health* x)
    (format t "An orc swings his club at you and knocks off ~A of your health points! " x)))

;; hydra

(defstruct (hydra (:include monster)))

(push #'make-hydra *monster-builders*)

(defmethod monster-show ((m hydra))
  (format t "A malicious hydra with ~A heads"
          (monster-health m)))

(defmethod monster-hit ((m hydra) x)
  (decf (monster-health m) x)
  (if (monster-dead m)
      (format t "The corpse of fully decapitated and decapacitated hydra falls to the floor! ")
      (format t "You lop off ~A of the hydra's heads! " x)))

(defmethod monster-attack ((m hydra))
  (let ((x (randval (ash (monster-health m) -1))))
    (incf (monster-health m))
    (decf *player-health* x)
    (format t
            "A hydra attacks you with ~A of its heads! It also grows back one more head! "
            x)))

;; monsters which steal agility

(defstruct (steals-agility (:include monster))
  (taken 0))

(defmethod steal-agility ((m steals-agility) x)
  (decf *player-agility* x)
  (incf (steals-agility-taken m) x))

(defmethod monster-hit :after ((m steals-agility) x)
  (when (monster-dead m)
    (let ((a (steals-agility-taken m)))
      (when (not (zerop a))
        (incf *player-agility* a)
        (setf (steals-agility-taken m) 0)
        (format t "You get back stolen ~a agility points) " a)))))

;; slime mold

(defstruct (slime-mold (:include steals-agility))
  (slimeness (randval 5)))

(push #'make-slime-mold *monster-builders*)

(defmethod monster-show ((m slime-mold))
  (format t "A slime mold with a slimeness of ~A"
          (slime-mold-slimeness m)))

(defmethod monster-attack ((m slime-mold))
  (let ((x (randval (slime-mold-slimeness m))))
    ;; bind player
    (steal-agility m x)
    (format t "A slime mold wraps around your legs and decreases your agility by ~A! " x)
    ;; hit player
    (when (zerop (random 2))
      (decf *player-health*)
      (format t " It also squirts in your face, taking away a health point! "))))

;; cunning brigand

(defstruct (brigand (:include steals-agility)))

(push #'make-brigand *monster-builders*)

(defmethod monster-attack ((m brigand))
  (let ((x (max *player-health*
                *player-agility*
                *player-strength*)))
    (cond
      ((= x *player-health*)
       (princ "A brigand hits you with his slingshot, taking off 2 health points! ")
       (decf *player-health* 2))
      ((= x *player-agility*)
       (princ "a brigand catches your leg with his whip, taking off 2 agility points! ")
       (steal-agility m 2))
      ((= x *player-strength*)
       (princ "A brigand cuts your arm with his whip, taking off 2 strength points! ")
       (decf *player-strength* 2)))))

;;;;;;;;;;;;;;;;;;;;;
;; cli entry point ;;
;;;;;;;;;;;;;;;;;;;;;

(defun orc-battle-cli ()
  (let ((args (cdr *posix-argv*)))
    (if (or (not args) (oddp (length args)))
        (format t "Parameters:
[-monsters <count of monsters to fight>]
[-health <your health>]
[-agility <your agility>]
[-strength <your strength>]~%")
        (let ((game-args (mapcan (lambda (x) (when (car x)
                                               (list (cdar x)
                                                     (let ((i (read-from-string (cadr x))))
                                                       (assert (integerp i))
                                                       i))))
                                 (loop for xs = args then (cddr xs)
                                       while xs
                                       collect
                                       (list (assoc (car xs) '(("-monsters" . :monster-num)
                                                               ("-health" . :player-health)
                                                               ("-agility" . :player-agility)
                                                               ("-strength" . :player-strength))
                                                    :test #'equal)
                                             (cadr xs))))))
          (apply #'orc-battle game-args)))))

;;;;;;;;;;;;;;;;;
;; compilation ;;
;;;;;;;;;;;;;;;;;


(defun compile-orc-battle ()
  (save-lisp-and-die "monster-battle.exe"
                     :executable t
                     :toplevel #'orc-battle-cli))
