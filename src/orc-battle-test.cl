(defun with-stats (stats-initial test)
  (let ((*player-health* (car stats-initial))
        (*player-agility* (cadr stats-initial))
        (*player-strength* (caddr stats-initial)))
    (funcall test)))

(defun assert-stats (expected)
  (assert 
    (equal 
      (list *player-health* 
            *player-agility* 
            *player-strength*) 
      expected)))

; =============
; brigand tests
; =============

; attacks

(mapc
  (lambda (v)
    (with-stats (car v) 
      (lambda ()
        (let ((b (make-brigand))
              (expected (cadr v)))
          (monster-attack b)
          (assert-stats expected)))))
  '(((11 10 10) (9 10 10))
    ((10 11 10) (10 9 10))
    ((10 10 11) (10 10 9))
    ((10 10 10) (8 10 10))))

; take and release agility

(with-stats
  '(10 15 10)
  (lambda ()
    (let ((b (make-brigand)))
      (setf (monster-health b) 1)
      ; take agility
      (monster-attack b)
      (monster-attack b)
      (assert (equal (steals-agility-taken b) 4))
      ; release agility
      (monster-hit b 1)
      (assert-stats '(10 15 10))
      (assert (equal (steals-agility-taken b) 0)))))