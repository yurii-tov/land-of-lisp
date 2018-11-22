(load "src/graph-utils.cl")

(defparameter *congestion-city-nodes* nil)
(defparameter *congestion-city-edges* nil)
(defparameter *visited-nodes* nil)
(defparameter *node-num* 30)
(defparameter *edge-num* 45)
(defparameter *worm-num* 3)
(defparameter *cop-odds* 15)

; =====================================
; Generate graph as list of connections
; =====================================

(defun random-node ()
  "pick a number from 1 to *node-num*
   Randomness is vital for this game"
  (1+ (random *node-num*)))

(defun edge-pair (a b)
  (unless (eql a b)
    (list (cons a b) (cons b a))))

(defun make-edge-list ()
  (apply #'append
    (loop 
      repeat *edge-num*
      collect (edge-pair (random-node) (random-node)))))

(defun direct-edges (node edge-list)
  (remove-if-not
    (lambda (x) (eql (car x) node))
    edge-list))

(defun get-connected (node edge-list)
  (labels
    ((traverse (visited node)
        (if (find node visited) visited
          (reduce
            (lambda (a x) (traverse a (cdr x)))
            (direct-edges node edge-list)
            :initial-value (cons node visited)))))
    (traverse nil node)))

(defun find-islands (nodes edge-list)
  (labels
    ((islands (land)
       (when land
         (let ((island (get-connected (car land) edge-list)))
           (cons island 
             (islands (set-difference land island)))))))
    (islands nodes)))

(defun create-bridges (islands)
  (when (cdr islands)
    (append (edge-pair (caar islands) (caadr islands))
      (create-bridges (cdr islands)))))

(defun connect-all-islands (nodes edge-list)
  (append (create-bridges (find-islands nodes edge-list))
    edge-list))

; =======================
; Generete graph as alist
; =======================

(defun make-city-edges ()
  (let* ((nodes (loop for x from 1 to *node-num* collect x))
         (edge-list (connect-all-islands nodes (make-edge-list)))
         (cops (remove-if-not
                 (lambda (x) (zerop (random *cop-odds*)))
                 edge-list)))
    (add-cops (edges-to-alist edge-list) cops)))

(defun edges-to-alist (edge-list)
  (mapcar
    (lambda (node1)
      (cons node1
        (mapcar (lambda (edge) (list (cdr edge)))
          (remove-duplicates
            (direct-edges node1 edge-list)
            :test #'equal))))
    (remove-duplicates (mapcar #'car edge-list))))

; -------------------
; performance improve
; -------------------

(defun edges-to-hashmap (edge-list)
  (let ((ht (make-hash-table)))
    (mapc 
      (lambda (edge)
        (pushnew (cdr edge) (gethash (car edge) ht)))
      edge-list)
    ht))

(defun get-connected-hash (node edges-hashmap)
  (let ((set (make-hash-table)))
    (labels ((traverse (n)
               (unless (gethash n set)
                 (setf (gethash n set) n)
                 (mapc
                   (lambda (x)
                     (traverse x))
                   (gethash n edges-hashmap)))))
      (traverse node)
      set)))

(defun add-cops (edge-alist edges-with-cops)
  (mapcar
    (lambda (x)
      (let ((node1 (car x))
            (node1-edges (cdr x)))
        (cons node1
          (mapcar
            (lambda (edge)
              (let ((node2 (car edge)))
                (if (intersection
                      (edge-pair node1 node2)
                      edges-with-cops
                      :test #'equal)
                  (list node2 'cops)
                  edge)))
            node1-edges))))
    edge-alist))

; ===========================================
; Generate list of nodes with additional data
; ===========================================

(defun neighbours (node edge-alist)
  (mapcar #'car (cdr (assoc node edge-alist))))

(defun within-one (a b edge-alist)
  (member b (neighbours a edge-alist)))

(defun within-two (a b edge-alist)
  (or (within-one a b edge-alist)
    (some
      (lambda (x) (within-one x b edge-alist)) 
      (neighbours a edge-alist))))

(defun make-city-nodes (edge-alist)
  (let ((wumpus (random-node))
        (glow-worms (loop for i below *worm-num*
                      collect (random-node))))
    (loop for n 
      from 1 to *node-num*
      collect
      (append (list n)
        (cond
          ((eql n wumpus) '(wumpus))
          ((within-two n wumpus edge-alist) '(blood!)))
        (cond
          ((member n glow-worms) '(glow-worm))
          ((some 
             (lambda (worm) (within-one n worm edge-alist))
             glow-worms)
           '(lights!)))
        (when (some #'cdr (cdr (assoc n edge-alist))) 
          '(sirens!))))))

; ==============
; Start new game
; ==============

(defun new-game ()
  (setf *congestion-city-edges* (make-city-edges))
  (setf *congestion-city-nodes* (make-city-nodes *congestion-city-edges*))
  (setf *player-pos* (find-empty-node))
  (setf *visited-nodes* (list *player-pos*))
  (draw-city)
  (draw-known-city))

(defun find-empty-node ()
  (let ((x (random-node)))
    (if (cdr (assoc x *congestion-city-nodes*))
      (find-empty-node) x)))

; ========================================
; Generate data for known part of the city
; ========================================

(defun known-city-nodes ()
  (mapcar
    (lambda (node)
      (if (member node *visited-nodes*)
        (let ((n (assoc node *congestion-city-nodes*)))
          (if (eql node *player-pos*)
            (append n '(*)) n))
        (list node '?)))
    (remove-duplicates
      (mapcan
        (lambda (node) 
          (cons node (neighbours node *congestion-city-edges*)))
        *visited-nodes*))))

(defun known-city-edges ()
  (mapcar
    (lambda (node)
      (cons node
        (mapcar
          (lambda (x)
            (if (member (car x) *visited-nodes*) x
              (list (car x))))
          (cdr (assoc node *congestion-city-edges*)))))
    *visited-nodes*))

; =========
; Draw city
; =========

(defun draw-city ()
  (ugraph->png "city" 
    *congestion-city-nodes* 
    *congestion-city-edges*))

(defun draw-known-city ()
  (ugraph->png "known-city"
    (known-city-nodes)
    (known-city-edges)))

; ==============
; Walking around
; ==============

(defun walk (pos)
  (handle-dtrection pos nil))

(defun charge (pos)
  (handle-dtrection pos t))

(defun handle-dtrection (pos charging)
  (let ((edge (assoc pos
                (cdr (assoc *player-pos* *congestion-city-edges*)))))
    (if edge
      (handle-new-place edge pos charging)
      (princ "That location does not exist!"))))

(defun handle-new-place (edge pos charging)
  (let*
    ((node (assoc pos *congestion-city-nodes*))
     (has-worm (and (member 'glow-worm node)
                    (not (member pos *visited-nodes*)))))
    (pushnew pos *visited-nodes*)
    (setf *player-pos* pos)
    (draw-known-city)
    (cond
      ((member 'cops edge) (princ "You ran into the cops. Game Over."))
      ((member 'wumpus node)
       (if charging
         (princ "You found the Wumpus!")
         (princ "You ran into the Wumpus")))
      (charging (princ "You wasted your last bullet. Game over."))
      (has-worm
        (let ((new-pos (random-node)))
          (format t "You ran into a Glow Worm Gang! You're now at ~A" new-pos)
          (handle-new-place nil new-pos nil))))))