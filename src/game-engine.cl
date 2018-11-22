(defparameter *nodes*
  '((living-room
     (you are in the living room. a wizard is snoring loudly on the couch.))
    (garden
     (you are in the beautiful garden. there is a well in front of you.))
    (attic
     (you are in the attic. there is a giant welding torch in the corner.))))

(defparameter *location* 'living-room)

(defparameter *edges*
  '((living-room
     (garden west door)
     (attic upstairs ladder))
    (garden
     (living-room east door))
    (attic
     (living-room downstairs ladder))))

(defparameter *objects*
  '(whiskey
    bucket
    frog
    chain))

(defparameter *object-locations*
  '((whiskey living-room)
    (bucket living-room)
    (chain garden)
    (frog garden)))

(defvar *allowed-commands*
  '(look walk pickup inventory))

(defun describe-location (loc nodes)
  (cadr (assoc loc nodes)))

(defun describe-path (edge)
  `(there is ,(caddr edge) going ,(cadr edge) from here.))

(defun describe-paths (location edges)
  (apply #'append (mapcar #'describe-path (cdr (assoc location edges)))))

(defun objects-at (loc objs obj-locs)
  (remove-if-not
    (lambda (o) (eq loc (cadr (assoc o obj-locs))))
    objs))

(defun describe-objects (loc objs obj-loc)
  (apply #'append
    (mapcar (lambda (o) `(you see a ,o on the floor.))
      (objects-at loc objs obj-loc))))

(defun look ()
  (append
    (describe-location *location* *nodes*)
    (describe-paths *location* *edges*)
    (describe-objects *location* *objects* *object-locations*)))

(defun walk (direction)
  (let ((next (find direction
                (cdr (assoc *location* *edges*))
                :key #'cadr)))
    (if next
      (progn
        (setf *location* (car next))
        (look))
      '(you cannot go that way))))

(defun pickup (object)
  (cond
    ((member object 
      (objects-at *location* *objects* *object-locations*))
     (push (list object 'body) *object-locations*)
     `(you are now carrying the ,object))
    (t '(you cannoy get that))))

(defun inventory ()
   (cons 'items- (objects-at 'body *objects* *object-locations*)))