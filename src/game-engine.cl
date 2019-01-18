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

(defparameter *chain-welded* nil)

(defparameter *bucket-filled* nil)

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

(defun have (object)
  (member object (inventory)))

(defmacro game-action (command subj obj place &body body)
  `(progn 
     (defun ,command (subject object)
       (if (and (eq *location* ',place)
                (eq subject ',subj)
                (eq object ',obj)
                (have ',subj))
         ,@body
         '(i cant ,command like that.)))
     (pushnew ',command *allowed-commands*)))

(game-action weld chain bucket attic
  (if (and (have 'bucket) (not *chain-welded*))
    (progn (setf *chain-welded* 't)
           '(the chain is now securely welded to the bucket.))
    '(you do not have a bucket.)))

(game-action dunk bucket well garden
  (if *chain-welded*
    (progn (setf *bucket-filled* 't)
           '(the bucket is now full of water))
    '(the water level is too low to reach.)))

(game-action splash bucket wizard living-room
  (cond ((not *bucket-filled*) '(the bucket has nothing in it.))
        ((have 'frog)
         '(the wizard awakens and sees that you stole his frog.
           he is so upset he banishes you to the netherworlds-
           you lose! the end.))
        (t '(the wizard awakens from his slumber and greets you warmly.
             he hands you the magic low-carb donut- you win! the end.))))
