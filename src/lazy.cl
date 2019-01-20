(defmacro lazy (&body body)
  (let ((value (gensym))
        (forced (gensym)))
    `(let ((,value nil)
           (,forced nil))
       (lambda ()
         (unless ,forced
           (progn (setf ,value (progn ,@body))
                  (setf ,forced t)))
         ,value))))

(defun force (x)
  (funcall x))

(defmacro lazy-cons (a b)
  `(lazy (cons ,a ,b)))

(defun lazy-car (x)
  (car (force x)))

(defun lazy-cdr (x)
  (cdr (force x)))

(defun lazy-nil ()
  (lazy nil))

(defun lazy-null (x)
  (not (force x)))

(defun make-lazy (xs)
  (lazy (when xs (cons (car xs) (make-lazy (cdr xs))))))

(defun lazy-list (&rest xs)
  (make-lazy xs))

(defun take (n xs)
  (unless (or (zerop n) (lazy-null xs))
    (cons (lazy-car xs) (take (1- n) (lazy-cdr xs)))))

(defun take-all (xs)
  (unless (lazy-null xs)
    (cons (lazy-car xs) (take-all (lazy-cdr xs)))))

(defun lazy-mapcar (f xs)
  (lazy (unless (lazy-null xs)
          (cons (funcall f (lazy-car xs))
                (lazy-mapcar f (lazy-cdr xs))))))

(defun lazy-mapcan (fun xs)
  (labels ((f (lst-cur)
             (if (lazy-null lst-cur)
               (force (lazy-mapcan fun (lazy-cdr xs)))
               (cons (lazy-car lst-cur)
                     (lazy (f (lazy-cdr lst-cur)))))))
    (lazy (unless (lazy-null xs)
            (f (funcall fun (lazy-car xs)))))))

(defun lazy-find-if (fun xs)
  (unless (lazy-null xs)
    (let ((x (lazy-car xs)))
      (if (funcall fun x) x
        (lazy-find-if fun (lazy-cdr xs))))))

(defun lazy-nth (n xs)
  (if (zerop n)
    (lazy-car xs)
    (lazy-nth (1- n) (lazy-cdr xs)))) 
