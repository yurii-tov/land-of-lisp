; ---------------------------------
; convert expressions to 'dot' data
; ---------------------------------

(defparameter *max-label-length* 30)

(defun dot-name (exp)
  (substitute-if #\_ 
  	(complement #'alphanumericp) 
  	(prin1-to-string exp)))

(defun dot-label (exp)
  (let* ((prettify
          (lambda (s)
            (if (> (length s) *max-label-length*)
              (string-concat
                (subseq s 0 (- *max-label-length* 3))
                "...")
              s)))
         (label
  	      (if exp
  	        (funcall prettify (write-to-string exp :pretty nil))
  	        ""))) 
  	(string-concat "[label=\"" label "\"];")))

(defun nodes->dot (nodes)
	(reduce
    (lambda (s node)
  	  (format nil
  	    "~A~A~A~%"
  	    s
  	    (dot-name (car node))
  	    (dot-label node)))
		nodes
		:initial-value ""))

; ---------------
; create dot file
; ---------------

(defun dot->png (fname thunk)
  (with-open-file
    (*standard-output* 
     fname
     :direction :output
     :if-exists :supersede)
    (funcall thunk))
  (ext:shell (string-concat "C:/Users/Tovarnov_Y/bin/graphviz/bin/dot.exe -Tpng -O " fname)))

; --------------
; directed graph
; --------------

(defun edges->dot (edges)
	(reduce
    (lambda (ns node)
    	(format nil "~A~A" ns
    		(reduce 
          (lambda (es edge)
		        (format nil
		        	"~A~A->~A~A~%" es
		        	(dot-name (car node))
		        	(dot-name (car edge))
		        	(dot-label (cdr edge))))
    		  (cdr node)
    		  :initial-value "")))
		edges
		:initial-value ""))

(defun graph->dot (nodes edges)
  (format t
  	"digraph{~%~A~A}"
  	(nodes->dot nodes)
    (edges->dot edges)))

(defun graph->png 
  (fname nodes edges)
  (dot->png fname
    (lambda () (graph->dot nodes edges))))

; ----------------
; undirected graph
; ----------------

(defun uedges->dot (graph)
  (apply #'string-concat
    (maplist
      (lambda (lst)
        (reduce 
          (lambda (dot-edges edge)
            (if (assoc (car edge) (cdr lst)) 
              dot-edges
              (format nil
                "~A~A--~A~A~%" 
                dot-edges
                (dot-name (caar lst))
                (dot-name (car edge))
                (dot-label (cdr edge)))))
          (cdar lst)
          :initial-value ""))
      graph)))

(defun ugraph->dot (nodes edges)
  (format t
    "graph{~%~A~A}"
    (nodes->dot nodes)
    (uedges->dot edges)))

(defun ugraph->png (fname nodes edges)
  (dot->png fname
    (lambda () (ugraph->dot nodes edges))))