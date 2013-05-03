;;;;
;;;; Project Euler - Problem 82
;;;;

(load "graph.lisp")

(defun euler082 ()
  (setf graph (load-matrix-side-file "matrix.txt"))
  (setf path (shortest-path 'start 'end graph))
  (cost path graph))

