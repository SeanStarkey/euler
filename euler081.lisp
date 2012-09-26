;;;;
;;;; Project Euler - Problem 81
;;;;

(load "graph.lisp")

(defun euler081 ()
  (setf graph (load-matrix-file "matrix.txt"))
  (setf path (shortest-path 'start 'end graph))
  (cost path graph))

