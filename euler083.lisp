;;;;
;;;; Project Euler - Problem 83
;;;;

(load "graph.lisp")

(defun euler083 ()
  (setf graph (load-matrix-full-file "matrix.txt"))
  (setf path (shortest-path 'start 'end graph))
  (cost path graph))

