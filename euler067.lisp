;;;;
;;;; Project Euler - Problem 67
;;;;

(load "graph.lisp")

(defun euler067 ()
  (setf graph (load-triangle-file "triangle.txt"))
  (setf path (shortest-path 'start 'end graph))
  (cost-highest path graph))
