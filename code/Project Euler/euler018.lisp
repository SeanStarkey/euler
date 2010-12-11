;;;;
;;;; Project Euler - Problem 18
;;;;

(load "graph.lisp")

(defun euler018 ()
  (setf graph (load-triangle-file "data018.txt"))
  (setf path (shortest-path '(0 0) 'end graph))
  (cost-highest path graph))
