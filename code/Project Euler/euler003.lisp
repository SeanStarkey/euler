;;;;
;;;; Project Euler - Problem 3
;;;;
;;;; http://projecteuler.net/index.php?section=problems&id=3
;;;;

(load "factors.lisp")

(defun euler003 ()
  (factors-init 600851475143)
  (caar (last (factors 600851475143))))
