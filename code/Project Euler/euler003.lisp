;;;;
;;;; Project Euler - Problem 3
;;;;
;;;; http://projecteuler.net/index.php?section=problems&id=3
;;;;

(load "factors.lisp")

(defun euler003 ()
  (car (last (factors 600851475143))))
