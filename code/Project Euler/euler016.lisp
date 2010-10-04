;;;;
;;;; Project Euler - Problem 16
;;;;
;;;; http://projecteuler.net/index.php?section=problems&id=16
;;;;

(load "sum-of-digits.lisp")

(defun euler016 ()
  (sum-of-digits (expt 2 1000)))

