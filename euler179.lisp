;;;;
;;;; Project Euler - Problem 179
;;;;
;;;; TOO SLOW

(load "factors.lisp")

(defun euler179 ()
  (factors-init (expt 10 7))
  (do ((x 1 (1+ x)))
      ((= x (expt 10 6)))
    (divisors-non-sorted x)))
