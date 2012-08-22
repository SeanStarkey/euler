;;;;
;;;; Project Euler - Problem 179
;;;;
;;;; TOO SLOW

(load "factors.lisp")

(defparameter *end* (expt 10 5))

(defun euler179 ()
  (factors-init *end*)
  (do ((x 1 (1+ x)))
      ((>= x *end*))
    ;(print x)
    (if (not (primep *end*))
        (divisors-non-sorted x))))
