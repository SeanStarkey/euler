;;;;
;;;; Project Euler - Problem 56
;;;;

(load "sum-of-digits.lisp")

(defun euler056 ()
  (let ((sum 0))
    (do ((x 0 (1+ x)))
        ((> x 100))
      (do ((y 0 (1+ y)))
          ((> y 100))
        (let* ((val (expt x y))
               (dig (sum-of-digits val)))
          (if (> dig sum)
              (setf sum dig)))))
    sum))

