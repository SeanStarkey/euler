;;;;
;;;; Project Euler - Problem 12
;;;;

(load "factors.lisp")

(defun euler012 ()
  (factors-init 100000000)
  (let ((iter 1)
        (triangle 1))
    (loop
       (setf iter (1+ iter))
       (setf triangle (+ iter triangle))
       (setf triangle-factors (factors triangle))
       (let ((number-divisors 1))
         (dolist (next-factor triangle-factors)
           (setf number-divisors (* number-divisors (1+ (cadr next-factor)))))
           (if (> number-divisors 500)
               (return triangle))))))
