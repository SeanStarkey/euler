;;;;
;;;; Project Euler - Problem 70
;;;;

(load "totient")
(load "permutations")

(defun euler070 ()
  (proclaim '(optimize (speed 3) (space 0) (debug 0)))
  (factors-init 20000000)

  (let ((min 1000000)
        (min-n 0))
    (loop for n from 2 to 10000000 do
         (let ((p (phi n)))
           (if (permutation? (number-to-list n) (number-to-list p))
               (let ((ratio (* 1.0 (/ n p))))
                 (if (< ratio min)
                     (progn
                       (setf min ratio)
                       (setf min-n n)))))))
    min-n))

