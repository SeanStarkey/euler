;;;;
;;;; Project Euler - Problem 47
;;;;

(load "factors.lisp")

(defun euler047 ()
  (factors-init 1000000)
  (dotimes (number 1000000 nil)
    (if (and (eql 4 (length (factors number)))
             (eql 4 (length (factors (+ number 1))))
             (eql 4 (length (factors (+ number 2))))
             (eql 4 (length (factors (+ number 3)))))
        (return-from euler047 number))))

