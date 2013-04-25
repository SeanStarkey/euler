;;;;
;;;; Project Euler - Problem 72
;;;;

(load "totient")

(defun euler072 ()
  (factors-init 1000000)
  (let ((total 0))
    (loop for d from 2 to 1000000 do
         (setf total (+ total (phi d))))
    total))
