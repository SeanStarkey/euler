;;;;
;;;; Project Euler - Problem 48
;;;;
;;;; http://projecteuler.net/index.php?section=problems&id=48
;;;;

(defun euler048 ()
  (let ((sum 0))
    (do ((iterator 1 (1+ iterator)))
        ((> iterator 1000) (mod sum 10000000000))
      (setf sum (+ sum (expt iterator iterator))))))
