;;;;
;;;; Project Euler - Problem 97
;;;;
;;;; http://projecteuler.net/index.php?section=problems&id=97
;;;;

(defun euler097 ()
  (let ((answer 0))
    (setf answer 28433)
    (do ((n 1 (1+ n)))
        ((> n 7830457) (mod (1+ answer) (expt 10 10)))
      (setf answer (* 2 answer))
      (if (zerop (mod n 1000))
          (setf answer (mod answer (expt 10 10)))))))
