;;;;
;;;; Project Euler - Problem 73
;;;;

(defparameter *end* 12000)
(defparameter *left* (/ 1 3))
(defparameter *right* (/ 1 2))

(defun euler073 ()
  (let ((count 0))
    (do ((d 2 (1+ d)))
        ((> d *end*))
      (do* ((n 1 (1+ n))
            (frac (/ n d) (/ n d)))
           ((>= frac *right*))
        (if (> frac *left*)
            (if (= (denominator frac) d)
                (incf count)))))
    count))
