;;;;
;;;; Problem 187
;;;;

(load "prime")

(defparameter *end* (expt 10 8))

(defun euler187 ()
  (prime-init (* 2 *end*))
  (let ((count 0))
    (block end
    (do ((x 1 (1+ x)))
        ((>= (prime x) *end*) count)
      (do ((y x (1+ y)))
          ((>= (* (prime x) (prime y)) *end*))
        ;(format t "~a ~a ~a ~a ~a~%" x y (prime x) (prime y) (* (prime x) (prime y)))
        (incf count))))))
