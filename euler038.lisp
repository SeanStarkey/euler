;;;;
;;;; Project Euler - Problem 38
;;;;

(load "pandigital")
(load "permutations")

(defun euler038 ()
  (let ((list-to-test nil)
        (final-answer 0))
    (do ((x 2 (1+ x)))
        ((< 10000 x))
      ;(format t "x=~A~%" x)
      (setf list-to-test nil)
      (do ((y 1 (1+ y)))
          ((<= 9 (length list-to-test)))
        ;(format t "y=~A~%" y)
        (setf list-to-test (append list-to-test (number-to-list (* x y))))
        ;(format t "l=~A~%" list-to-test)
        )
      (if (pandigital-1-9-p list-to-test)
          (setf final-answer list-to-test)))
    (list-to-number final-answer)))
