;;;;
;;;; Project Euler - Problem 29
;;;;

(defun euler029 ()
  (let ((set nil))
    (do ((a 2 (1+ a)))
        ((> a 100))
      (do ((b 2 (1+ b)))
          ((> b 100))
        (setf set (adjoin (expt a b) set))))
    (length set)))

