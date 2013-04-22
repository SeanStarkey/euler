;;;;
;;;; Problem 69
;;;;

(load "factors")
(load "totient");

(defun euler069 ()
  (factors-init 1000000)
  (let ((max 0)
        (n 0))
    (do ((i 1 (1+ i)))
        ((> i 1000000))
      (let ((ratio (/ i (phi i))))
        (if (> ratio max)
            (progn
              (setf max ratio)
              (setf n i)))))
    n))
