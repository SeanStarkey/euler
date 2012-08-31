;;;;
;;;; Problem 57
;;;;

(defun euler057 ()
  (let ((total 0))
    (do ((n 1 (1+ n)))
        ((> n 1000) total)
      (let ((ratio (+ 1 (expand n))))
        (if (> (number-of-digits (numerator ratio)) (number-of-digits (denominator ratio)))
            (incf total))))))

(defun expand (n)
  (if (= n 1)
      (/ 1 2)
      (/ 1 (+ 2 (expand (- n 1))))))

(defun number-of-digits (n)
  (length (write-to-string n)))
