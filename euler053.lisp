;;;;
;;;; Project Euler - Problem 53
;;;;

(defun euler053 ()
  (let ((count 0))
    (do ((n 1 (1+ n)))
        ((> n 100))
      (do ((r 1 (1+ r)))
          ((> r n))
        (if (> (c n r) 1000000)
            (incf count))))
    count))

(defun fact (n)
  (if (eq 0 n)
      1
      (* n (fact (1- n)))))
(defun c (n r)
  (/ (fact n) (* (fact r) (fact (- n r)))))
