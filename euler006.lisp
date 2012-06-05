;;;;
;;;; Project Euler - Problem 6
;;;;
;;;; http://projecteuler.net/index.php?section=problems&id=6
;;;;

(defun euler006 ()
  (- (square-of-sum 100) (sum-of-squares 100)))

(defun sum-of-squares (number)
  (let ((sum 0))
    (do ((iter 1 (1+ iter)))
        ((> iter number) sum)
      (setf sum (+ sum (* iter iter))))))

(defun square-of-sum (number)
  (let ((sum 0))
    (do ((iter 1 (1+ iter)))
        ((> iter number) (* sum sum))
      (setf sum (+ sum iter)))))
