;;;;
;;;; Project Euler - Problem 1
;;;;
;;;; If we list all the natural numbers below 10 that are multiples of 3
;;;; or 5, we get 3, 5, 6 and 9. The sum of these multiples is 23.
;;;;
;;;; Find the sum of all the multiples of 3 or 5 below 1000.
;;;;

(defun euler001 ()
  (sum-naturals 1000))

(defun sum-naturals (n)
  (let ((sum 0))
    (do ((i 1 (1+ i)))
        ((= i n) sum)
      (if (or (mult3 i) (mult5 i))
          (setf sum (+ sum i))))))

(defun mult3 (i)
  (= (mod i 3) 0))

(defun mult5 (i)
  (= (mod i 5) 0))
