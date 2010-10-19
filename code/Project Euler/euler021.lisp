;;;;
;;;; Project Euler - Problem 21
;;;;

(load "factors.lisp")

(defun euler021 ()
  (factors-init 10000)
  (let ((sum 0))
    (dotimes (number 10000)
      (if (/= number 0)
          (if (amicablep number)
              (setf sum (+ number sum)))))
    sum))

(defun amicablep (number)
  (let* ((sum1 (sum-of-divisors (divisors number)))
         (sum2 (sum-of-divisors (divisors sum1))))
    (and (/= sum1 sum2)
         (= number sum2))))

(defun sum-of-divisors (lst)
  (reduce #'+ lst))
