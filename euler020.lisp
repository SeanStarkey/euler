;;;;
;;;; Project Euler - Problem 5
;;;;
;;;; n! means n  (n  1)  ...  3  2  1
;;;;
;;;; Find the sum of the digits in the number 100!
;;;;

(load "sum-of-digits.lisp")

(defun euler020 ()
  (sum-of-digits (fact 100)))


;;;
;;; Compute the factorial
;;;
(defun fact (number)
  (if (eql number 1)
      '1
  (* number (fact (1- number)))))
