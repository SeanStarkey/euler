;;;;
;;;; Project Euler - Problem 4
;;;;
;;;; A palindromic number reads the same both ways. The largest
;;;; palindrome made from the product of two 2-digit numbers is 9009 =
;;;; 91 x 99.
;;;;
;;;; Find the largest palindrome made from the product of two 3-digit
;;;; numbers.
;;;;

(load "palindrome.lisp")

(defun euler004 ()
  (let ((largest-product 0))
    (do ((x 100 (1+ x)))
        ((= x 1000) largest-product)
      (do ((y 100 (1+ y)))
          ((= y 1000))
        (let ((current-product (* x y)))
          (if (palindromep current-product)
              (if (> current-product largest-product)
                  (setf largest-product current-product))))))))
