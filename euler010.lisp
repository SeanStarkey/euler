;;;;
;;;; Project Euler - Problem 10
;;;;
;;;; The sum of the primes below 10 is 2 + 3 + 5 + 7 = 17.
;;;;
;;;; Find the sum of all the primes below two million.
;;;;

(load "prime.lisp")

(defun euler010 ()
  (prime-init 2000100)
  (do ((iteration 1 (1+ iteration))
       (current-prime 0 (prime iteration))
       (sum 0 (+ sum current-prime)))
      ((> current-prime 2000000) sum)))

