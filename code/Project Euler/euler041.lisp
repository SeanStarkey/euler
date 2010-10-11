;;;;
;;;; Project Euler - Problem 41
;;;;

(load "permutations.lisp")
(load "prime.lisp")

(defun euler041 ()
  (prime-init 3000)
  (dolist (number (permutations-number 7654321))
          (if (primep number)
              (return number))))
