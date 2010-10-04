;;;;
;;;; Find the prime factors of any number
;;;;

(load "prime.lisp")

;;;
;;; Return the prime factors as a list
;;;
(defun factors (number)
  (prime-init (1+ number))
  (factors-recursive number 1))

;;;
;;; Recursively determine the next factor
;;;
(defun factors-recursive (number prime-iteration)
  (if (primep number)
      (list number)
      (loop
         (let ((prime-to-check (prime prime-iteration)))
           (if (eql (mod number prime-to-check) 0)
               (return (cons prime-to-check
                             (factors-recursive (/ number prime-to-check)
                                                prime-iteration)))
               (setq prime-iteration (1+ prime-iteration)))))))

