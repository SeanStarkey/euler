;;;;
;;;; Project Euler - Problem 50
;;;;

(load "prime.lisp")

(defparameter euler050-limit 1000000)

(defun euler050 ()
  (prime-init (+ 100 euler050-limit))
  (let ((highest-prime-computed 0)
        (longest-sequence 0))
    (do ((prime-start 1 (1+ prime-start)))
        ((> (prime prime-start) euler050-limit) highest-prime-computed)
      (let ((sum 0))
        (do ((prime-iter prime-start (1+ prime-iter)))
            ((> sum euler050-limit))
          (setf sum (+ sum (prime prime-iter)))
          (if (and (< sum euler050-limit)
                   (> (- prime-iter prime-start) longest-sequence)
                   (primep sum))
              (progn
                (setf highest-prime-computed sum)
                (setf longest-sequence (- prime-iter prime-start)))))))))

