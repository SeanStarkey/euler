;;;;
;;;; Problem 46
;;;;

(load "prime.lisp")

(defun euler046 ()
  (prime-init 100000)
  (do ((n 3 (+ 2 n)))
      ((>= n 10000) n nil)
    (unless (primep n)
      (block unless
        (do* ((s 1 (1+ s))
              (double-square-s (double-square s) (double-square s)))
             ((> double-square-s n))
          (let ((diff (- n double-square-s)))
            (if (primep diff)
                (progn
;                  (format t "~a ~a~%" n diff)
                  (return-from unless)))))
            (return n)))
      ))

(defun double-square (n)
  (* 2 (* n n)))
