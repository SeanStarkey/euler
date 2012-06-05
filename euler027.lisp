;;;;
;;;; Project Euler - Problem 27
;;;;

(load "prime.lisp")

(defun euler027 ()
  (prime-init 100000)
  (let ((final-a 0) (final-b 0) (longest 0))
    (do* ((prime-counter 1 (1+ prime-counter))
          (b (prime prime-counter) (prime prime-counter)))
         ((> b 1000) 'done)
      (loop for a from -1000 to 1000 do
           (let ((n 0))
             (loop
                (if (primep (quadratics a b n))
                    (setf n (1+ n))
                    (progn
                      (if (> n longest)
                          (setf final-a a
                                final-b b
                                longest n))
                      (return)))))))
    (* final-a final-b)))

(defun quadratics (a b n)
  (+ (* n n) (* a n) b))


