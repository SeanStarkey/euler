;;;;
;;;; Project Euler - Problem 25
;;;;

(load "fibonacci.lisp")

(defun euler025 ()
  (fibonacci-init 10000)
  (let ((iter 1)
        (finished-value (expt 10 999)))
    (loop
       (let ((computed-value (fibonacci iter)))
         (if (> computed-value finished-value)
             (return iter)
             (setq iter (1+ iter)))))))
