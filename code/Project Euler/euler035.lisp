;;;;
;;;; Project Euler - Problem 35
;;;;

(load "prime.lisp")

(defun euler035 ()
  (prime-init 1001000)
  (let ((iter 1)
        (count 0))
    (loop
       (let ((prime-to-check (prime iter)))
         (if (> prime-to-check 1000000)
             (return-from euler035 count))
         (if (all-prime? (rotate-number prime-to-check))
             (setf count (1+ count))))
       (setf iter (1+ iter)))))

(defun all-prime? (lst)
  (dolist (number lst)
    (if (not (primep number))
        (return-from all-prime? nil)))
  t)

(defun rotate-number (number)
  (let ((number-size (floor (log number 10)))
        (return-list nil))
    (dotimes (iter (1+ number-size))
      (setf return-list (append return-list (list number)))
      (multiple-value-bind (quotient remainder) (floor number 10)
        (setf number (+ quotient (* remainder (expt 10 number-size))))))
    return-list))


