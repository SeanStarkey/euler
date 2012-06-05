;;;;
;;;; Project Euler - Problem 37
;;;;

(load "prime.lisp")

(defun euler037 ()
  (prime-init 1000000)
  (let ((iter 5)
        (number-found 0)
        (sum 0))
    (loop
       (when (= number-found 11)
         (return sum))
       (let ((prime-to-check (prime iter)))
         (if (and (check-left prime-to-check)
                  (check-right prime-to-check))
             (progn
               (setf sum (+ sum prime-to-check))
               (incf number-found)))
         (incf iter)))))

(defun check-left (n)
  (if (null n)
      t
      (if (primep n)
          (check-left (truncate-left n))
          nil)))

(defun truncate-left (n)
  (if (< n 10)
      nil
      (let* ((log (log10 n))
             (floor-value (floor n log))
             (left (- n (* floor-value log))))
        left)))

(defun check-right (n)
  (if (null n)
      t
      (if (primep n)
          (check-right (truncate-right n))
          nil)))

(defun truncate-right (n)
  (if (< n 10)
      nil
      (floor n 10)))

(defun log10 (n)
  (cond
    ((> n 10000000) 10000000)
    ((> n 1000000) 1000000)
    ((> n 100000) 100000)
    ((> n 10000) 10000)
    ((> n 1000) 1000)
    ((> n 100) 100)
    ((> n 10) 10)
    (t 1)))
