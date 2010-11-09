;;;;
;;;; Project Euler - Problem 34
;;;;

(load "memoize.lisp")

(defun euler034 ()
  (let ((sum 0))
    (do ((x 10 (1+ x)))
        ((> x 370000) sum)
      (if (= (sum-of-factorial-digits x) x)
          (setf sum (+ sum x))))))



(defun sum-of-factorial-digits (number)
  (let ((sum 0))
    (loop
         (let ((division (multiple-value-list (floor number 10))))
           (if (zerop (car division))
               (return (+ sum (funcall factorial-memoized (cadr division))))
               (progn
                 (setf sum (+ sum (funcall factorial-memoized (cadr division))))
                 (setf number (car division))))))))

(defvar factorial-memoized (memoize #'factorial))

(defun factorial (number)
  (if (= 0 number)
      1
      (* number (funcall factorial-memoized (1- number)))))
