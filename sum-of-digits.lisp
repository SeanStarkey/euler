;;;;
;;;; Computes the sum of the digits of any number
;;;;
(defun sum-of-digits (number)
  (let ((sum 0))
    (loop
         (let ((division (multiple-value-list (floor number 10))))
           (if (zerop (car division))
               (return (+ sum (cadr division)))
               (progn
                 (setf sum (+ sum (cadr division)))
                 (setf number (car division))))))))
