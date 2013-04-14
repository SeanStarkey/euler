;;;;
;;;; Project Euler 92
;;;;

(load "figurate")

(defvar a (make-array 698))

(defun euler092 ()
  (setf (aref a 1) 1)
  (setf (aref a 89) 89)
  (let ((i 1)
        (total 0))
    (loop
       (if (> i 10000000)
           (return total)
           (if (= (square-of-digits-chain i) 89)
               (incf total)))
       (incf i))))


(defun square-of-digits-chain (n)
  (let ((comp (sum-of-square-of-digits n)))
    (when (null (aref a comp))
          (setf (aref a comp) (square-of-digits-chain comp))
          comp)
    (aref a comp)))

(defun sum-of-square-of-digits (number)
  (let ((sum 0))
    (loop
         (let ((division (multiple-value-list (floor number 10))))
           (if (zerop (car division))
               (return (+ sum (square (cadr division))))
               (progn
                 (setf sum (+ sum (square (cadr division))))
                 (setf number (car division))))))))
