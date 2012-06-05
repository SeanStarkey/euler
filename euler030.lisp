;;;;
;;;; Project Euler - Problem 30
;;;;

(defun euler030 ()
  (let ((sum 0))
    (do ((x 10 (1+ x)))
        ((> x 1000000) sum)
      (if (= (sum-of-5th-power-digits x) x)
          (setf sum (+ sum x))))))


(defun sum-of-5th-power-digits (number)
   (let ((sum 0))
    (loop
         (let ((division (multiple-value-list (floor number 10))))
           (if (zerop (car division))
               (return (+ sum (expt (cadr division) 5)))
               (progn
                 (setf sum (+ sum (expt (cadr division) 5)))
                 (setf number (car division))))))))
