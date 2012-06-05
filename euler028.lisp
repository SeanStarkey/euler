;;;;
;;;; Project Euler - Problem 28
;;;;

(defconstant *euler028-array-size* 1001)

(load "spiral.lisp")

(defun euler028 ()
  (let ((sum 0)
        (spiral (get-spiral *euler028-array-size*)))
    (do ((x 0 (1+ x)))
        ((= x *euler028-array-size*))
      (setf sum (+ sum (aref spiral x x)))
      (setf sum (+ sum (aref spiral x
                             (1- (- *euler028-array-size* x))))))
     (1- sum)))

