;;;;
;;;; Project Euler - Problem 15
;;;;

(defconstant *euler015-depth* 20)

(defun euler015 ()
  (/ (factorial (* 2 *euler015-depth*)) (* (factorial *euler015-depth*)
                                           (factorial *euler015-depth*))))
;  (find-path 0 0))

;;;
;;; find-path would have found the problem... *eventually*
;;;
;;; By analyzing the results, one can see the pattern is the central
;;; binomial coefficients C(2n,n) = (2n)!/(n!)^2
;;;
(defun find-path (x y)
  (format t "x:~d y:~d~%" x y)
  (if (and (= x *euler015-depth*)
           (= y *euler015-depth*))
      1
      (+
       (if (< x *euler015-depth*)
           (find-path (1+ x) y)
           0)
       (if (< y *euler015-depth*)
           (find-path x (1+ y))
           0))))

(defun factorial (number)
  (if (zerop number)
      1
      (* number (factorial (- number 1)))))
