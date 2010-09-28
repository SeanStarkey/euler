;;;;
;;;; Project Euler - Problem 2
;;;;
;;;; Each new term in the Fibonacci sequence is generated by adding the
;;;; previous two terms. By starting with 1 and 2, the first 10 terms will
;;;; be:
;;;;
;;;; 1, 2, 3, 5, 8, 13, 21, 34, 55, 89, ...
;;;;
;;;; Find the sum of all the even-valued terms in the sequence which do
;;;; not exceed four million.
;;;;

;;
;; Store the fibonacci values in a vector as we compute them.
;;
(defparameter *fibonacci-values* (make-array 100 :initial-element nil))
(setf (svref *fibonacci-values* 1) 1)
(setf (svref *fibonacci-values* 2) 2)

(defun euler002 ()
  (let ((lastfib 0)
        (sum 0))
    (do ((i 1 (1+ i)))
        ((> lastfib 4000000) sum)
      (setf lastfib (fibonacci i))
      (if (evenp lastfib)
          (setf sum (+ sum lastfib))))))

;;;
;;; Performs a "quick" fibonacci computation
;;;
(defun fibonacci (n)
  (if (not (null (svref *fibonacci-values* n)))
      (svref *fibonacci-values* n)
      (let ((new-fib
             (+ (fibonacci (- n 1)) (fibonacci (- n 2)))))
        (setf (svref *fibonacci-values* n) new-fib)
        new-fib)))


