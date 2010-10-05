;;;;
;;;; Compute the fibonacci series
;;;;
;;;; 1, 1, 2, 3, 5, 8, 13, 21, 34, 55, 89...
;;;;
;;;; Store previously computed values in an array to speed computation.
;;;;

;;;
;;; Performs a "quick" fibonacci computation
;;;
;;; Returns the nth term in the fibonacci series
;;;
(defun fibonacci (n)
  (if (not (null (svref *fibonacci-values* n)))
      (svref *fibonacci-values* n)
      (do ((iter (1+ *fibonacci-largest-computed*) (1+ iter)))
          ((> iter n) (svref *fibonacci-values* n))
        (setf (svref *fibonacci-values* iter)
              (+ (svref *fibonacci-values* (- iter 1))
                 (svref *fibonacci-values* (- iter 2))))
        (setf *fibonacci-largest-computed* iter))))

;;;
;;; Initalize the fibonacci engine
;;;
(defun fibonacci-init (n)
  (defparameter *fibonacci-values* (make-array (1+ n) :initial-element nil))
  (setf (svref *fibonacci-values* 1) 1)
  (setf (svref *fibonacci-values* 2) 1)
  (defparameter *fibonacci-largest-computed* 2)
  t)
