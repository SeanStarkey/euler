;;;;
;;;; Project Euler - Problem 14
;;;;

(defun euler014 ()
  (colatz-init)
  (let ((number-with-largest-chain 0)
        (largest-chain 0))
    (do ((iter 1 (+ 2 iter)))
        ((> iter (expt 10 6)) number-with-largest-chain)
      (if (> (colatz iter) largest-chain)
          (progn
            (setf largest-chain (colatz iter))
            (setf number-with-largest-chain iter))))
    number-with-largest-chain))

(defun colatz-init ()
  (defparameter *colatz-hash* (make-hash-table))
  (setf (gethash 1 *colatz-hash*) 1)
  t)

;;;
;;; This function returns the number of terms in the colatz sequence
;;; for the given number.
;;;
;;; Memoization is used for performance.
;;;
(defun colatz (number)
  (let ((hash-value (gethash number *colatz-hash*)))
    (if (not (null hash-value))
        hash-value
        (progn
          (if (oddp number)
              (setf hash-value (colatz (+ (* 3 number) 1)))
              (setf hash-value (colatz (/ number 2))))
          (setf hash-value (1+ hash-value))
          (setf (gethash number *colatz-hash*) hash-value)))))

