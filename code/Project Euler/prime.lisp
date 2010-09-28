;;;;
;;;; Perform prime sieve of Eratosthenes
;;;;

(defparameter *prime-limit* 2500000)
(defparameter *prime-sieve* (make-array *prime-limit* :initial-element 'prime))
(defparameter *sieve-done* nil)
(setf (svref *prime-sieve* 0) 'not-prime)
(setf (svref *prime-sieve* 1) 'not-prime)

;;;
;;; Return the nth prime number
;;;
(defun prime (nth)
  (if (not *sieve-done*)
      (perform-sieve))
  (do ((iteration 0 iteration)
       (number-checked 2 (1+ number-checked)))
      ((>= iteration nth) (1- number-checked))
    (if (eql (svref *prime-sieve* number-checked) 'prime)
        (setf iteration (1+ iteration)))))

;;;
;;; Predicate to determine if a number is prime
;;;
(defun primep (number)
  (if (not *sieve-done*)
      (perform-sieve))
  (eql (svref *prime-sieve* number) 'prime))

;;;
;;; Performs the sieve
;;; This should be run only once
;;;
(defun perform-sieve ()
  (setf *sieve-done* T)
  (setf (svref *prime-sieve* 0) 'not-prime)
  (setf (svref *prime-sieve* 1) 'not-prime)
  (perform-sieve-on-number 2)
  (let ((current-prime 2)
        (number-checked 2))
    (do ((number-checked 2 (1+ number-checked)))
        ((>= (* number-checked number-checked) *prime-limit*))
      (if (eql (svref *prime-sieve* number-checked) 'prime)
          (progn
            (setf current-prime number-checked)
            (if (< (* current-prime current-prime) *prime-limit*)
                (perform-sieve-on-number current-prime)))))))

;;;
;;; Crosses out one particular prime from the sieve
;;;
(defun perform-sieve-on-number (p)
  (do ((i (+ p p) (+ i p)))
       ((>= i *prime-limit*))
    (setf (svref *prime-sieve* i) 'not-prime)))
