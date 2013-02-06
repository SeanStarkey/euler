;;;;
;;;; Perform prime sieve of Eratosthenes
;;;;

(defparameter *prime-sieve* nil)
(defparameter *prime-array* (make-array 1 :initial-element nil))
(defparameter *prime-largest* 0)

;;;
;;; initialize the prime engine
;;;
(defun prime-init (prime-limit)
  (prime-init-init prime-limit)
  (prime-init-sieve prime-limit)
  (prime-init-load-array prime-limit)
  T)

;;;
;;; Setup variables for prime engine
;;;
(defun prime-init-init (prime-limit)
  (setf *prime-sieve* (make-array prime-limit :initial-element t))
  (setf (svref *prime-sieve* 0) nil)
  (setf (svref *prime-sieve* 1) nil)
  (perform-sieve-on-number 2 prime-limit)
)

;;;
;;; Return the nth prime number
;;;
(defun prime (nth)
  (svref *prime-array* nth))

;;;
;;; Predicate to determine if a number is prime
;;;
(defun primep (number)
  (if (< number 1)
      nil
      (if (< number *prime-largest*)
          (svref *prime-sieve* number)
          (if (< *prime-largest* (isqrt number))
              (error (format nil "Number too big: ~d~%" number))
              (let ((end-value (isqrt number))
                    (prime-iterator 1))
                (loop
                   (if (> (prime prime-iterator) end-value)
                       (return t)
                       (if (zerop (mod number (prime prime-iterator)))
                           (return nil)
                           (setf prime-iterator (1+ prime-iterator))))))))))

;;;
;;; Do initial sieve
;;;
(defun prime-init-sieve (prime-limit)
  (let ((current-prime 2)
        (number-checked 2))
    (do ((number-checked 2 (1+ number-checked)))
        ((>= (* number-checked number-checked) prime-limit))
      (if (svref *prime-sieve* number-checked)
          (progn
            (setf current-prime number-checked)
            (if (< (* current-prime current-prime) prime-limit)
                (perform-sieve-on-number current-prime prime-limit)))))))

;;;
;;; Load the array with the primes
;;;
(defun prime-init-load-array (prime-limit)
  (setf *prime-array* (adjust-array *prime-array* 100000000))
  (do ((number-checked 2 (1+ number-checked))
       (index 0 index))
      ((>= number-checked prime-limit)
       (setf *prime-array* (adjust-array *prime-array* (1+ index))))
    (if (svref *prime-sieve* number-checked)
        (progn
          (setf index (1+ index))
          (setf (svref *prime-array* index) number-checked)
          (setf *prime-largest* number-checked)))))

;;;
;;; Crosses out one particular prime from the sieve
;;;
(defun perform-sieve-on-number (p prime-limit)
  (do ((i (+ p p) (+ i p)))
       ((>= i prime-limit))
    (setf (svref *prime-sieve* i) nil)))
