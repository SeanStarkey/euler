;;;;
;;;; Project Euler - Problem 058
;;;;
;;;; Through research, the formulas for the diagonals are:
;;;;
;;;; where x = 1, 3, 5, etc...
;;;;
;;;;  Diagonal +x, +y = x**2 + 0x + 0
;;;;  Diagonal -x, +y = x**2 - 1x + 1
;;;;  Diagonal -x, -y = x**2 - 2x + 2
;;;;  Diagonal +x, -y = x**2 - 3x + 3
;;;;

;(load "spiral.lisp")
(load "prime.lisp")
(load "memoize.lisp")

(defun euler058 ()
  (prime-init 30000)
  (let ((spiral-size 3)
        (number-of-primes-memoized (memoize #'number-of-primes)))
    (loop
       (if (< (get-ratio spiral-size) 0.1)
           (return-from euler058 spiral-size))
       (setf spiral-size (+ 2 spiral-size)))))

(defun get-ratio (spiral-size)
    (/ (number-of-primes spiral-size) (1- (* 2 spiral-size))))

(defun number-of-primes (spiral-size)
  (if (= spiral-size 1)
      0
      (+ (funcall number-of-primes-memoized (- spiral-size 2))
         ;(if (primep (e 0 0 spiral-size)) 1 0)
         (if (primep (e -1 1 spiral-size)) 1 0)
         (if (primep (e -2 2 spiral-size)) 1 0)
         (if (primep (e -3 3 spiral-size)) 1 0))))

(defun e (b c x)
  (+ (* x x) (* b x) c))

