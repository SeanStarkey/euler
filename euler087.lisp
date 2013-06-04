;;;;
;;;; Project Euler - Problem 87
;;;;

(load "prime")

(defparameter *end* 50000000)

(defun euler087 ()
  (prime-init 100000)
  (let ((results-hash (make-hash-table)))
    (loop for 2-power from 1 to (floor (expt *end* (/ 1 2))) do
         (loop for 3-power from 1 to (floor (expt *end* (/ 1 3))) do
              (loop for 4-power from 1 to (floor (expt *end* (/ 1 4))) do
                   (let ((sum (sum 2-power 3-power 4-power)))
                     (if (<= sum *end*)
                         (setf (gethash sum results-hash) t))))))
    (hash-table-count results-hash)))

(defun sum (2-power 3-power 4-power)
  (let ((2-power-prime (prime 2-power))
        (3-power-prime (prime 3-power))
        (4-power-prime (prime 4-power)))
    (+ (* 2-power-prime 2-power-prime)
       (* 3-power-prime 3-power-prime 3-power-prime)
       (* 4-power-prime 4-power-prime 4-power-prime 4-power-prime))))
