;;;;
;;;; Project Euler - Problem 23
;;;;

(load "factors.lisp")
(load "memoize.lisp")

(defun euler023 ()
  (factors-init 28123)
  (let ((sum 0)
        (abundant-list (get-abundant-list)))
    (dotimes (x 28123)
      (if (not (sum-of-two-abundants? x abundant-list))
          (setf sum (+ sum x))))
    sum))

(defun sum-of-two-abundants? (number abundant-list)
  (dolist (number-to-check abundant-list)
    (if (> number number-to-check)
        (if (funcall abundantp-memoized (- number number-to-check))
            (return-from sum-of-two-abundants? t))))
  nil)

(defun get-abundant-list ()
    (let ((abundant-list nil))
      (dotimes (x 28123)
        (if (funcall abundantp-memoized x)
            (setf abundant-list (append abundant-list (list x)))))
      abundant-list))


(defvar abundantp-memoized (memoize #'abundantp))

(defun abundantp (number)
  (> (reduce #'+ (divisors number)) number))
