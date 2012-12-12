;;;;
;;;; Project Euler - Problem 76
;;;;

(load "prime")
(load "memoize")
(load "factors")

(defun euler076 (n)
  (let ((total 1))
    (do ((i 2 (1+ i)))
        ((> i n))
      (let ((compute (a i 2)))
        (setf total (+ total compute))))
    (1- total))
  )


(defparameter *addon-memoize* (make-hash-table :test #'equal))

;;
(defun addon (num startdiv)
  (let ((hash-value (+ (* 1009 num) startdiv)))
    (multiple-value-bind
          (result exists)
        (gethash hash-value *addon-memoize*)
      (if exists
          result
          (setf (gethash hash-value *addon-memoize*)
                (a num startdiv))))))

(defun a (num startdiv)
  (let ((total 0))
    (do ((div startdiv (1+ div)))
        ((> div num))
      (if (>= num div)
          (if (= num div)
              (incf total)
              (if (>= (- num div) div)
                  (setf total (+ total (addon (- num div) div)))))))
    total))


;;
;; This would have worked, but took too much memory and too long to compute
;;
(defun sums (n)
  (format t "sums=~A~%" n)
  (if (= n 1)
      (list (prime 1))
      (let ((acc (list (prime n))))
        (do ((m (1- n) (1- m)))
            ((< m 1))
;          (format t "acc=~A m=~A~%" acc m)
;          (format t "n=~A s=~A~%" n (mapcar (lambda (x) (* (prime m) x)) (sums (- n m))))
          (setf acc (union acc (mapcar (lambda (x) (* (prime m) x)) (funcall sums-memoized (- n m)))))
          )
        acc)))

(defvar sums-memoized (memoize #'sums))
