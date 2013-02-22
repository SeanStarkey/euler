;;;;
;;;; Problem 71
;;;;

(defparameter *goal* 3/7)
(defparameter *highest* 1000000)

(defun euler071 ()
  (let ((max 0)
        (numer 0))
    (loop for n from 1 to *highest* do
         (let* ((numer-to-test (find-numerator n))
                (fract-to-test (/ numer-to-test n)))
           (if (and (> fract-to-test max)
                    (< fract-to-test *goal*))
               (progn
                 (setf numer numer-to-test)
                 (setf max fract-to-test)))))
   numer))

(defun find-numerator (n)
  (floor (/ *goal* (/ 1 n))))
