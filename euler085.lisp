;;;;
;;;; Project Euler 85
;;;;

(defparameter *goal* 2000000)

(defun euler085 ()
  (let ((diff-goal *goal*)
        (x-goal 0)
        (y-goal 0)
        (x 1)
        (y 1))
    (loop named outer do
         (let* ((number (numberOfRectangles x y))
                (diff (abs (- number *goal*))))
           (if (and (= x 1) (> number *goal*))
               (return-from outer))
           (if (< diff diff-goal)
               (progn
                 (setf x-goal x)
                 (setf y-goal y)
                 (setf diff-goal diff)))
           (if (> number *goal*)
               (progn
                 (incf y)
                 (setf x 1))
               (incf x))))
    (print (* x-goal y-goal))))

(defun numberOfRectangles (x-size y-size)
  (let ((total 0))
    (loop for x from 1 to x-size do
         (loop for y from 1 to y-size do
              (setf total (+ total (* x y)))))
    total))
