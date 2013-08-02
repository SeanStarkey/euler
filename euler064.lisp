;;;;
;;;; Project Euler - Problem 64
;;;;

;;;
;;; Algorithm for determining repeats found in wikipedia.
;;; https://en.wikipedia.org/wiki/Methods_of_computing_square_roots#Continued_fraction_expansion
;;;

(load "sqrt-fraction")

(defun euler064 ()
  (let ((total 0))
    (loop for n from 1 to 10000 do
         (if (oddp (determine-repeat n))
             (incf total)))
    total))

(defun determine-repeat (S)
  (let ((current-tuple (sqrt-fraction-start S))
        (tuples (make-array 0 :fill-pointer 0 :adjustable t)))
    (vector-push-extend (list (nth 0 current-tuple) (nth 1 current-tuple)) tuples)
    (loop do
         (setf current-tuple (sqrt-fraction-iter current-tuple))
         (let ((pos (position (list (nth 0 current-tuple) (nth 1 current-tuple)) tuples :test 'equal)))
           (if (not (null pos))
               (return-from determine-repeat (- (length tuples) pos)))
           (if (= 0 (nth 2 current-tuple))
               (return-from determine-repeat 0)))
         (vector-push-extend (list (nth 0 current-tuple) (nth 1 current-tuple)) tuples))))
