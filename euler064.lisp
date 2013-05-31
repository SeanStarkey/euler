;;;;
;;;; Project Euler - Problem 64
;;;;

;;;
;;; Algorithm for determining repeats found in wikipedia.
;;; https://en.wikipedia.org/wiki/Methods_of_computing_square_roots#Continued_fraction_expansion
;;;

(defun euler064 ()
  (let ((total 0))
    (loop for n from 1 to 10000 do
         (if (oddp (determine-repeat n))
             (incf total)))
    total))

(defun determine-repeat (S)
  (let* (;(sequence (make-array 0 :fill-pointer 0 :adjustable t))
         (tuples (make-array 0 :fill-pointer 0 :adjustable t))
         (m 0)
         (d 1)
         (a (floor (sqrt S)))
         (a0 a))
    (loop do
         (let* ((tuple (list m d))
                (pos (position tuple tuples :test 'equal)))
           (if (not (null pos))
               (return-from determine-repeat (- (length tuples) pos)))
           ;(vector-push-extend a sequence)
           (vector-push-extend tuple tuples)
           ;(format t "m=~A d=~A a=~A~%" m d a)
           (if (= a 0)
               (return-from determine-repeat 0))
           (setf m (- (* d a) m))
           (setf d (/ (- S (* m m)) d))
           (if (/= d 0)
               (setf a (floor (/ (+ a0 m) d)))
               (setf a 0))))))
