;;;;
;;;; Find square root as a continued fraction expansion
;;;;
;;;; https://en.wikipedia.org/wiki/Methods_of_computing_square_roots#Continued_fraction_expansion
;;;;

(defun sqrt-fraction-start (S)
  (let* ((m 0)
         (d 1)
         (a (floor (sqrt S)))
         (a0 a))
    (list m d a a0 S)))

(defun sqrt-fraction-iter (tuple)
  (let ((m (nth 0 tuple))
        (d (nth 1 tuple))
        (a (nth 2 tuple))
        (a0 (nth 3 tuple))
        (S (nth 4 tuple)))
    (if (= a 0)
        (return-from sqrt-fraction-iter nil))
    (setf m (- (* d a) m))
    (setf d (/ (- S (* m m)) d))
    (if (/= d 0)
        (setf a (floor (/ (+ a0 m) d)))
        (setf a 0))
    (list m d a a0 S)))
