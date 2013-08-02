;;;;
;;;; Project Euler - Problem 66
;;;;

(load "sqrt-fraction")

(defun euler066 ()
  (let ((max 0)
        (maxd 0))
  (loop for d from 2 to 1000 do
       (if (not (is-square d))
           (let ((x (find-x d)))
             (if (> x max)
                 (progn
                   (setf max x)
                   (setf maxd d))))))
  maxd))

(defun find-x (d)
    (let ((components (make-array 0 :fill-pointer 0 :adjustable t))
          (tu (sqrt-fraction-start d)))
      (loop do
           (vector-push-extend (nth 2 tu) components)
           (let* ((f (get-fraction components))
                  (y (is-y-integer d (numerator f))))
             (if (not (null y))
                 (return-from find-x (numerator f)))
             (setf tu (sqrt-fraction-iter tu))))))

(defun get-fraction (v)
  (let ((f 0))
    (loop for x from (1- (length v)) downto 1 do
         (setf f (/ 1 (+ f (elt v x)))))
    (+ f (elt v 0))))

(defun is-square (S)
  (let ((f (isqrt S)))
    (= (* f f) S)))

(defun is-y-integer (d x)
  (let ((ysquared (/ (- (* x x) 1) d)))
    (if (and (integerp ysquared)
             (is-square ysquared))
        (isqrt ysquared)
        nil)))
