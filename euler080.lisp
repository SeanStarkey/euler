;;;;
;;;; Project Euler - Problem 80
;;;;

(defun euler080 ()
  (let ((sum 0))
    (loop for n from 1 to 100 do
         (if (/= (floor (sqrt n)) (sqrt n))
             (setf sum (+ sum (reduce '+ (sqrt-digits n))))))
    sum))

;;;
;;; SQRT digit algorithm found on wikipedia
;;;
(defun find-x (c p)
  (loop for x from 1 to 9 do
       (if (> (* x (+ (* 20 p) x)) c)
           (return-from find-x (1- x))))
  9)

(defun sqrt-digits (n)
  (let ((ret nil)
        (c n)
        (p 0))
    (loop for digits from 0 to 99 do
         (let* ((x (find-x c p))
                (y (* x (+ (* 20 p) x))))
           (push x ret)
           ;(format t "c=~A p=~A x=~A y=~A~%" c p x y)
           (setf p (+ (* p 10) x))
           (setf c (* 100 (- c y)))))
    (reverse ret)))
