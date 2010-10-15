;;;;
;;;; Project Euler - Problem 28
;;;;

(defconstant *euler028-array-size* 1001)
(defparameter *euler028-array* nil)

(defun euler028 ()
  (fill-array)
  (get-sum))

(defun get-sum ()
  (let ((sum 0))
    (do ((x 0 (1+ x)))
        ((= x *euler028-array-size*))
      (setf sum (+ sum (aref *euler028-array* x x)))
      (setf sum (+ sum (aref *euler028-array* x
                             (1- (- *euler028-array-size* x))))))
     (1- sum)))

(defun fill-array ()
  (setf *euler028-array* (make-array (list *euler028-array-size*
                                           *euler028-array-size*)))
  (let ((x (floor *euler028-array-size* 2))
        (y (floor *euler028-array-size* 2))
        (value 1))
    (setf value (set-array x y value))
    (multiple-value-setq (x y value) (move-right x y value))
    (loop
       (multiple-value-setq (x y value) (move-down x y value))
       (multiple-value-setq (x y value) (move-left x y value))
       (multiple-value-setq (x y value) (move-up x y value))
       (multiple-value-setq (x y value) (move-right x y value))
       (if (and (= (1+ x) *euler028-array-size*) (= y 0))
           (return *euler028-array*)))))

(defun set-array (x y value)
  (setf (aref *euler028-array* y x) value)
  (setf value (1+ value)))

(defun move-down (x y value)
  (loop
     (setf y (1+ y))
     (setf value (set-array x y value))
     (if (null (aref *euler028-array* y (1- x)))
         (return (values x y value)))))

(defun move-left (x y value)
  (loop
     (setf x (1- x))
     (setf value (set-array x y value))
     (if (null (aref *euler028-array* (1- y) x))
         (return (values x y value)))))

(defun move-up (x y value)
  (loop
     (setf y (1- y))
     (setf value (set-array x y value))
     (if (null (aref *euler028-array* y (1+ x)))
         (return (values x y value)))))

(defun move-right (x y value)
  (loop
     (if (= (1+ x) *euler028-array-size*)
         (return (values x y value)))
     (setf x (1+ x))
     (setf value (set-array x y value))
     (if (null (aref *euler028-array* (1+ y) x))
         (return (values x y value)))))
