;;;;
;;;; Creates a spiral
;;;;
;;;; For example, this a spiral of size 5:
;;;; 21 22 23 24 25
;;;; 20  7  8  9 10
;;;; 19  6  1  2 11
;;;; 18  5  4  3 12
;;;; 17 16 15 14 13
;;;;

(defun get-spiral (size)
  (let ((spiral (make-array (list size size)))
        (x (floor size 2))
        (y (floor size 2))
        (value 1))
    (multiple-value-setq (spiral value) (set-array spiral x y value))
    (multiple-value-setq (spiral x y value)
      (move-right spiral size x y value))
    (loop
       (multiple-value-setq (spiral x y value) (move-down spiral x y value))
       (multiple-value-setq (spiral x y value) (move-left spiral x y value))
       (multiple-value-setq (spiral x y value) (move-up spiral x y value))
       (multiple-value-setq (spiral x y value)
         (move-right spiral size x y value))
       (if (and (= (1+ x) size) (= y 0))
           (return spiral)))))

(defun set-array (spiral x y value)
  (setf (aref spiral y x) value)
  (setf value (1+ value))
  (values spiral value))

(defun move-down (spiral x y value)
  (loop
     (setf y (1+ y))
     (multiple-value-setq (spiral value) (set-array spiral x y value))
     (if (null (aref spiral y (1- x)))
         (return (values spiral x y value)))))

(defun move-left (spiral x y value)
  (loop
     (setf x (1- x))
     (multiple-value-setq (spiral value) (set-array spiral x y value))
     (if (null (aref spiral (1- y) x))
         (return (values spiral x y value)))))

(defun move-up (spiral x y value)
  (loop
     (setf y (1- y))
     (multiple-value-setq (spiral value) (set-array spiral x y value))
     (if (null (aref spiral y (1+ x)))
         (return (values spiral x y value)))))

(defun move-right (spiral size x y value)
  (loop
     (if (= (1+ x) size)
         (return (values spiral x y value)))
     (setf x (1+ x))
     (multiple-value-setq (spiral value) (set-array spiral x y value))
     (if (null (aref spiral (1+ y) x))
         (return (values spiral x y value)))))
