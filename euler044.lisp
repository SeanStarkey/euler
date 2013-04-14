;;;;
;;;; Project euler problem 44
;;;;

(load "figurate")

(defparameter *size* 10000)
(defparameter *pentagonal-hash* (make-hash-table))

(defun euler044 ()
  (let ((pentagonal-array (init-pentagonal-array *size*)))

    (do ((x 1 (1+ x)))
        ((> x *size*))
      (do ((y 1 (1+ y)))
          ((>= y x))
        (let* ((pent-x (pentagonal x))
               (pent-y (pentagonal y))
               (sum (+ pent-x pent-y))
               (diff (- pent-x pent-y)))
;          (if (and (member-array sum pentagonal-array)
;                   (member-array diff pentagonal-array))
          (if (and (pentagonalp sum)
                   (pentagonalp diff))
              (return-from euler044 diff)))))))

(defun init-pentagonal-array (size)
  (let ((return-array (make-array (1+ size))))
    (do ((index 1 (1+ index)))
        ((> index size) return-array)
      (let ((pent (pentagonal index)))
        (setf (gethash pent *pentagonal-hash*) t)
        (setf (aref return-array index) pent)))))

(defun pentagonalp (n)
  (nth-value 0 (gethash n *pentagonal-hash*)))

(defun member-array (n array)
  (do ((index 1 (1+ index)))
      (nil)
      (if (= (aref array index) n)
          (return t)
          (if (> (aref array index) n)
              (return nil)))))
