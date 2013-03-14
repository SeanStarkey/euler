;;;;
;;;; Project Euler - Problem 62
;;;;

(load "permutations")

(defparameter *cube-hash* (make-hash-table :test #'equal))
(defparameter *cube-list* nil)
(defparameter *limit* 5)
(defparameter *end* 10000)

(defun euler062 ()
  (let ((current-length 0))
    (do ((n 1 (1+ n)))
        ((> n *end*) 'end)
      (let* ((c (cube n))
             (c-list (number-to-list c))
             (c-length (length c-list)))
        (if (= current-length c-length)
            (let ((result (compare c-list)))
              (if (not (null result))
                  (return-from euler062 (list-to-number result))))
            (progn
              (reset-list)
              (setf current-length c-length)))))))

(defun compare (c-list)
  (dolist (c-to-compare *cube-list*)
    (if (permutation? c-to-compare c-list)
        (progn
          (if (null (gethash c-to-compare *cube-hash*))
              (setf (gethash c-to-compare *cube-hash*) 2)
              (incf (gethash c-to-compare *cube-hash*)))
          (if (>= (gethash c-to-compare *cube-hash*) *limit*)
              (return-from compare c-to-compare)))))
  (push c-list *cube-list*)
  nil)

(defun reset-list ()
  ;(print *cube-hash*)
  (setf *cube-hash* (make-hash-table :test #'equal))
  (setf *cube-list* nil))

(defun cube (n)
  (* n n n))
