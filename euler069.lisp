;;;;
;;;; Problem 69
;;;;

(load "factors")

(defun euler069 ()
  (factors-init 1000000)
  (let ((max 0)
        (n 0))
    (do ((i 1 (1+ i)))
        ((> i 1000000))
      (let ((ratio (/ i (phi i))))
        (if (> ratio max)
            (progn
              (setf max ratio)
              (setf n i)))))
    n))

;;
;; Formula found on wikipedia
;;
(defun phi (n)
  (labels ((f (m)
          (- 1 (/ 1 m))))
    (* n (reduce #'* (mapcar #'f (mapcar #'car (factors n)))))))
