;;
;; Formula found on wikipedia
;;
(defun phi (n)
  (labels ((f (m)
          (- 1 (/ 1 m))))
    (* n (reduce #'* (mapcar #'f (mapcar #'car (factors n)))))))
