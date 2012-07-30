;;;;
;;;; Project Euler - Problem 49
;;;;
;;;; (prime 169) -> 1009
;;;; (prime 1230) -> 9973

(load "prime")
(load "permutations")

(defparameter *start* 169)
(defparameter *end* 1230)

(defun euler049 ()
  (prime-init 10500)
  (do* ((i *start* (1+ i))
        (p1 (prime i) (prime i)))
      ((>= i *end*))
    (let ((perm (cdr (mapcar 'list-to-number (permutations (number-to-list p1))))))
      (setf perm (delete p1 perm :test-not #'(lambda (x y)
                                    (if (and (< x y)
                                             (primep y)
                                             )
                                        t
                                        nil))))
      (setf perm (remove-duplicates perm))
      (setf perm (sort perm #'<))
      (mapc #'(lambda (p2)
                (let* ((diff (- p2 p1))
                       (p3 (+ p2 diff)))
                  (if (and (/= p1 1487) (member p3 perm))
                      (return-from euler049 (+ (* 10000 (+ (* 10000 p1) p2)) p3)))))
            perm))))
