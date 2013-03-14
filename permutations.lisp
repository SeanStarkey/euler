;;;;
;;;; Compute the permutations of a list
;;;;
(defun permutations (bag)
  (if (null bag)
      '(())
      (mapcan #'(lambda (e)
                  (mapcar #'(lambda (p) (cons e p))
                          (permutations
                           (remove e bag :count 1))))
              bag)))

(defun permutations-number (number)
  (mapcar #'list-to-number (permutations (number-to-list number))))

(defun same-bag-p (bag1 bag2 &key (test #'eql))
  (let ((table (make-hash-table :test test)))
    (loop for key in bag1 do (incf (gethash key table 0)))
    (loop for key in bag2 do (decf (gethash key table 0)))
    (loop for val being each hash-value of table always (= val 0))))

(defun permutation? (a b)
  (same-bag-p a b))

(defun number-to-list (number)
  (if (zerop number)
      nil
      (multiple-value-bind (quotient remainder) (floor number 10)
        (append (number-to-list quotient)
                (list remainder)))))

(defun list-to-number (lst)
  (list-to-number-helper (reverse lst)))

(defun list-to-number-helper (lst)
  (if (null lst)
      0
      (+ (car lst) (* 10 (list-to-number-helper (cdr lst))))))

(defun fact (n)
  (if (= n 0)
      1
      (* n (fact (- n 1)))))

(defun c (n r)
  (/ (fact n) (* (fact r) (fact (- n r)))))

(defun p (n r)
  (/ (fact n) (fact (- n r))))
