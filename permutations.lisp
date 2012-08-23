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

(defun ! (n)
  (if (= n 0)
      1
      (* n (! (- n 1)))))

(defun c (n r)
  (/ (! n) (* (! r) (! (- n r)))))

(defun p (n r)
  (/ (! n) (! (- n r))))
