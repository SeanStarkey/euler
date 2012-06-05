;;;;
;;;; Project Euler - Problem 22
;;;;

(load "read-word-list.lisp")

(defun euler022 ()
  (let ((names-list (sorted-name-list))
        (sum 0)
        (iter 1))
    (dolist (name names-list)
      (setf sum (+ sum (* (score name) iter)))
      (setf iter (1+ iter)))
    sum))

(defun sorted-name-list ()
  (mapcar 'remove-quotes (sort (read-word-list "names.txt") #'string-lessp)))

(defun score (string)
  (let ((sum 0))
    (do ((x 0 (1+ x)))
        ((= x (length string)))
      (setf sum (+ sum (- (char-code (elt string x)) 64))))
    sum))
