;;;;
;;;; Project Euler - Problem 42
;;;;

(load "read-word-list.lisp")

(defun euler042 ()
  (let ((word-list (mapcar 'remove-quotes (read-word-list "words.txt")))
        (count 0))
    (dolist (word word-list)
      (when (trianglep (word-value word))
        (incf count)))
    count))

(defun word-value (word)
  (let ((sum 0))
    (do ((x 0 (1+ x)))
        ((= x (length word)))
      (setf sum (+ sum (- (char-code (elt word x)) 64))))
    sum))

(defun trianglep (value)
  (do* ((x 0 (1+ x))
        (tri 1 (/ (* x (1+ x)) 2)))
       ((>= tri value) (= tri value))))
