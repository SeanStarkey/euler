;;;;
;;;; Project Euler - Problem 22
;;;;

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

(defun remove-quotes (string)
  (subseq string 1 (1- (length string))))

(defun read-word-list (filename)
  (with-open-file (stream (probe-file filename))
                  (split-string (read-line stream nil) #\,)))

(defun split-string (string delimeter)
  (let ((word-list nil))
    (do* ((pos 0 pos)
          (next-pos (position delimeter string)
                    (position delimeter string :start pos)))
        ((eq next-pos nil) (push (subseq string pos) word-list))
      (push (subseq string pos next-pos) word-list)
      (setf pos (1+ next-pos)))
    (reverse word-list)))
