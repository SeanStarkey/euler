;;;;
;;;; Project Euler - Problem 99
;;;;

(load "split-string")

(defun euler099 ()
  (let ((index 0)
        (highest-index 0)
        (highest-value 0))
    (with-open-file (stream (probe-file "base_exp.txt"))
      (do ((line (read-line stream)
                 (read-line stream nil 'eof)))
          ((eq line 'eof))
        (let* ((nums (split-string-numbers line #\,))
               (a (car nums))
               (e (cadr nums))
               (l (* e (log a))))
          (incf index)
          (if (> l highest-value)
              (progn
                (setf highest-index index)
                (setf highest-value l))))))
    highest-index))
