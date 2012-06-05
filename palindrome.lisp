;;;;
;;;; Determines if a number is a palindrome
;;;;

(defun palindromep (number)
  (let ((s (write-to-string number)))
    (string= s (reverse s))))
