;;;
;;; This function splits a string by a delimeter
;;;
(defun split-string-numbers (string delimeter)
  (mapcar #'parse-integer (split-string string delimeter)))

(defun split-string (string delimeter)
  (let ((lst nil))
    (do* ((pos 0 pos)
          (next-pos (position delimeter string)
                    (position delimeter string :start pos)))
        ((eq next-pos nil) (push (subseq string pos) lst))
      (push (subseq string pos next-pos) lst)
      (setf pos (1+ next-pos)))
    (reverse lst)))
