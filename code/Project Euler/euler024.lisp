;;;;
;;;; Project Euler - Problem 24
;;;;

(defun euler024 ()
  (get-next "0123")
  )

(defun get-next (str)
  (if (eql 2 (length str))
      (if (char< (elt str 0) (elt str 1))
          (reverse str)
          nil)
      (let ((last (get-next (substring str 1))))
        (if (null last)
            (print "rearrange")
            (concatenate 'string (substring str 0 1) last)))))

(defun get-highest (str)
  (let ((index -1)
        (ch #\0))
    (dotimes (n (length str))
      (if (char< ch (elt str n))
          (setf index n
                ch (elt str n))))
    index))

