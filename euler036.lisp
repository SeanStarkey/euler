;;;;
;;;; Project Euler - Problem 36
;;;;

(defun euler036 ()
  (let ((sum 0))
    (do ((i 1 (1+ i)))
        ((= i 1000000) sum)
      (setf (fill-pointer *s*) 0)
      (format *s* "~A" i)
      (when (palindrome? *s*)
          (setf (fill-pointer *s*) 0)
          (format *s* "~B" i)
          (when (palindrome? *s*)
            (setf sum (+ sum i))))
      )
    )
  )


(defparameter *s*
  (make-array 0
    :element-type 'character
    :adjustable t
    :fill-pointer 0))

(defun palindrome? (s)
    (string= s (reverse s)))