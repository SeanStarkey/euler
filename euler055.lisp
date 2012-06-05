;;;;
;;;; Project Euler - Problem 55
;;;;

(load "palindrome.lisp")

(defun euler055 ()
  (do ((iter 0 (1+ iter))
       (number-found 0 number-found))
      ((= iter 10000) number-found)
    (if (lychrelp iter)
        (setf number-found (1+ number-found)))))

(defun lychrelp (number)
  (do ((iter 0 (1+ iter)))
    ((> iter 50) t)
    (setf number (+ number
                    (parse-integer (reverse (write-to-string number)))))
    (if (palindromep number)
        (return nil))))

