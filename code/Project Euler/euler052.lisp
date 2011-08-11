;;;;
;;;; Project Euler - Problem 52
;;;;

(defun euler052 ()
  (let ((num 10)
        (len 2))
    (loop
       (let* ((num-str (write-to-string num))
             (num-set (remove-duplicates (coerce num-str 'list))))
         ;(format t "~A ~A ~A ~%" num (length num-str) (length num-set))
         (if (/= (length num-str) (length num-set))
             (incf num)
             (progn
               (let* ((x6 (* 6 num))
                      (x6-str (write-to-string x6)))
                 ;(format t "~A ~A ~%" num x6)
                 (if (/= (length num-str) (length x6-str))
                     (progn
                       (setf num (expt 10 len))
                       (incf len))
                     (progn
                       (if (and (same-digits num (* 2 num))
                                (same-digits num (* 3 num))
                                (same-digits num (* 4 num))
                                (same-digits num (* 5 num))
                                (same-digits num (* 6 num)))
                           (return num)
                           (incf num)))))))))))

(defun same-digits (n1 n2)
  (let* ((n1-str (write-to-string n1))
         (n1-set (remove-duplicates (coerce n1-str 'list)))
         (n2-str (write-to-string n2))
         (n2-set (remove-duplicates (coerce n2-str 'list))))
    ;(format t "~A ~A ~%" n1-set n2-set)
    (null (set-exclusive-or n1-set n2-set))))
