;;;;
;;;; Functions for pandigital
;;;;

(defun pandigital-1-9-p (l)
  (if (/= (length l) 9)
      nil
      (do ((x 1 (1+ x)))
          ((> x 9) t)
        (if (/= 1 (count x l))
            (return nil)))))

(defun pandigital-0-9-p (l)
  (if (/= (length l) 10)
      nil
      (do ((x 0 (1+ x)))
          ((> x 9) t)
        (if (/= 1 (count x l))
            (return nil)))))

