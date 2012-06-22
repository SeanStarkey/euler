;;;;
;;;; Project Euler - Problem 32
;;;;
;;;; A x BCDE = FGHI
;;;; AB x CDE = FGHI

(load "permutations.lisp")
(load "pandigital.lisp")

(defun euler032 ()
  (let ((final '()))
    (do ((A 1 (1+ A)))
        ((> A 9))
      (do ((BCDE 1234 (1+ BCDE)))
          ((> BCDE 9876))
        (let ((FGHI (* A BCDE)))
          (if (pandigital-1-9-p (concatenate 'list
                                             (number-to-list A)
                                             (number-to-list BCDE)
                                             (number-to-list FGHI)))
                (setf final (cons FGHI final))))))
    (do ((AB 12 (1+ AB)))
        ((> AB 98))
      (do ((CDE 123 (1+ CDE)))
          ((> CDE 987))
        (let ((FGHI (* AB CDE)))
          (if (pandigital-1-9-p (concatenate 'list
                                             (number-to-list AB)
                                             (number-to-list CDE)
                                             (number-to-list FGHI)))
                (setf final (cons FGHI final))))))
    (delete-duplicates final)
    (reduce #'+ final)))
