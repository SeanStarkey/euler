;;;;
;;;; Project Euler 68
;;;;
;;;
;;; Vector positions:
;;;
;;;       5
;;;         -
;;;           -
;;;             0        6
;;;           -  -     -
;;;         -      -  -
;;;       4          1
;;;     -  -        -
;;;   -     -      -
;;; 9        3----2----7
;;;           -
;;;            -
;;;             8

(load "permutations")

(defun euler068 ()
  (let ((results (permutations '(1 2 3 4 5 6 7 8 9 10))))
    (setf results (remove-if-not #'(lambda (l)
                                     (ismagic? (list->vector l))) results))
    (setf results (remove-if-not #'remove-gon-duplicates results))
    (setf results (mapcar #'asstring results))
    (setf results (remove-if-not #'(lambda (s) (= 16 (length s))) results))
    (setf results (sort results #'string-greaterp))
    (car results)
  ))

(defun asstring (v)
  (concatenate 'string (write-to-string (elt v 5))
               (write-to-string (elt v 0))
               (write-to-string (elt v 1))
               (write-to-string (elt v 6))
               (write-to-string (elt v 1))
               (write-to-string (elt v 2))
               (write-to-string (elt v 7))
               (write-to-string (elt v 2))
               (write-to-string (elt v 3))
               (write-to-string (elt v 8))
               (write-to-string (elt v 3))
               (write-to-string (elt v 4))
               (write-to-string (elt v 9))
               (write-to-string (elt v 4))
               (write-to-string (elt v 0))
               ))

(defun remove-gon-duplicates (v)
  (and (< (elt v 5) (elt v 6))
       (< (elt v 5) (elt v 7))
       (< (elt v 5) (elt v 8))
       (< (elt v 5) (elt v 9))))

(defun list->vector (l)
  (make-array (length l) :initial-contents l))

(defun ismagic? (v)
  (let ((sum (+ (elt v 5) (elt v 0) (elt v 1))))
    (if (/= sum (+ (elt v 6) (elt v 1) (elt v 2)))
        (return-from ismagic? nil))
    (if (/= sum (+ (elt v 7) (elt v 2) (elt v 3)))
        (return-from ismagic? nil))
    (if (/= sum (+ (elt v 8) (elt v 3) (elt v 4)))
        (return-from ismagic? nil))
    (if (/= sum (+ (elt v 9) (elt v 4) (elt v 0)))
        (return-from ismagic? nil))
    t))
