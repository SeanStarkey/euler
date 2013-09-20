;;;;
;;;; Project Euler - Problem 75
;;;;

;;
;; Pythagorean triple algorithm found here:
;; https://en.wikipedia.org/wiki/Tree_of_Pythagorean_triples
;;

(defparameter *end* 1500000)
(defparameter *result-vector* (make-array (1+ *end*) :initial-element 0))

(defparameter *pythagorean-triple-A* '((1 2 2) (-2 -1 -2) (2 2 3)))
(defparameter *pythagorean-triple-B* '((1 2 2) (2 1 2) (2 2 3)))
(defparameter *pythagorean-triple-C* '((-1 -2 -2) (2 1 2) (2 2 3)))
(defparameter *pythagorean-triple-start* '((3 4 5)))
(defparameter *pythagorean-triples-primitives* nil)

(defun euler075 ()
  (get-and-store-pythagorean-triples *pythagorean-triple-start*)
  (dolist (triple *pythagorean-triples-primitives*)
    (let ((perimeter (get-perimeter triple)))
      (do* ((multiplier 1 (1+ multiplier))
            (l (* multiplier perimeter) (* multiplier perimeter)))
          ((> l *end*))
        (incf (elt *result-vector* l)))))

  (let ((total 0))
    (loop for n from 1 to *end* do
          (if (= 1 (elt *result-vector* n))
              (incf total)))
    total))

(defun get-perimeter (triple)
  (+ (caar triple) (cadar triple) (caddar triple)))

(defun get-and-store-pythagorean-triples (triple)
  (let ((a (caar triple))
        (b (cadar triple))
        (c (caddar triple)))
    (if (<= (+ a b c) *end*)
        (progn
          (push triple *pythagorean-triples-primitives*)
          (get-and-store-pythagorean-triples
           (matrix-multiply triple *pythagorean-triple-A*))
          (get-and-store-pythagorean-triples
           (matrix-multiply triple *pythagorean-triple-B*))
          (get-and-store-pythagorean-triples
           (matrix-multiply triple *pythagorean-triple-C*))
))))

(defun matrix-multiply (matrix1 matrix2)
 (mapcar
  (lambda (row)
   (apply #'mapcar
    (lambda (&rest column)
     (apply #'+ (mapcar #'* row column))) matrix2)) matrix1))
