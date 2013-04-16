;;;;
;;;; Project Euler - Problem 60
;;;;

;; 173 is (prime 122)

(defparameter *max* 1100)

(load "prime")

(defun euler060 ()
  (prime-init 1000000)
  (let ((total (* 5 (prime *max*))))
    (loop for a-i from 2 to *max* do
         (let ((a (prime a-i)))
           (loop for b-i from a-i to *max* do
                (let ((b (prime b-i)))
                  (if (and (primep (concatenate-numbers a b))
                           (primep (concatenate-numbers b a)))
                      (loop for c-i from b-i to *max* do
                           (let ((c (prime c-i)))
                             (if (and (primep (concatenate-numbers a c))
                                      (primep (concatenate-numbers c a))
                                      (primep (concatenate-numbers b c))
                                      (primep (concatenate-numbers c b)))
                                 (loop for d-i from c-i to *max* do
                                      (let ((d (prime d-i)))
                                        (if (and (primep (concatenate-numbers a d))
                                                 (primep (concatenate-numbers d a))
                                                 (primep (concatenate-numbers b d))
                                                 (primep (concatenate-numbers d b))
                                                 (primep (concatenate-numbers c d))
                                                 (primep (concatenate-numbers d c)))
                                            (loop for e-i from d-i to *max* do
                                                 (let ((e (prime e-i)))
                                                   (if (and (primep (concatenate-numbers a e))
                                                            (primep (concatenate-numbers e a))
                                                            (primep (concatenate-numbers b e))
                                                            (primep (concatenate-numbers e b))
                                                            (primep (concatenate-numbers c e))
                                                            (primep (concatenate-numbers e c))
                                                            (primep (concatenate-numbers d e))
                                                            (primep (concatenate-numbers e d)))
                                                       (let ((sum (+ a b c d e)))
                                                         (format t "~A ~A ~A ~A ~A~%" a b c d e)
                                                         (if (< sum total)
                                                             (setf total sum)))))))))))))))))
    total))


(defun concatenate-numbers (n m)
  (if (< m 10)
      (return-from concatenate-numbers (+ (* n 10) m)))
  (if (< m 100)
      (return-from concatenate-numbers (+ (* n 100) m)))
  (if (< m 1000)
      (return-from concatenate-numbers (+ (* n 1000) m)))
  (if (< m 10000)
      (return-from concatenate-numbers (+ (* n 10000) m)))
  (if (< m 100000)
      (return-from concatenate-numbers (+ (* n 100000) m)))
  (if (< m 1000000)
      (return-from concatenate-numbers (+ (* n 1000000) m))))
