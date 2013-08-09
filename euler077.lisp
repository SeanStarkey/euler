;;;;
;;;; Project Euler - Problem 77
;;;;

(load "memoize")
(load "prime")

(defun euler077 ()
  (prime-init 10000)
  (let ((number 2))
    (loop
       (if (> (length (funcall ways-memoized number)) 5000)
           (return-from euler077 number))
       (incf number)))
)

(defun create-vector (n)
  (let ((ret (make-array n :initial-element 0)))
    (setf (elt ret (1- n)) 1)
    ret))

(defun combine-vectors (a b)
  (let ((ret (make-array (max (length a) (length b)) :initial-element 0)))
    (loop for a-index from 0 to (1- (length a)) do
         (setf (elt ret a-index) (+ (elt a a-index) (elt ret a-index))))
    (loop for b-index from 0 to (1- (length b)) do
         (setf (elt ret b-index) (+ (elt b b-index) (elt ret b-index))))
    ret))

(defun ways (n)
  (let ((number 0)
        (prime-index 0)
        (sums-vector nil))
    (loop
       (incf prime-index)
       (if (> (* 2 (prime prime-index)) n)
           (progn
             (if (primep n)
                 (push (create-vector (prime-nth n)) sums-vector))
             (return-from ways (remove-duplicates sums-vector :test #'equalp))))
       (let ((diff (- n (prime prime-index))))
         (if (>= diff (- n diff))
             (let ((new-prime-vector (create-vector prime-index))
                   (diff-vectors (funcall ways-memoized diff)))
               (dolist (vec diff-vectors)
                 (push (combine-vectors new-prime-vector vec) sums-vector)
                 )))))))

(defvar ways-memoized (memoize #'ways))
