;;;;
;;;; Project Euler - Problem 112
;;;;

(load "permutations")

(defun euler112 ()
  (let ((bouncy 0))
    (do ((n 100 (1+ n)))
        ((>= (/ bouncy n) 1.0) n)
      (if (bouncy? n)
          (incf bouncy))
      (if (>= (/ bouncy n) (/ 99 100))
          (return-from euler112 n)))))

(defun bouncy? (n)
  (let* ((lst (number-to-list n))
         (dir (if (> (car lst) (cadr lst))
                  1
                  (if (< (car lst) (cadr lst))
                      -1
                      0))))
    (do ((i 1 (1+ i)))
        ((= (1- (length lst)) i) nil)
      (if (> (nth i lst) (nth (1+ i) lst))
          (if (= dir -1)
              (return-from bouncy? t)
              (if (= dir 0)
                  (setf dir 1))))
      (if (< (nth i lst) (nth (1+ i) lst))
          (if (= dir 1)
              (return-from bouncy? t)
              (if (= dir 0)
                  (setf dir -1)))))
    nil))


    (labels ((b (lst dir)   ; dir: 1 = ascending -1 = decending
               (if (= (length lst) 1)
                   dir
                   (if (= dir 0)
                       (if (> (car lst) (cadr lst))
                           (b (cdr lst) 1)
                           (b (cdr lst) 0))
                       (if (< (car lst) (cadr lst))
                           (if (= dir -1)
                               (b (cdr lst) -1)
                               0)
                           (if (> (car lst) (cadr lst))
                               (if (= dir 1)
                                   (b (cdr lst) 1)
                                   0)
                               dir))))))
      (if (< (car lst) (cadr lst))
          (b (cdr lst) -1)
          (if (> (car lst) (cadr lst))
              (b (cdr lst) 1)
              (b (cdr lst) 0))))))
