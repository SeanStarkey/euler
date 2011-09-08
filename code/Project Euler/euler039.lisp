;;;;
;;;; Project Euler - Problem 39
;;;;

(defun euler039 ()
  (let ((max 0)
        (num 0))
    (do ((p 3 (incf p))
         (cur 0 0))
        ((> p 1000) max)
      (do ((a 1 (incf a)))
          ((> a p))
        (do* ((b (1+ a) (incf b))
              (c (- p a b) (- p a b))
              (r (right? a b c) (right? a b c)))
             ((or
               (< c 0)
               (>= b c)) (when (> cur num)
                           (setf num cur)
                           (setf max p)))
          (if (= r 0)
              (incf cur)))))))


(defun right? (a b c)
  (let ((sum (+ (* a a)
                (* b b)))
        (h (* c c)))
    (cond
      ((< sum h) -1)
      ((> sum h) 1)
      (t 0))))
