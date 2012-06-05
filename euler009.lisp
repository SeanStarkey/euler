;;;;
;;;; Project Euler - Problem 9
;;;;

(defun euler009 ()
  (let ((product 0))
    (do ((a 1 (1+ a)))
        ((> a 1000))
      (do ((b a (1+ b)))
          ((> b 1000))
        (let ((sum (+ (* a a) (* b b))))
          (let ((c (isqrt sum)))
            (if (= (sqrt sum) (isqrt sum))
                (if (= (+ a b c) 1000)
                    (setf product (* a b c))))))))
    product))
