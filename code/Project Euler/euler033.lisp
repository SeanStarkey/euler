;;;;
;;;; Project Euler 33
;;;;

(defun euler033 ()
  (let ((n-product 1)
        (d-product 1))
    (do ((n 10 (1+ n)))
        ((> n 99) (denominator (/ n-product d-product)))
      (let ((n-tens (tens n))
            (n-ones (ones n)))
        (do ((d 10 (1+ d)))
            ((> d 99))
          (let ((d-tens (tens d))
                (d-ones (ones d)))
            (if (and (= d-tens n-ones)
                     (/= d-ones 0)
                     (= (/ n d) (/ n-tens d-ones))
                     (/= (/ n d) 1))
                (progn
                  (setf n-product (* n-tens n-product))
                  (setf d-product (* d-ones d-product)))
                ))))))))


(defun tens (n)
  (floor n 10))

(defun ones (n)
  (mod n 10))
