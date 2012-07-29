;;;;
;;;; Project Euler - Problem 43
;;;;

(load "pandigital")
(load "permutations")


(defun euler043 ()
  (let ((sum 0))
    (do ((x 1234567000 (+ x 1000)))
        ((> x 10000000000) sum)
      (do ((y 17 (+ y 17)))
          ((> y 1000))
        (let ((z (+ x y)))
          (if (and (div13 z)
                   (div11 z)
                   (div7 z)
                   (div5 z)
                   (div3 z)
                   (div2 z)
                   (pandigital-0-9-p (number-to-list z))
                   )
              (setf sum (+ sum z)))
          )))))

(defun div2 (x)
  (eq (mod (mod (floor x 1000000) 1000) 2) 0))

(defun div3 (x)
  (eq (mod (mod (floor x 100000) 1000) 3) 0))

(defun div5 (x)
  (eq (mod (mod (floor x 10000) 1000) 5) 0))

(defun div7 (x)
  (eq (mod (mod (floor x 1000) 1000) 7) 0))

(defun div11 (x)
  (eq (mod (mod (floor x 100) 1000) 11) 0))

(defun div13 (x)
  (eq (mod (mod (floor x 10) 1000) 13) 0))
