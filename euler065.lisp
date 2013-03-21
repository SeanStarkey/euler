;;;;
;;;; Problem 65
;;;;

(load "sum-of-digits")

(defun euler065 ()
  (sum-of-digits (numerator (+ 2 (convergence 1 100)))))

(defun convergence (n end)
  (if (= n (1- end))
      (/ 1 (coeff n))
      (/ 1 (+ (coeff n) (convergence (1+ n) end)))))

(defun coeff (n)
  (let ((frac (/ (- n 2) 3)))
    (if (= (floor frac) frac)
        (* 2 (1+ frac))
        1)))
