(defun triangle (n)
  (/ (* n (+ n 1)) 2))

(defun square (n)
  (* n n))

(defun pentagonal (n)
  (/ (* n (- (* 3 n) 1)) 2))

(defun hexagonal (n)
  (* n (- (* 2 n) 1)))

(defun heptagonal (n)
  (/ (* n (- (* 5 n) 3)) 2))

(defun octagonal (n)
  (* n (- (* 3 n) 2)))
