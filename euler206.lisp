;;;;
;;;; Project Euler - Problem 206
;;;;
;;;; (sqrt 1020304050607080900) -> 1010100992
;;;; (sqrt 1929394959697989990) -> 1389026560

(load "permutations")

(defun euler206 ()
  (do ((x 1010100990 (+ x 10)))
      ((> x 13890265560))
    (if (pattern? (* x x))
        (return-from euler206 x))))

(defun pattern? (n)
  (let ((l (number-to-list n)))
    (and (= 0 (nth 18 l))
         (= 9 (nth 16 l))
         (= 8 (nth 14 l))
         (= 7 (nth 12 l))
         (= 6 (nth 10 l))
         (= 5 (nth 8 l))
         (= 4 (nth 6 l))
         (= 3 (nth 4 l))
         (= 2 (nth 2 l))
         (= 1 (nth 0 l)))))


