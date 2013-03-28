;;;;
;;;; Project Euler - Problem 74
;;;;

(load "permutations")

(defun euler074 ()
  (let ((total 0))
    (do ((n 1 (1+ n)))
        ((> n 1000000))
      (if (= 60 (chain-length n))
          (incf total)))
    total))

; memoizing this would make it faster - but it is fast enough
(defun chain-length (n)
  (let ((chain (list n)))
    (do ((next (next-in-chain n) (next-in-chain next)))
        ((member next chain) (length chain))
      (setf chain (append chain (list next))))))

(defun next-in-chain (n)
  (reduce '+ (map 'list 'fast-fact (number-to-list n))))

(defun fast-fact (n)
  (case n
    (0 1)
    (1 1)
    (2 2)
    (3 6)
    (4 24)
    (5 120)
    (6 720)
    (7 5040)
    (8 40320)
    (9 362880)))
