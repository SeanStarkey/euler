;;;;
;;;; Project Euler - Problem 102
;;;;

(load "split-string")

(defun euler102 ()
  (let ((total 0))
    (dolist (tri (read-triangles-file "triangles.txt"))
      (if (origin-in-triangle? tri)
          (incf total)))
    total))

(defun origin-in-triangle? (triangle)
  (let* ((A (list (car triangle) (nth 1 triangle)))
         (B (list (nth 2 triangle) (nth 3 triangle)))
         (C (list (nth 4 triangle) (nth 5 triangle)))
         (v0 (vector-subtract C A))
         (v1 (vector-subtract B A))
         (v2 (vector-subtract '(0 0) A))
         (dot00 (dot-product v0 v0))
         (dot01 (dot-product v0 v1))
         (dot02 (dot-product v0 v2))
         (dot11 (dot-product v1 v1))
         (dot12 (dot-product v1 v2))
         (invDenom (/ 1 (- (* dot00 dot11) (* dot01 dot01))))
         (u (* invDenom (- (* dot11 dot02) (* dot01 dot12))))
         (v (* invDenom (- (* dot00 dot12) (* dot01 dot02)))))
    (and (>= u 0) (>= v 0) (< (+ u v) 1))))

(defun dot-product (a b)
  (reduce '+ (mapcar '* a b)))

(defun vector-subtract (a b)
  (list (- (car a) (car b)) (- (cadr a) (cadr b))))

(defun read-triangles-file (filename)
  (let ((results nil))
    (with-open-file (stream (probe-file filename))
      (do ((line (read-line stream)
                 (read-line stream nil 'eof)))
          ((eq line 'eof))
        (push (split-string-numbers line #\,) results)))
    (reverse results)))
