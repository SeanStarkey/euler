;;;;
;;;; Project Euler - Problem 96
;;;;

(defun euler096 ()
  (let ((total 0))
    (with-open-file (stream (probe-file "sudoku.txt"))
                    (do ((line (read-line stream)
                               (read-line stream nil 'eof))
                         (puzzle (make-array '(9 9)))
                         (x 0)
                         (y 0))
                        ((eq line 'eof))
                      (if (not (eql (elt line 0) #\G))
                          (progn
                            (map 'string #'(lambda (c) (setf (aref puzzle y x) c) (incf x) c) line)
                            (incf y)
                            (setf x 0)
                            ))
                      (if (= y 9)
                          (progn
                            (setf puzzle (solve-puzzle puzzle))
;                            (print puzzle)
                            (setf total (+ total (* 100 (- (char-code (aref puzzle 0 0)) (char-code #\0)))
                                           (* 10 (- (char-code (aref puzzle 0 1)) (char-code #\0)))
                                           (- (char-code (aref puzzle 0 2)) (char-code #\0))))
                            (setf puzzle (make-array '(9 9)))
                            (setf y 0)))
                      ))
    total))

(defun solve-puzzle (puzzle-in)
  (let ((puzzle (copy-array puzzle-in)))
;  (format t "SP: ~%~A~%******************~%" puzzle)
  (block singles
    (let ((x 0) (y 0))
      (loop do
            (if (eql (aref puzzle y x) #\0)
                (let ((possibilities (find-possibilities puzzle x y)))
;                  (format t "S ~A ~A ~A~%" x y possibilities)
                  (if (= (length possibilities) 0)
                      (return-from solve-puzzle nil))
                  (if (= (length possibilities) 1)
                      (progn
                        (setf (aref puzzle y x) (car possibilities))
;                        (format t "Sset ~A ~A ~A~%" x y (car possibilities))
                        (setf y 0)
                        (setf x -1))))
;                (format t "S*~A ~A ~A~%" x y (aref puzzle y x))
                )
            (incf x)
            (if (= x 9)
                (progn
                  (incf y)
                  (setf x 0)))
            (if (= y 9)
                (return-from singles)))))
  (block multiples
    (let ((x 0) (y 0))
      (loop do
            (if (eql (aref puzzle y x) #\0)
                (let ((possibilities (find-possibilities puzzle x y)))
;                  (format t "M ~A ~A ~A~%" x y possibilities)
                  (dolist (p possibilities)
;                    (format t "Mtry ~A ~A ~A~%" x y p)
                    (let ((test-puzzle (test-number puzzle x y p)))
                      (if (not (null test-puzzle))
                          (return-from solve-puzzle (solve-puzzle test-puzzle)))))
                  (return-from solve-puzzle nil))
;                (format t "M*~A ~A ~A~%" x y (aref puzzle y x))
                )
            (incf x)
            (if (= x 9)
                (progn
                  (incf y)
                  (setf x 0)))
            (if (= y 9)
                (return-from multiples puzzle)))))
  puzzle))

(defun test-number (puzzle-in x y val)
  (let ((puzzle (copy-array puzzle-in)))
  (setf (aref puzzle y x) val)
  (solve-puzzle puzzle)))

(defun find-possibilities (puzzle x y)
  (let ((ret-lst '()))
    (loop for c in '(#\1 #\2 #\3 #\4 #\5 #\6 #\7 #\8 #\9) do
          (push c ret-lst))
    (loop for i from 0 to 8 do
          (setf ret-lst (remove (aref puzzle i x) ret-lst))
          (setf ret-lst (remove (aref puzzle y i) ret-lst)))
    (let ((xbase (* 3 (floor x 3)))
          (ybase (* 3 (floor y 3))))
      (loop for ix from xbase to (+ xbase 2) do
            (loop for iy from ybase to (+ ybase 2) do
                  (setf ret-lst (remove (aref puzzle iy ix) ret-lst)))))
    ret-lst))


(defun copy-array (array &key
                   (element-type (array-element-type array))
                   (fill-pointer (and (array-has-fill-pointer-p array)
                                      (fill-pointer array)))
                   (adjustable (adjustable-array-p array)))
  "Returns an undisplaced copy of ARRAY, with same fill-pointer and
adjustability (if any) as the original, unless overridden by the keyword
arguments."
  (let* ((dimensions (array-dimensions array))
         (new-array (make-array dimensions
                                :element-type element-type
                                :adjustable adjustable
                                :fill-pointer fill-pointer)))
    (dotimes (i (array-total-size array))
      (setf (row-major-aref new-array i)
            (row-major-aref array i)))
    new-array))

