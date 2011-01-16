;;;;
;;;; Project Euler - Problem 24
;;;;

(defun euler024 ()
  (setq str (make-string 10))
  (setf (char str 0) #\0)
  (setf (char str 1) #\1)
  (setf (char str 2) #\2)
  (setf (char str 3) #\3)
  (setf (char str 4) #\4)
  (setf (char str 5) #\5)
  (setf (char str 6) #\6)
  (setf (char str 7) #\7)
  (setf (char str 8) #\8)
  (setf (char str 9) #\9)

  (dotimes (n 999999)
    (get-next str))
  str
  )

(defun get-next (str)
  (let* ((N (length str))
         (i (1- N))
         (j N)
         (return-str str))
    (loop
       (when (char< (char str (1- i)) (char str i)) (return))
       (setf i (1- i)))
    (loop
       (when (char> (char str (1- j)) (char str (1- i))) (return))
       (setf j (1- j)))
    (swap return-str (1- i) (1- j))
    (setf i (1+ i)
          j N)
    (loop
       (when (not (< i j)) (return))
       (swap return-str (1- i) (1- j))
       (setf i (1+ i))
       (setf j (1- j)))
    return-str))

(defun swap (str x y)
  (let ((c (char str x))
        (return-str str))
    (setf (char return-str x) (char str y))
    (setf (char return-str y) c)
    return-str))
