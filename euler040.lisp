;;;;
;;;; Project Euler - Problem 40
;;;;

(defun euler040 ()
  (let ((product 1)
        (current-integer 1)
        (current-place 1)
        (current-test 1)
        (previous-integer 0)
        (previous-place 0))
    (loop
       (when (null current-test)
           (return))
       (when (> current-place current-test)
         (setf product (* product
                          (get-digit previous-integer previous-place current-test)))
         (setf current-test (next-to-test current-test)))
       (setf previous-integer current-integer)
       (setf previous-place current-place)
       (setf current-place (+ current-place (digit-length current-integer)))
       (incf current-integer))
    product))


(defun get-digit (integer place test)
  (let ((pos (- test place)))
    (parse-integer (subseq (write-to-string integer) pos (1+ pos)))))

(defun digit-length (n)
  (cond
    ((< n 10) 1)
    ((< n 100) 2)
    ((< n 1000) 3)
    ((< n 10000) 4)
    ((< n 100000) 5)
    ((< n 1000000) 6)
    ((< n 10000000) 7)
    (T nil)))


(defun next-to-test (n)
  (cond
    ((= n 1) 10)
    ((= n 10) 100)
    ((= n 100) 1000)
    ((= n 1000) 10000)
    ((= n 10000) 100000)
    ((= n 100000) 1000000)
    ((= n 1000000) nil)))
