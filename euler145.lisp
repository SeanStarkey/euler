;;;;
;;;; Project Euler - Problem 145
;;;;

(declaim (optimize speed))

(defun euler145 ()
   (setf t1 (sb-thread:make-thread #'step-1))
   (setf t2 (sb-thread:make-thread #'step-2))
   (setf t3 (sb-thread:make-thread #'step-3))
   (setf t4 (sb-thread:make-thread #'step-4))
   (+
    (sb-thread:join-thread t1)
    (sb-thread:join-thread t2)
    (sb-thread:join-thread t3)
    (sb-thread:join-thread t4)))

(require :sb-sprof)

(defun pro ()
  (sb-sprof:with-profiling (:mode :cpu :loop nil :show-progress t :report :graph) (too-long)))

(defun too-long ()
  (let ((total 0))
    (loop for n from 1 to 100000000 do
          (if (and (not (last-digit-zero? n))
                   (odd-digits? (+ n (reverse-number n))))
              (incf total)))
    total))

(defun step-1 ()
  (let ((total 0))
;    (loop for n from 1 to 250000000 do
    (loop for n from 1 to 25000000 do
          (if (and (not (last-digit-zero? n))
                   (odd-digits? (+ n (reverse-number n))))
              (incf total)))
    total))

(defun step-2 ()
  (let ((total 0))
;    (loop for n from 250000001 to 500000000 do
    (loop for n from 25000001 to 50000000 do
          (if (and (not (last-digit-zero? n))
                   (odd-digits? (+ n (reverse-number n))))
              (incf total)))
    total))

(defun step-3 ()
  (let ((total 0))
;    (loop for n from 500000001 to 750000000 do
    (loop for n from 50000001 to 75000000 do
          (if (and (not (last-digit-zero? n))
                   (odd-digits? (+ n (reverse-number n))))
              (incf total)))
    total))

(defun step-4 ()
  (let ((total 0))
;    (loop for n from 750000001 to 1000000000 do
    (loop for n from 75000001 to 100000000 do
          (if (and (not (last-digit-zero? n))
                   (odd-digits? (+ n (reverse-number n))))
              (incf total)))
    total))

(defun reverse-number (number)
  (declare (type integer number))
  (let ((ret 0))
    (loop
     (if (zerop number)
         (return ret))
     (multiple-value-bind (a b) (floor number 10)
       (setf number a)
       (setf ret (+ (* 10 ret) b))))))

(defun last-digit-zero? (number)
  (declare (type integer number))
  (= (mod number 10) 0))

(defun odd-digits? (number)
  (declare (type integer number))
  (loop
   (if (zerop number)
       (return t))
   (multiple-value-bind (a b) (floor number 10)
     (if (evenp b)
         (return nil))
     (setf number a))))

