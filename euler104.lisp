;;;;
;;;; Project Euler - Problem 104
;;;;

(load "pandigital")

(defparameter *front-limit* 1000000000000000)
(defparameter *rear-limit* 1000000000000)

(defun euler104 ()
  (let ((front '(1 1))
        (rear '(1 1))
        (k 2))
    (loop do
          (incf k)
          (push (+ (car front) (cadr front)) front)
          (setf front (butlast front))
          (if (> (car front) *front-limit*)
              (setf front (list (floor (+ (car front) 5) 10)
                                (floor (+ (cadr front) 5) 10))))
          (push (+ (car rear) (cadr rear)) rear)
          (setf rear (butlast rear))
          (if (> (car rear) *rear-limit*)
              (setf rear (list (mod (car rear) *rear-limit*)
                               (mod (cadr rear) *rear-limit*))))

          (if (> k 50)
              (let* ((fib-str (write-to-string (car front)))
                     (fib-front (mapcar #'(lambda (x)
                                            (- (char-int x) 48))
                                        (coerce (subseq fib-str 0 9) 'list))))
                (if (pandigital-1-9-p fib-front)
                    (progn
                      (let* ((fib-str (write-to-string (car rear)))
                             (fib-rear (mapcar #'(lambda (x)
                                                   (- (char-int x) 48))
                                               (coerce (subseq (reverse fib-str) 0 9) 'list))))
                        (if (pandigital-1-9-p fib-rear)
                            (return-from euler104 k))))))))))
