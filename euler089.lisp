;;;;
;;;; Project Euler - Problem 89
;;;;

(defun euler089 ()
  (let ((total 0))
    (with-open-file (stream (probe-file "roman.txt"))
      (do ((line (read-line stream)
                 (read-line stream nil 'eof)))
          ((eq line 'eof))
        (let* ((value (roman-to-number line))
               (new-roman (number-to-roman value)))
          (setf total (+ total (- (length line) (length new-roman)))))))
    total))

(defun number-to-roman (number)
  (let ((final ""))
    (setf number (do ((n number (- n 1000)))
                     ((< n 1000) n)
                   (setf final (concatenate 'string final "M"))))
    (if (>= number 900)
        (progn
          (setf final (concatenate 'string final "CM"))
          (setf number (- number 900))))
    (if (>= number 500)
        (progn
          (setf final (concatenate 'string final "D"))
          (setf number (- number 500))))
    (if (>= number 400)
        (progn
          (setf final (concatenate 'string final "CD"))
          (setf number (- number 400))))
    (setf number (do ((n number (- n 100)))
                     ((< n 100) n)
                   (setf final (concatenate 'string final "C"))))
    (if (>= number 90)
        (progn
          (setf final (concatenate 'string final "XC"))
          (setf number (- number 90))))
    (if (>= number 50)
        (progn
          (setf final (concatenate 'string final "L"))
          (setf number (- number 50))))
    (if (>= number 40)
        (progn
          (setf final (concatenate 'string final "XL"))
          (setf number (- number 40))))
    (setf number (do ((n number (- n 10)))
                     ((< n 10) n)
                   (setf final (concatenate 'string final "X"))))
    (if (>= number 9)
        (progn
          (setf final (concatenate 'string final "IX"))
          (setf number (- number 9))))
    (if (>= number 5)
        (progn
          (setf final (concatenate 'string final "V"))
          (setf number (- number 5))))
    (if (>= number 4)
        (progn
          (setf final (concatenate 'string final "IV"))
          (setf number (- number 4))))
    (setf number (do ((n number (- n 1)))
                     ((< n 1) n)
                   (setf final (concatenate 'string final "I"))))
  final))

(defun roman-value (ch)
  (case ch
    (#\M 1000)
    (#\D 500)
    (#\C 100)
    (#\L 50)
    (#\X 10)
    (#\V 5)
    (#\I 1)))

(defun roman-to-number (roman)
  (let ((total 0)
        (last 10000))
    (loop for c from 0 below (length roman)
         for ch = (char roman c) do
         (let ((value (roman-value ch)))
           (setf total (+ total
                          (if (< last value)
                              (- value (* 2 last))
                              value)))
           (setf last value)))
    total))
