;;;;
;;;; Problem 59
;;;;

(load "read-word-list")

(defun euler059 ()
  (let ((cipher (mapcar #'parse-integer (read-word-list "cipher1.txt"))))
    (do ((a (char-code #\a) (1+ a)))
        ((> a (char-code #\z)))
      (do ((b (char-code #\a) (1+ b)))
          ((> b (char-code #\z)))
        (do ((c (char-code #\a) (1+ c)))
            ((> c (char-code #\z)))
          (if (null (find-if-not #'valid (apply-decrypt cipher a b c)))
              (return-from euler059 (reduce #'+ (apply-decrypt cipher a b c)))))))))

(defun apply-decrypt (cipher a b c)
  (labels ((ad (l n)
             (if (null l)
                 nil
                 (cons
                  (cond
                    ((= (mod n 3) 1) (logxor (car l) a))
                    ((= (mod n 3) 2) (logxor (car l) b))
                    ((= (mod n 3) 0) (logxor (car l) c)))
                  (ad (cdr l) (1+ n))))))
    (ad cipher 1)))

(defun valid (c)
  (cond
    ((= c (char-code #\Rubout)) nil)
    ((= c (char-code #\})) nil)
    ((= c (char-code #\{)) nil)
    ((= c (char-code #\#)) nil)
    ((= c (char-code #\/)) nil)
    ((= c (char-code #\~)) nil)
    ((= c (char-code #\+)) nil)
    (t t)))
