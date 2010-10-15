;;;;
;;;; Project Euler - Problem 17
;;;;

(defun euler017 ()
  (let ((total 0))
    (do ((x 1 (1+ x)))
        ((> x 1000))
      (setf total (+ total (number-letters (number->words x)))))
    total))

;;;
;;; Returns the number of letters in the words (doesn't count spaces)
;;;
(defun number-letters (words)
  (let ((total 0))
    (dotimes (x (length words))
      (unless (eql (elt words x) #\ )
        (setf total (1+ total))))
    total))

;;;
;;; converts a number to words
;;;
(defun number->words (number)
  (let ((thousands-words nil)
        (hundreds-words nil)
        (and-word nil)
        (tens-words nil)
        (units-words nil))
    (multiple-value-bind (thousands hundreds tens units) (split-number number)
      (if (> thousands 0)
          (setf thousands-words (concatenate 'string
                                             (units-number->words thousands)
                                             " thousand ")))
      (if (> hundreds 0)
          (setf hundreds-words (concatenate 'string
                                            (units-number->words hundreds)
                                            " hundred ")))
      (setf units-words (units-number->words units))
      (setf tens-words (tens-number->words tens))
      (when (= tens 1)
          (setf tens-words (special-tens-number->words units))
          (setf units-words nil))
      (if (and (or (= 1 thousands)
                   (> hundreds 0))
               (or (> tens 0)
                   (> units 0)))
          (setf and-word "and "))
      (concatenate 'string thousands-words hundreds-words and-word
                   tens-words units-words)
           )))

;;;
;;; Splits a number into thousands, hundreds, tens and units
;;;
(defun split-number (number)
  (multiple-value-bind (higher1 units) (floor number 10)
    (multiple-value-bind (higher2 tens) (floor higher1 10)
      (multiple-value-bind (thousands hundreds) (floor higher2 10)
        (values thousands hundreds tens units)))))

(defun tens-number->words (number)
  (case number
    (2 "twenty ")
    (3 "thirty ")
    (4 "forty ")
    (5 "fifty ")
    (6 "sixty ")
    (7 "seventy ")
    (8 "eighty ")
    (9 "ninety ")))

(defun special-tens-number->words (number)
  (case number
    (0 "ten")
    (1 "eleven")
    (2 "twelve")
    (3 "thirteen")
    (4 "fourteen")
    (5 "fifteen")
    (6 "sixteen")
    (7 "seventeen")
    (8 "eighteen")
    (9 "nineteen")))

(defun units-number->words (number)
  (case number
    (1 "one")
    (2 "two")
    (3 "three")
    (4 "four")
    (5 "five")
    (6 "six")
    (7 "seven")
    (8 "eight")
    (9 "nine")))
