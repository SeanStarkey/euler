;;;;
;;;; Project Euler - problem 19
;;;;

(defstruct date
  dayofweek ; 0 = Sunday
  year
  month
  day)

(defun euler019 ()
  (let ((end (make-date :dayofweek 0 :year 2000 :month 12 :day 31))
        (current (make-date :dayofweek 2 :year 1901 :month 1 :day 1))
        (number-of-sundays 0))
    (loop
     (if (and (eql (date-dayofweek current) 0)
              (eql (date-day current) 1))
         (setf number-of-sundays (1+ number-of-sundays)))
     (when (and (eql (date-year current) (date-year end))
                (eql (date-month current) (date-month end))
                (eql (date-day current) (date-day end)))
       (return))
     (setf current (next-date current)))
    number-of-sundays))

(defun next-date (current)
  (setf (date-dayofweek current) (mod (1+ (date-dayofweek current)) 7))
  (if (eql (date-day current) (days-in-month (date-month current)
                                             (date-year current)))
      (progn
        (setf (date-day current) 1)
        (if (eql (date-month current) 12)
            (progn
              (setf (date-month current) 1)
              (setf (date-year current) (1+ (date-year current))))
            (setf (date-month current) (1+ (date-month current)))))
      (setf (date-day current) (1+ (date-day current))))
  current)

(defun days-in-month (month year)
  (case month
    ((1 3 5 7 8 10 12) 31)
    ((4 6 9 11) 30)
    ((2) (if (leap-year? year) 29 28))
    (otherwise 0)
    ))

(defun leap-year? (year)
  (if (eql year 1900)
      nil
      (if (eql (mod year 4) 0)
          t
          nil)))

