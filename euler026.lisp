;;;;
;;;; Project Euler - Problem 26
;;;;

(defun euler026 ()
  (let ((fraction-list nil)
        (longest-denominator))
    (dotimes (n 1000)
      (if (> n 1)
          (let ((new-fraction (make-instance 'fraction :denominator n)))
            (push new-fraction fraction-list))))
    (let ((longest-value 0))
      (dolist (fraction fraction-list)
        (let ((repeat-length (length (slot-value fraction 'repeat))))
          (if (> repeat-length longest-value)
              (progn
                (setf longest-value repeat-length)
                (setf longest-denominator (slot-value fraction 'denominator)))))))
    longest-denominator))

(defclass fraction ()
  ((denominator
    :initarg :denominator
    :initform (error "Must supply a denominator"))
   (numerators
    :initform (make-array 0 :fill-pointer 0 :adjustable t))
   (non-repeat
    :initform (make-array 0 :fill-pointer 0 :adjustable t))
   (repeat
    :initform (make-array 0 :fill-pointer 0 :adjustable t))))

(defmethod initialize-instance :after ((fraction fraction) &key)
  (let ((numerator 1))
    (do ((x 0 (1+ x)))
        ((or
          (detect-repeat fraction numerator)
          (= numerator 0)))
      (vector-push-extend numerator (slot-value fraction 'numerators))
      (setf numerator (* numerator 10))
      (multiple-value-bind (new-digit new-numerator) (floor numerator (slot-value fraction 'denominator))
        (setf numerator new-numerator)
        (vector-push-extend new-digit (slot-value fraction 'non-repeat))))))

(defun detect-repeat (fraction numerator)
  (let ((start-of-repeat (position numerator (slot-value fraction 'numerators))))
    (if (null start-of-repeat)
        nil
        (progn
          (setf (slot-value fraction 'repeat) (subseq (slot-value fraction 'non-repeat) start-of-repeat))
          (setf (slot-value fraction 'non-repeat) (subseq (slot-value fraction 'non-repeat) 0 start-of-repeat))))))
