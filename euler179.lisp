;;;;
;;;; Project Euler - Problem 179
;;;;

(load "prime.lisp")

(defparameter *end* (expt 10 7))
(defparameter *array* (make-array (+ *end* 1)
                                  :initial-element 0
                                  :element-type 'integer))

(defun number-divisors (factor-list)
  (let ((total 1)
        (combined-factor-list nil))
    (dolist (single-factor factor-list)
      (if (null combined-factor-list)
          (setf combined-factor-list (list (list single-factor 1)))
          (if (= (caar (last combined-factor-list)) single-factor)
              (setf (cadar (last combined-factor-list))
                    (1+ (cadar (last combined-factor-list))))
              (setf combined-factor-list
                    (append combined-factor-list
                            (list (list single-factor 1)))))))
    (dolist (factor combined-factor-list)
      (setf total (* total (1+ (cadr factor)))))
    total))

(defun add-prime (factor-list new)
  (append factor-list (list new)))

(defun multiply-prime (cur-level factors prev-prod start-index)
  (if (= cur-level 1)
      (do* ((i start-index (1+ i))
            (this-prod (* prev-prod (prime i)) (* prev-prod (prime i))))
           ((> this-prod *end*))
        (setf (svref *array* this-prod)
              (number-divisors (add-prime factors (prime i)))))
      (do* ((i start-index (1+ i))
            (this-prod (* prev-prod (prime i)) (* prev-prod (prime i))))
           ((> this-prod *end*))
        (multiply-prime (1- cur-level) (add-prime factors (prime i))
                            (* prev-prod (prime i)) i))))

(defun euler179 ()
  (prime-init (+ *end* 50))

  (loop for i from 1 to 23 do
       (multiply-prime i nil 1 1))

  (let ((total 0))
    (loop for i from 2 to (- *end* 1) do
         (if (> (svref *array* i) 0)
             (if (= (svref *array* i) (svref *array* (1+ i)))
                 (incf total))))
    total))

