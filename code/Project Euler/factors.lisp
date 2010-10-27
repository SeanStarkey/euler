;;;;
;;;; Find the prime factors of any number
;;;;

(load "prime.lisp")
(load "memoize.lisp")

;;;
;;; Return the divisors as a list
;;;
(defun divisors (number)
  (butlast (sort (funcall divisors-helper-memoized (factors number) 1) #'<)))

(defun divisors-non-sorted (number)
  (butlast (funcall divisors-helper-memoized (factors number) 1)))

(defvar divisors-helper-memoized (memoize #'divisors-helper))

(defun divisors-helper (lst product)
  (let ((return-list nil))
    (if (null lst)
        (setf return-list (append return-list (list product)))
        (dotimes (number-multiplied (1+ (cadar lst)))
          (setf return-list
                (append return-list
                        (funcall divisors-helper-memoized (cdr lst)
                                 (* product
                                    (expt (caar lst)
                                          number-multiplied)))))))
    return-list))

;;;
;;; Return the prime factors as a list
;;;
(defun factors (number)
  (if (or (= number 1) (= number 0))
      (return-from factors nil))
  (let ((factor-list (funcall factors-recursive-memoized number 1))
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
    combined-factor-list))

;;;
;;; Initialize the factors engine
;;;
(defun factors-init (number)
  (prime-init (+ 100 (isqrt number))))

(defvar factors-recursive-memoized (memoize #'factors-recursive))

;;;
;;; Recursively determine the next factor
;;;
(defun factors-recursive (number prime-iteration)
  (if (primep number)
      (list number)
      (loop
         (let ((prime-to-check (prime prime-iteration)))
           (if (eql (mod number prime-to-check) 0)
               (return (cons prime-to-check
                             (funcall factors-recursive-memoized
                                      (/ number prime-to-check)
                                      prime-iteration)))
               (setq prime-iteration (1+ prime-iteration)))))))

