;;;;
;;;; Find the prime factors of any number
;;;;

(load "prime.lisp")

;;;
;;; Return the prime factors as a list
;;;
(defun factors (number)
  (if (or (= number 1) (= number 0))
      (return-from factors nil))
  (let ((factor-list (factors-recursive number 1))
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
                             (factors-recursive (/ number prime-to-check)
                                                prime-iteration)))
               (setq prime-iteration (1+ prime-iteration)))))))

