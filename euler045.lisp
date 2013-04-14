;;;;
;;;; Project Euler - Problem 45
;;;;

(load "figurate")

(defun euler045 ()
  (let* ((triangle-count 286)
         (triangle-eval (triangle triangle-count))
         (pentagonal-count 165)
         (pentagonal-eval (pentagonal pentagonal-count))
         (hexagonal-count 143)
         (hexagonal-eval (hexagonal hexagonal-count)))
    (loop
       (when (= triangle-eval pentagonal-eval hexagonal-eval)
         (return triangle-eval))
       (if (and (< triangle-eval pentagonal-eval)
                (< triangle-eval hexagonal-eval))
           (progn
             (incf triangle-count)
             (setf triangle-eval (triangle triangle-count)))
           (if (and (<= pentagonal-eval triangle-eval)
                    (< pentagonal-eval hexagonal-eval))
               (progn
                 (incf pentagonal-count)
                 (setf pentagonal-eval (pentagonal pentagonal-count)))
               (progn
                 (incf hexagonal-count)
                 (setf hexagonal-eval (hexagonal hexagonal-count))))))))
