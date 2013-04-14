;;;;
;;;; Project Euler - Problem 61
;;;;

(load "figurate")
(load "permutations")

(defun euler061 ()
  (dolist (candidate (get-candidates))
    (if (find-cycle candidate)
        (return (reduce #'+ candidate)))))

(defun find-cycle (l)
  (let* ((digits (map 'list #'number-to-list l))
         (first (car digits))
         (last (remove-cycled first (cdr digits))))
    (if (null last)
        nil
        (if (and (= (nth 2 last) (nth 0 first))
                 (= (nth 3 last) (nth 1 first)))
            l))))

(defun remove-cycled (n l)
  (if (= (length l) 0)
      n
      (dolist (c l)
        (progn
          (if (and (= (nth 2 n) (nth 0 c))
                   (= (nth 3 n) (nth 1 c)))
              (return (remove-cycled c (remove c l)))))
        nil)))

(defun get-candidates ()
  (let* ((return-list nil)
         (low-3 (find-low #'triangle))
        (high-3 (find-high #'triangle low-3))
        (low-4 (find-low #'square))
        (high-4 (find-high #'square low-4))
        (low-5 (find-low #'pentagonal))
        (high-5 (find-high #'pentagonal low-5))
        (low-6 (find-low #'hexagonal))
        (high-6 (find-high #'hexagonal low-6))
        (low-7 (find-low #'heptagonal))
        (high-7 (find-high #'heptagonal low-7))
        (low-8 (find-low #'octagonal))
        (high-8 (find-high #'octagonal low-8)))
    (loop for i-8 from low-8 to high-8 do
         (let ((n-8 (octagonal i-8)))
           (loop for i-7 from low-7 to high-7 do
                (let ((n-7 (heptagonal i-7)))
                  (if (cyclic-either? n-7 n-8)
                      (loop for i-6 from low-6 to high-6 do
                           (let ((n-6 (hexagonal i-6)))
                             (if (or (cyclic-either? n-6 n-7)
                                     (cyclic-either? n-6 n-8))
                                 (loop for i-5 from low-5 to high-5 do
                                      (let ((n-5 (pentagonal i-5)))
                                        (if (or (cyclic-either? n-5 n-6)
                                                (cyclic-either? n-5 n-7)
                                                (cyclic-either? n-5 n-8))
                                            (loop for i-4 from low-4 to high-4 do
                                                 (let ((n-4 (square i-4)))
                                                   (if (or (cyclic-either? n-4 n-5)
                                                           (cyclic-either? n-4 n-6)
                                                           (cyclic-either? n-4 n-7)
                                                           (cyclic-either? n-4 n-8))
                                                       (loop for i-3 from low-3 to high-3 do
                                                            (let ((n-3 (triangle i-3)))
                                                              (if (or (cyclic-either? n-3 n-4)
                                                                      (cyclic-either? n-3 n-5)
                                                                      (cyclic-either? n-3 n-6)
                                                                      (cyclic-either? n-3 n-7)
                                                                      (cyclic-either? n-3 n-8))
                                                                  (push (list n-3 n-4 n-5 n-6 n-7 n-8) return-list)
                                                       )))))))))))))))))
    return-list))

(defun find-low (fn)
  (let ((n 1))
    (loop do
         (if (>= (funcall fn n) 1000)
             (return n))
         (incf n))))

(defun find-high (fn n)
  (let ((m n))
    (loop do
         (if (>= (funcall fn m) 10000)
             (return (1- m)))
         (incf m))))

(defun cyclic-either? (n m)
  (let ((n-list (number-to-list n))
        (m-list (number-to-list m)))
    (or (and (= (nth 2 n-list) (nth 0 m-list))
             (= (nth 3 n-list) (nth 1 m-list)))
        (and (= (nth 2 m-list) (nth 0 n-list))
             (= (nth 3 m-list) (nth 1 n-list))))))
