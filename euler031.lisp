;;;;
;;;; Project Euler - Problem 31
;;;;

(defun euler031 ()
  (let ((2p 0)
        (5p 0)
        (10p 0)
        (20p 0)
        (50p 0)
        (100p 0)
        (200p 0)
        (count 0))
    (loop
       (if (<= (evaluate 2p 5p 10p 20p 50p 100p 200p) 200)
           (progn
             (incf count)
             (incf 2p))
           (progn
             (if (not (zerop 2p))
                 (progn
                   (incf 5p)
                   (setf 2p 0))
                 (if (not (zerop 5p))
                     (progn
                       (incf 10p)
                       (setf 2p 0)
                       (setf 5p 0))
                     (if (not (zerop 10p))
                         (progn
                           (incf 20p)
                           (setf 2p 0)
                           (setf 5p 0)
                           (setf 10p 0))
                         (if (not (zerop 20p))
                             (progn
                               (incf 50p)
                               (setf 2p 0)
                               (setf 5p 0)
                               (setf 10p 0)
                               (setf 20p 0))
                             (if (not (zerop 50p))
                                 (progn
                                   (incf 100p)
                                   (setf 2p 0)
                                   (setf 5p 0)
                                   (setf 10p 0)
                                   (setf 20p 0)
                                   (setf 50p 0))
                                 (if (not (zerop 100p))
                                     (progn
                                       (incf 200p)
                                       (setf 2p 0)
                                       (setf 5p 0)
                                       (setf 10p 0)
                                       (setf 20p 0)
                                       (setf 50p 0)
                                       (setf 100p 0))
                                     (if (not (zerop 200p))
                                         (return count)))))))))))))

(defun evaluate (2p 5p 10p 20p 50p 100p 200p)
  (+ (* 2 2p)
     (* 5 5p)
     (* 10 10p)
     (* 20 20p)
     (* 50 50p)
     (* 100 100p)
     (* 200 200p)))