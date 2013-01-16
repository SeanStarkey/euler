;;;;
;;;; Problem 205
;;;;

(defun euler205 ()
  (let ((p-array (make-array 37 :initial-element 0))
        (p-total 0)
        (c-array (make-array 37 :initial-element 0))
        (c-total 0)
        (p-win 0))
        (loop for p1 from 1 to 4 do
             (loop for p2 from 1 to 4 do
                  (loop for p3 from 1 to 4 do
                       (loop for p4 from 1 to 4 do
                            (loop for p5 from 1 to 4 do
                                 (loop for p6 from 1 to 4 do
                                      (loop for p7 from 1 to 4 do
                                           (loop for p8 from 1 to 4 do
                                                (loop for p9 from 1 to 4 do
                                                     (progn
                                                       (incf (aref p-array (+ p1 p2 p3 p4 p5 p6 p7 p8 p9)))
                                                       (incf p-total))
                                                     )))))))))
        (loop for c1 from 1 to 6 do
             (loop for c2 from 1 to 6 do
                  (loop for c3 from 1 to 6 do
                       (loop for c4 from 1 to 6 do
                            (loop for c5 from 1 to 6 do
                                 (loop for c6 from 1 to 6 do
                                      (progn
                                        (incf (aref c-array (+ c1 c2 c3 c4 c5 c6)))
                                        (incf c-total))
                                      ))))))
        (loop for p from 1 to 36 do
             (loop for c from 1 to 36 do
                  (if (> p c)
                      (setf p-win (+ p-win (* (/ (aref p-array p) p-total)
                                              (/ (aref c-array c) c-total)))))))
        (format t "~0,7F~%" (coerce p-win 'double-float))))
