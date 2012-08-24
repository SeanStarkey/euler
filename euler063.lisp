;;;;
;;;; Problem 63
;;;;

(defun euler063 ()
  (let ((cnt 0))
    (do ((n 1 (1+ n)))
         ((> n 30))
      (do* ((power 1 (1+ power))
           (computed-log (floor (log10 (expt n power))) (floor (log10 (expt n power)))))
          ((/= computed-log (1- power)))
        (if (= power (1+ computed-log))
            (incf cnt))))
    cnt))

(defun log10 (n)
  (/ (log n) (log 10)))
