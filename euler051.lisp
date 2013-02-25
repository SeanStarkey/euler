;;;;
;;;; Problem 51
;;;;

;;;
;;; 121313
;;;

(load "prime")

(defparameter *num-digits* 6)
(defparameter *cutoff* 8)

(defun euler051 ()
  (let ((prime-candidates (make-array 0 :fill-pointer t :adjustable t)))
    (do* ((prime-index (prime-start) (1+ prime-index))
          (prime-to-test (prime prime-index) (prime prime-index))
          (digits-to-test (get-digits prime-to-test) (get-digits prime-to-test)))
         ((> prime-to-test (expt 10 *num-digits*)) prime-to-test)
      (let ((repeating-digit (repeat-3? digits-to-test)))
        (if (not (null repeating-digit))
          (if (test-prime digits-to-test repeating-digit)
              (return-from euler051 (create-test-number digits-to-test repeating-digit))))))))

(defun test-prime (prime-digits repeating-digit)
  (let ((test-template (make-array (length prime-digits) :initial-element -1)))
    (loop for x from 0 to (1- (length prime-digits)) do
         (if (/= repeating-digit (elt prime-digits x))
             (setf (elt test-template x) (elt prime-digits x))))
    (let ((count 0))
      (loop for digit from 1 to 9 do
           (let ((test-number (create-test-number test-template digit)))
             (if (primep test-number)
                 (incf count))))
      (if (>= count *cutoff*)
          t
          nil))))

(defun prime-start ()
  (prime-init (+ 1000 (expt 10 *num-digits*)))
  (let ((start (expt 10 (1- *num-digits*)))
        (index 1))
    (loop
         (if (> (prime index) start)
             (return-from prime-start index))
       (incf index))))

(defun repeat-3? (p)
  (labels ((repeat-3-h (p index)
             (if (< (- (length p) index) 3)
                 nil
                 (let ((total 0)
                       (digit (elt p index)))
                   (loop for test-index from index to (1- (length p)) do
                        (if (= digit (elt p test-index))
                            (incf total))
                        (if (= total 3)
                            (return-from repeat-3-h digit)))
                   (repeat-3-h p (1+ index))))))
    (repeat-3-h p 0)))

(defun create-test-number (num digit)
  (let ((result 0))
    (loop for x from 0 to (1- (length num)) do
         (setf result (* 10 result))
         (setf result (+ (if (= (elt num x) -1) digit (elt num x))
                         result)))
    result))

(defun get-digits (p)
  (let ((result (make-array 0 :fill-pointer t :adjustable t))
        (n p))
    (loop
       (multiple-value-bind (num rem) (floor n 10)
         (if (zerop num)
             (progn
               (vector-push-extend rem result)
               (let ((r (make-array 0 :fill-pointer t :adjustable t)))
                 (loop
                    (if (= (length result) 0)
                        (return-from get-digits r))
                    (vector-push-extend (vector-pop result) r))))
             (progn
               (vector-push-extend rem result)
               (setf n num)))))))
