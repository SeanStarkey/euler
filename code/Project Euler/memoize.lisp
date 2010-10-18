;;;;
;;;; Setup memoize functionality
;;;;
;;;; Usage:
;;;;   Define closure first:
;;;;     (defvar f-memoized (memoize #'f))
;;;;
;;;;   Define function that uses memoized call:
;;;;     (defun f (x)
;;;;        stuff
;;;;        (funcall f-memoized params))
;;;;
(defun memoize (fn)
  (let ((cache (make-hash-table :test #'equal)))
    #'(lambda (&rest args)
        (multiple-value-bind
              (result exists)
            (gethash args cache)
          (if exists
              result
              (setf (gethash args cache)
                    (apply fn args)))))))
