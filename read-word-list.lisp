(load "split-string")

(defun remove-quotes (string)
  (subseq string 1 (1- (length string))))

(defun read-word-list (filename)
  (with-open-file (stream (probe-file filename))
                  (split-string (read-line stream nil) #\,)))
