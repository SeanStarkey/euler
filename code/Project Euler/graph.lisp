;;;;
;;;; Routines for graphs
;;;;
;;;; Use adjacency list for graph
;;;;

(defparameter graph-test-data '((a
                                 ((b 2) (c 3)))
                                (b
                                 ((d 3) (e 6)))
                                (c
                                 ((d 3)))
                                (d
                                 ((e 3) (f 2)))
                                (e
                                 ((g 3)))
                                (f
                                 ((g 3)))
                                (g ((nil)))))
;(defun test () (cost (shortest-path 'a 'g graph-test-data) graph-test-data))
(defun test () (length (load-triangle-file "small.txt")))
;(defun test () (shortest-path '(0 0) 'end (load-triangle-file "small.txt")))

(defun nodes (graph)
  "returns a list of all the nodes"
  (let ((nodes))
    (dolist (node graph)
      (push (car node) nodes))
    (reverse nodes)))

(defun node (node graph)
  "Returns a node given a graph"
  (find node graph :key #'car))

(defun vertices (node graph)
  (cadr (node node graph)))

(defun cost (path graph)
  "Returns the cost of a path"
  (print path)
  (let ((sum 0)
        (previous nil))
    (dolist (node path)
      (if (null previous)
          (setf previous node)
            (setf sum (+ sum (vertex-cost previous node graph))
                  previous node)))
    sum))

(defun vertex-cost (start end graph)
  "Returns the cost of one vertex"
  (cadr (find end (cadr (node start graph)) :key #'car)))

;;
;; See wikipedia article for pseudocode
;; http://en.wikipedia.org/wiki/Dijkstra's_algorithm
;;
(defun shortest-path (start end graph)
  "Returns the shortest path in a graph using Dijkstra's algorithm"
  (let ((dist (make-hash-table :test #'equal))
        (previous (make-hash-table :test #'equal))
        (q (make-hash-table :test #'equal)))
    ;; Initialize variables
    (dolist (node (nodes graph))
      (setf (gethash node dist) most-positive-fixnum)
      (setf (gethash node previous) nil)
      (setf (gethash node q) t))
    (setf (gethash start dist) 0)
    (shortest-path-loop start end graph dist previous q)
    (let ((path)
          (u end))
      (loop while (not (eql u start)) do
            (push u path)
            (setf u (gethash u previous)))
      (push start path)
      path)))

(defun shortest-path-loop (start end graph dist previous q)
  (loop while (/= 0 (hash-table-count q)) do
        (let ((u (get-smallest-dist q dist)))
          (if (eq (gethash u dist) most-positive-fixnum)
              (return-from shortest-path-loop))
          (if (eq u end)
              (return-from shortest-path-loop))
          (remhash u q)
          (dolist (v (vertices u graph))
            (let ((alt (+ (gethash u dist) (cadr v))))
              (if (< alt (gethash (car v) dist))
                  (setf (gethash (car v) dist) alt
                        (gethash (car v) previous) u)))))))

;;;
;;; Gets the smallest computed distance of nodes in q
;;;
(defun get-smallest-dist (q dist)
  (let ((shortest-dist most-positive-fixnum)
        (u nil))
    (maphash #'(lambda (k v)
                 (if (< (gethash k dist) shortest-dist)
                     (setf shortest-dist (gethash k dist)
                           u k))) q)
    u))


(defun load-triangle-file (filename)
  (let ((graph nil)
        (node-lst nil))
    (with-open-file (stream (probe-file filename))
                    (do ((index 1 (1+ index))
                         (line (read-line stream)
                               (read-line stream nil 'eof)))
                        ((eq line 'eof))
                      (push (index-nodes index (split-string line #\Space))
                            node-lst)))
    (setf node-lst (reverse node-lst))
    (do ((row 1 (1+ row)))
        ((> row (length node-lst)))
      (format t "~%--- row ~d ---" row)
      (print (nth (1- row) node-lst))
      (format t "~%----------")
      (do ((col 0 (1+ col)))
          ((eq col (length (nth (1- row) node-lst))))
        (let ((connect nil))
        ;(print (get-node row col node-lst))
        (format t "~%C: (~d ~d) to " (1- row) col)
        (format t "(~d ~d)[~d]" row col (get-node-cost row col node-lst))
        (if (/= row (1+ col))
            (format t " and (~d ~d)[~d]" row (1+ col)
                    (get-node-cost row (1+ col) node-lst)))
        (if (/= row (1+ col))
            (push (list (list row (1+ col))
                        (get-node-cost row (1+ col) node-lst)) connect))
        (push (list (list row col)
                    (get-node-cost row col node-lst)) connect)
        (setf connect (list (list (1- row) col) connect))
        (push connect graph))))
    (format t "~%--------")
    (do ((col 0 (1+ col)))
        ((eql col (length node-lst)))
      (push (list (list (length node-lst) col) (list (list 'end 0))) graph)
      (format t "~%C: (~d ~d) to end" (length node-lst) col))
    (setf graph (reverse graph))
    graph))

(defun get-node-cost (row col node-lst)
  (cadr (get-node row col node-lst)))

(defun get-node (row col node-lst)
  ;(format t "~%r=~d c=~d " row col)
  ;(print (nth row node-lst))
  (find (list row col) (nth (1- row) node-lst) :test #'equal :key #'car))

;;;
;;; Return a list of the nodes indexed by location
;;;
(defun index-nodes (index lst)
  (let ((return-lst nil)
        (index2 0))
    (dolist (cost lst)
      (push (list (list index index2) cost) return-lst)
      (setf index2 (1+ index2)))
    (reverse return-lst)))

;;;
;;; Used by load-triangle-file
;;;
(defun split-string (string delimeter)
  (let ((number-list nil))
    (do* ((pos 0 pos)
          (next-pos (position delimeter string)
                    (position delimeter string :start pos)))
        ((eq next-pos nil) (push (parse-integer (subseq string pos))
                                 number-list))
      (push (parse-integer (subseq string pos next-pos)) number-list)
      (setf pos (1+ next-pos)))
    (reverse number-list)))
