;;;;
;;;; Routines for graphs
;;;;
;;;; Use adjacency list for graph
;;;;

(defparameter graph-test-data '((a
                                 ((b 2) (c 3)))
                                (b
                                 ((d 3)))
                                (c
                                 ((d 3)))
                                (d
                                 ((nil)))))

(defun nodes (graph)
  "returns a list of all the nodes"
  (let ((nodes))
    (dolist (node graph)
      (push (car node) nodes))
    (reverse nodes)))

(defun node (node graph)
  "Returns a node given a graph"
  (find node graph :key #'car))

(defun shortest-path (start end graph)
  "Returns the shortest path in a graph using Dijkstra's algorithm"
  (let ((dist (make-hash-table))
        (previous (make-hash-table))
        (q (make-hash-table)))
    (dolist (node (nodes graph))
      (setf (gethash node dist) most-positive-fixnum)
      (setf (gethash node previous) nil)
      (setf (gethash node q) t))
    (setf (gethash start dist) 0)
    (print (hash-table-count dist)))
  nil)
