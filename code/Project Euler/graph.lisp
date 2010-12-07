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
(defun test () (cost (shortest-path 'a 'g graph-test-data) graph-test-data))

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
  (let ((dist (make-hash-table))
        (previous (make-hash-table))
        (q (make-hash-table)))
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
