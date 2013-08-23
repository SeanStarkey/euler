;;;;
;;;; Routines for graphs
;;;;
;;;; Use adjacency list for graph
;;;;

(load "split-string")

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
;(defun test () (load-triangle-file "small.txt"))
(defun test () (shortest-path 'start 'end (load-triangle-file "triangle_small.txt")))
;(defun test () (longest-path 'a 'g graph-test-data))
(defparameter *max-value* 10000)

(defun nodes (graph)
  "returns a list of all the nodes"
  (let ((nodes))
    (dolist (node graph)
      (push (car node) nodes))
    (reverse nodes)))

(defun node (node graph)
  "Returns a node given a graph"
  (find node graph :key #'car :test #'equal))

(defun vertices (node graph)
  (cadr (node node graph)))

(defun cost (path graph)
  "Returns the cost of a path"
  (let ((sum 0)
        (previous nil))
    (dolist (node path)
      (if (null previous)
          (setf previous node)
            (setf sum (+ sum (vertex-cost previous node graph))
                  previous node)))
    sum))

(defun cost-highest (path graph)
  (let ((sum 0)
        (previous nil))
    (dolist (node path)
      (if (null previous)
          (setf previous node)
            (setf sum (+ sum (- *max-value* (vertex-cost previous node graph)))
                  previous node)))
    (- sum *max-value*)))

(defun vertex-cost (start end graph)
  "Returns the cost of one vertex"
  (cadr (find end (cadr (node start graph)) :key #'car :test #'equal)))

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
      (loop while (not (equal u start)) do
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
  (create-graph filename 'triangle))

(defun load-matrix-corner-file (filename)
  (create-graph filename 'matrix-corner))

(defun load-matrix-side-file (filename)
  (create-graph filename 'matrix-side))

(defun load-matrix-full-file (filename)
  (create-graph filename 'matrix-full))

(defun create-graph (filename type)
  (let ((graph nil)
        (row-lst nil))
    (with-open-file (stream (probe-file filename))
      (do ((index 0 (1+ index))
           (line (read-line stream)
                 (read-line stream nil 'eof)))
          ((eq line 'eof))
        (push (index-nodes index (split-string-numbers line (get-delim type)))
              row-lst)))
    (setf row-lst (reverse row-lst))
    (setf graph (case type
                  ('triangle (set-vertices-triangle row-lst graph))
                  ('matrix-corner (set-vertices-matrix-corner row-lst graph))
                  ('matrix-side (set-vertices-matrix-side row-lst graph))
                  ('matrix-full (set-vertices-matrix-full row-lst graph))))
    (push '(end (nil)) graph)
    (setf graph (reverse graph))
    graph))

(defun set-vertices-triangle (row-lst graph)
  (push (list 'start (list (list '(0 0) (get-node-cost-max 0 0 row-lst)))) graph)
  (do ((row 0 (1+ row)))
      ((>= row (length row-lst)))
    (do ((col 0 (1+ col)))
        ((>= col (length (nth row row-lst))))
      (let ((connect nil))
        (if (and (/= row (1+ col))
                 (/= (1+ row) (length row-lst)))
            (push (list (list (1+ row) (1+ col))
                        (get-node-cost-max (1+ row) (1+ col) row-lst)) connect))
        (if (/= (1+ row) (length row-lst))
            (push (list (list (1+ row) col)
                        (get-node-cost-max (1+ row) col row-lst)) connect))
        (if (= (1+ row) (length row-lst))
            (push (list 'end 0) connect))
        (setf connect (list (list row col) connect))
        (push connect graph))))
  graph)

(defun set-vertices-matrix-corner (row-lst graph)
  (push (list 'start (list (list '(0 0) (get-node-cost 0 0 row-lst)))) graph)
  (do ((row 0 (1+ row)))
      ((>= row (length row-lst)))
    (do ((col 0 (1+ col)))
        ((>= col (length (nth row row-lst))))
      (let ((connect nil))
        (if (/= (1+ col) (length row-lst))
            (push (list (list row (1+ col))
                        (get-node-cost row (1+ col) row-lst)) connect))
        (if (/= (1+ row) (length row-lst))
            (push (list (list (1+ row) col)
                        (get-node-cost (1+ row) col row-lst)) connect))
        (setf connect (list (list row col) connect))
        (push connect graph))))
  graph)

(defun set-vertices-matrix-side (row-lst graph)
  (let ((start-list nil))
    (do ((col 0 (1+ col)))
        ((>= col (length row-lst)))
      (push (list (list 0 col) (get-node-cost col 0 row-lst)) start-list))
    (push (list 'start start-list) graph))
  (do ((row 0 (1+ row)))
      ((>= row (length row-lst)))
    (do ((col 0 (1+ col)))
        ((>= col (length (nth row row-lst))))
      (let ((connect nil))
        (if (/= (1+ col) (length row-lst))
            (push (list (list (1+ col) row)
                        (get-node-cost row (1+ col) row-lst)) connect))
        (if (/= (1+ row) (length row-lst))
            (push (list (list col (1+ row))
                        (get-node-cost (1+ row) col row-lst)) connect))
        (if (/= row 0)
            (push (list (list col (1- row))
                        (get-node-cost (1- row) col row-lst)) connect))
        (if (= (1+ col) (length row-lst))
            (push (list 'end 0) connect))
        (setf connect (list (list col row) connect))
        (push connect graph))))
  graph)

(defun set-vertices-matrix-full (row-lst graph)
  (push (list 'start (list (list '(0 0) (get-node-cost 0 0 row-lst)))) graph)
  (do ((row 0 (1+ row)))
      ((>= row (length row-lst)))
    (do ((col 0 (1+ col)))
        ((>= col (length (nth row row-lst))))
      (let ((connect nil))
        (if (/= (1+ col) (length row-lst))
            (push (list (list (1+ col) row)
                        (get-node-cost row (1+ col) row-lst)) connect))
        (if (/= (1+ row) (length row-lst))
            (push (list (list col (1+ row))
                        (get-node-cost (1+ row) col row-lst)) connect))
        (if (/= row 0)
            (push (list (list col (1- row))
                        (get-node-cost (1- row) col row-lst)) connect))
        (if (/= col 0)
            (push (list (list (1- col) row)
                        (get-node-cost row (1- col) row-lst)) connect))
        (if (and (= (1+ row) (length row-lst))
                 (= (1+ col) (length row-lst)))
            (push (list 'end 0) connect))
        (setf connect (list (list col row) connect))
        (push connect graph))))
  graph)

(defun get-delim (type)
  (case type
    ('triangle #\Space)
    ('matrix-corner #\,)
    ('matrix-side #\,)
    ('matrix-full #\,)))

(defun get-node-cost-max (row col node-lst)
  (- *max-value* (cadr (get-node row col node-lst))))

(defun get-node-cost (row col node-lst)
  (cadr (get-node row col node-lst)))

(defun get-node (row col node-lst)
  (find (list row col) (nth row node-lst) :test #'equal :key #'car))

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
