(defpackage :aoc/2016/22 #.cl-user::*aoc-use*)
(in-package :aoc/2016/22)

(defun pos (n) (nth 0 n))
(defun size (n) (nth 1 n))
(defun used (n) (nth 2 n))
(defun avail (n) (nth 3 n))

(defun parse-node (string)
  (cl-ppcre:register-groups-bind ((#'parse-integer c r size used avail))
      ("node-x(\\d+)-y(\\d+)\\s+(\\d+)T\\s+(\\d+)T\\s+(\\d+)" string)
    (list (complex c r) size used avail)))

(defun parse-nodes (data)
  (remove nil (mapcar #'parse-node data)))

(defun viable-pair-p (n1 n2)
  (and (plusp (used n1))
       (not (eq n1 n2))
       (< (used n1) (avail n2))))

(defun viable-pairs (nodes)
  (loop for n1 in nodes
        append (loop for n2 in nodes
                     when (viable-pair-p n1 n2)
                     collect (list n1 n2))))

(defun find-target-node (nodes)
  (let (c-max max)
    (dolist (n nodes max)
      (with-complex-parts (c r) (pos n)
        (when (and (zerop r)
                   (or (null c-max)
                       (> c c-max)))
          (setf c-max c
                max n))))))

(defun find-empty-node (nodes)
  (find 0 nodes :key #'used))

(defun interchangeable-nodes (nodes)
  (loop with empty = (find-empty-node nodes)
        for n in nodes
        when (and (<= (used n) (size empty)))
        collect n))

(defstruct (state (:conc-name)) cur empty)

(defun cost-to-make-space (grid fixed from to)
  (nth-value 1 (a* from
                   :goal-state to
                   :neighbors (search-unit-cost (lambda (pos)
                                                  (loop for n in (adjacents pos)
                                                        when (and (gethash pos grid)
                                                                  (/= pos fixed))
                                                        collect n)))
                   :heuristic (partial-1 #'manhattan-distance to))))

(defun neighbors (state grid)
  (with-slots (cur empty) state
    (loop for next in (adjacents cur)
          for cost = (and (gethash next grid)
                          (cost-to-make-space grid cur empty next))
          when cost collect (cons (make-state :cur next
                                              :empty cur)
                                  (1+ cost)))))

(defun move-data (nodes)
  (let* ((intechangeables (interchangeable-nodes nodes))
         (grid (list-hash-table intechangeables #'pos))
         (init-state (make-state :cur (pos (find-target-node intechangeables))
                                 :empty (pos (find-empty-node intechangeables)))))
    (nth-value 1 (a* init-state
                     :goalp (partial-1 #'= (cur _) 0)
                     :neighbors (partial-1 #'neighbors _ grid)
                     :heuristic (partial-1 #'manhattan-distance (cur _) 0)
                     :test 'equalp))))

(define-solution (2016 22) (nodes parse-nodes)
  (values (length (viable-pairs nodes))
          (move-data nodes)))

(define-test (2016 22) (1024 230))
