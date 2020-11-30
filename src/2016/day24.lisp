(defpackage :aoc/2016/24 #.cl-user::*aoc-use*)
(in-package :aoc/2016/24)

(defstruct blueprint
  (walls (make-hash-table))
  sites)

(defun char- (a b) (- (char-code a) (char-code b)))

(defun read-blueprint (data)
  (let ((blueprint (make-blueprint))
        (points-of-interest (make-hash-table)))
    (with-slots (walls sites) blueprint
      (loop for row from 0 for string in data do
            (loop for column from 0 for ch across string
                  for pos = (complex column row) do
                  (cond
                    ((char= ch #\#) (setf (gethash pos walls) t))
                    ((char= ch #\.) nil)
                    (t (setf (gethash pos points-of-interest) (char- ch #\0))))))
      (setf sites (make-array (hash-table-count points-of-interest)))
      (maphash #'(lambda (pos poi) (setf (aref sites poi) pos)) points-of-interest))
    blueprint))

(defun find-path (walls from to)
  (nth-value 1 (a* from :goal-state to
                   :neighbors (lambda (p)
                                (loop for n in (adjacents p)
                                      unless (gethash n walls)
                                      collect (cons n 1)))
                   :heuristic (partial-1 #'manhattan-distance to))))

(defun precompute-paths (blueprint)
  (with-slots (walls sites) blueprint
    (let* ((n (length sites))
           (paths (make-array (list n n))))
      (loop for i from 0 below n do
            (loop for j from i below n
                  for cost = (find-path walls (aref sites i) (aref sites j)) do
                  (setf (aref paths i j) cost
                        (aref paths j i) cost)))
      paths)))

(defstruct (state (:conc-name)) robot visited)

(defun neighbors (paths site-count state)
  (with-slots (robot visited) state
    (loop for j below site-count
          for cost = (aref paths robot j)
          unless (logbitp j visited)
          collect (cons (make-state :robot j
                                    :visited (logior visited (ash 1 j)))
                        cost))))

(defun visit-sites (blueprint)
  (let* ((init-state (make-state :robot 0 :visited 1))
         (site-count (length (blueprint-sites blueprint)))
         (all-sites (1- (ash 1 site-count)))
         (paths (precompute-paths blueprint)))
    (nth-value 1 (a* init-state :goalp (partial-1 #'= (visited _) all-sites)
                     :neighbors (partial-1 #'neighbors paths site-count)))))

(defun visit-sites-part2 (blueprint)
  (let* ((init-state (make-state :robot 0 :visited 1))
         (site-count (length (blueprint-sites blueprint)))
         (all-sites (1- (ash 1 site-count)))
         (paths (precompute-paths blueprint)))
    (multiple-value-bind (end-state end-state-cost)
        (a* init-state :goalp (partial-1 #'= (visited _) all-sites)
            :neighbors (partial-1 #'neighbors paths site-count)
            :heuristic #'(lambda (s)
                          (aref paths (robot s) 0)))
      (+ end-state-cost (aref paths (robot end-state) 0)))))

(define-problem (2016 24) (blueprint read-blueprint)
  (values (visit-sites blueprint)
          (visit-sites-part2 blueprint)))

(1am:test test-2016/24
  (multiple-value-bind (part1 part2) (problem-run)
    (1am:is (= 430 part1))
    (1am:is (= 700 part2))))
