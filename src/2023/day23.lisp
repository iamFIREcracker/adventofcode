(defpackage :aoc/2023/23 #.cl-user::*aoc-use*)
(in-package :aoc/2023/23)


(defparameter *north* '(-1 0))
(defparameter *east*  '(0 1))
(defparameter *south* '(1 0))
(defparameter *west*  '(0 -1))

(defun move-straight (pos dir) (mapcar #'+ pos dir))
(defun rotate-cw (dir) (list (second dir) (- (first dir))))
(defun rotate-ccw (dir) (list (- (second dir)) (first dir)))

(defun parse-map (&optional (strings (uiop:read-file-lines #P"src/2023/day23.txt")))
  (bnd* ((rows (length strings))
         (cols (length (first strings)))
         (map (make-hash-table :test 'equal))
         (start))
    (doseq ((i s) (enumerate strings))
      (doseq ((j ch) (enumerate s))
        (when (find ch ".^>v<")
          (setf (gethash (list i j) map) ch))))
    (list map (list 0 1) (list (1- rows) (- cols 2)))))
#+#:excluded (parse-map)

(defun part1 (&optional (input (parse-map)))
  (destructuring-bind (map start end) input
    (bnd1 (longest 0)
      (labels ((recur (pos path)
                 (cond ((gethash pos path) nil)
                       ((equal pos end)
                        (setf longest (max (1+ (hash-table-count path)) longest)))
                       ((find (gethash pos map) "^>v<")
                        (setf (gethash pos path) t)
                        (bnd1 (dir (case (gethash pos map)
                                     (#\> *east*)
                                     (#\v *south*)
                                     (#\< *east*)
                                     (#\^ *north*)))
                          (recur (move-straight pos dir) path))
                        (remhash pos path))
                       (t (setf (gethash pos path) t)
                          (dolist (dir (list *north* *east* *south* *west*))
                            (bnd1 (npos (move-straight pos dir))
                              (when (gethash npos map)
                                (recur npos path))))
                          (remhash pos path)))))
        (recur start (make-hash-table :test 'equal))
        (1- longest)))))
#+#:excluded (part1)


(defun valid-directions (pos map)
  (looping
    (dolist (dir (list *north* *east* *south* *west*))
      (bnd1 (npos (move-straight pos dir))
        (when (gethash npos map)
          (collect! dir))))))

(defun intersection? (pos map)
  (> (length (valid-directions pos map)) 2))

(defun intersections (&optional (input (parse-map)))
  (destructuring-bind (map _1 _2) input
    (declare (ignore _1 _2))
    (looping
      (dolist (pos (hash-table-keys map))
        (when (intersection? pos map)
          (collect! pos))))))
#+#:excluded (intersections)
#+#:excluded (length *)

(defun distance (start target points &optional (input (parse-map)))
  (destructuring-bind (map _1 _2) input
    (declare (ignore _1 _2))
    (search-cost
      (a* start
          :goal-state target
          :test 'equal
          :neighbors (lambda (pos)
                       (looping
                         (dolist (dir (list *north* *east* *south* *west*))
                           (bnd* ((npos (move-straight pos dir)))
                             (when (and (gethash npos map)
                                        (or (equal npos start)
                                            (equal npos target)
                                            (not (member npos points :test 'equal))))
                               (collect! (cons npos 1)))))))
          :heuristic [manhattan-distance _ target]))))

(defun cache-get (cache p1 p2) (aif (gethash p1 cache) (gethash p2 it)))
(defun cache-set (value cache p1 p2)
  (if (not (gethash p1 cache))
    (setf (gethash p1 cache) (make-hash-table :test 'equal)))
  (setf (gethash p2 (gethash p1 cache)) value))

(defun cached-distances (&optional (input (parse-map)))
  (destructuring-bind (_ start end) input
    (declare (ignore _))
    (bnd* ((points (pr (list* start end (intersections input))))
           (cache (make-hash-table :test 'equal)))
      (dosublists ((p1 . rest) points)
        (cache-set 0 cache p1 p1)
        (dolist (p2 rest)
          (awhen (distance p1 p2 points input)
            (cache-set it cache p1 p2)
            (cache-set it cache p2 p1))))
      cache)))
#+#:excluded (hash-table-alist (cached-distances))

(defun part2 (&optional (input (parse-map)))
  (destructuring-bind (map start end) input
    (bnd* ((cache (cached-distances input))
           (longest 0))
      (labels ((recur (pos path distance)
                 (cond ((gethash pos path) nil)
                       ((equal pos end)
                        (setf longest (max distance longest)))
                       (t (setf (gethash pos path) t)
                          (dolist (npos (hash-table-keys (gethash pos cache)))
                            (when (gethash npos map)
                              (recur npos path (+ (cache-get cache pos npos) distance))))
                          (remhash pos path)))))
        (recur start (make-hash-table :test 'equal) 0)
        longest))))
#+#:excluded (part2)
6406
