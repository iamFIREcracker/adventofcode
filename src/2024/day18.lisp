(defpackage :aoc/2024/18 #.cl-user::*aoc-use*)
(in-package :aoc/2024/18)

(defvar *side* 71)

(defun parse-input (&optional (strings (uiop:read-file-lines #P"src/2024/day18.txt")))
  (let1 grid (make-hash-table :test 'equal)
    (dotimes (i *side*)
      (dotimes (j *side*)
        (setf (gethash (list i j) grid) #\.)))
    (list grid (mapcar #'extract-positive-integers strings))))
#+#:excluded (parse-input)


(defun steps-to-exit (grid)
  (let ((start (list 0 0))
        (end (list (1- *side*) (1- *side*))))
    (search-cost
      (a* start :goal-state end :test 'equal
          :neighbors (fn (pos)
                       (destructuring-bind (i j) pos
                         (looping
                           (doseq ((di dj) '((-1 0) (0 1) (1 0) (0 -1)))
                             (let1 pos1 (list (+ i di) (+ j dj))
                               (when (aand (gethash pos1 grid) (char= it #\.))
                                 (collect! (cons pos1 1))))))))
          :heuristic [manhattan-distance _ end]))))


(define-solution (2024 18) (input parse-input)
  (destructuring-bind (grid bytes) input
    (let (part1 part2)
      (doeseq ((n 1) (j i) bytes)
        (setf (gethash (list i j) grid) #\#)
        (if (= n 1024)
            (setf part1 (steps-to-exit grid)))
        (if (and (not part2) (not (steps-to-exit grid)))
            (setf part2 (spr j "," i))))
      (values part1 part2))))

(define-test (2024 18) (408 "45,16"))
