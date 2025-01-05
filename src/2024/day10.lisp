(defpackage :aoc/2024/10 #.cl-user::*aoc-use*)
(in-package :aoc/2024/10)


(defun parse-input (&optional (strings (uiop:read-file-lines #P"src/2024/day10.txt")))
  (prog1-let map (make-hash-table :test 'equal)
    (doseq ((i s) (enumerate strings))
      (doseq ((j ch) (enumerate s))
        (setf (gethash (list i j) map) (- (char-code ch) (char-code #\0)))))))
#+#:excluded (parse-input)


(defun trailheads (map i j)
  (let1 frontier (list (list i j))
    (looping
      (while frontier
        (destructuring-bind (i j) (pop frontier)
          (if (= (gethash (list i j) map) 9)
              (collect! (list i j))
              (doseq ((di dj) '((-1 0) (0 1) (1 0) (0 -1)))
                (let ((i1 (+ i di)) (j1 (+ j dj)))
                  (if (aand (gethash (list i1 j1) map) (= (1- it) (gethash (list i j) map)))
                      (push (list i1 j1) frontier))))))))))


(define-solution (2024 10) (map parse-input)
  (values
    (looping
      (dohash ((i j) height map)
        (when (= height 0)
          (sum! (length (remove-duplicates (trailheads map i j) :test 'equal))))))
    (looping
      (dohash ((i j) height map)
        (when (= height 0)
          (sum! (length (trailheads map i j))))))))

(define-test (2024 10) (472 969))
