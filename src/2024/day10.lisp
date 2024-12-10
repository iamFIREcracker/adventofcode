(defpackage :aoc/2024/10 #.cl-user::*aoc-use*)
(in-package :aoc/2024/10)

#;
(defun parse-input (&optional (strings (uiop:read-file-lines #P"src/2024/day10.txt")))
  (prog1-let (map (make-hash-table :test 'equal))
    (doseq ((i s) (enumerate strings))
      (doseq ((j ch) (enumerate s))
        (setf (@ map (list i j)) (- (char-code ch) (char-code #\0)))))))
#+#:excluded (parse-input)

(defun count-trailheads (map i j)
  (let1 frontier (list (list i j))
    (length
      (remove-duplicates
        (looping
          (while frontier
            (destructuring-bind (i j) (pop frontier)
              (when (= (@ map (list i j)) 9)
                (collect! (list i j)))
              (doseq ((di dj) '((-1 0) (0 1) (1 0) (0 -1)))
                (let ((i1 (+ i di)) (j1 (+ j dj)))
                  (if (aand (@ map (list i1 j1)) (= (1- it) (@ map (list i j))))
                      (push (list i1 j1) frontier)))))))
        :test 'equal))))
(defun count-trailheads (map i j)
  (let1 frontier (list (list i j))
    (looping
      (while frontier
        (destructuring-bind (i j) (pop frontier)
          (count! (= (@ map (list i j)) 9))
          (doseq ((di dj) '((-1 0) (0 1) (1 0) (0 -1)))
            (let ((i1 (+ i di)) (j1 (+ j dj)))
              (if (aand (@ map (list i1 j1)) (= (1- it) (@ map (list i j))))
                  (push (list i1 j1) frontier)))))))))
(let1 map (parse-input)
  (looping
    (dohash ((i j) height map)
      (when (= height 0)
        (sum! (count-trailheads map i j))))))
969 nope
(untrace count-trailheads)
