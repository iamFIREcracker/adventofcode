(defpackage :aoc/2024/04 #.cl-user::*aoc-use*)
(in-package :aoc/2024/04)

(defun parse-grid (&optional (strings (uiop:read-file-lines #P"src/2024/day04.txt")))
  (let1 grid (make-hash-table :test 'equal)
    (doseq ((i s) (enumerate strings))
      (doseq ((j ch) (enumerate s))
        (setf (@ grid (list i j)) ch)))
    grid))
#+#:excluded (parse-grid)

(defparameter *xmas-patterns*
  '(((#\X 0 0) (#\M -1  0) (#\A -2  0) (#\S -3  0))
    ((#\X 0 0) (#\M  1  0) (#\A  2  0) (#\S  3  0))

    ((#\X 0 0) (#\M  0  1) (#\A  0  2) (#\S  0  3))
    ((#\X 0 0) (#\M  0 -1) (#\A  0 -2) (#\S  0 -3))

    ((#\X 0 0) (#\M -1  1) (#\A -2  2) (#\S -3  3))
    ((#\X 0 0) (#\M  1 -1) (#\A  2 -2) (#\S  3 -3))

    ((#\X 0 0) (#\M  1  1) (#\A  2  2) (#\S  3  3))
    ((#\X 0 0) (#\M -1 -1) (#\A -2 -2) (#\S -3 -3))
    ))

(defparameter *x-mas-patterns* '(((#\A 0  0) (#\M -1 -1) (#\S -1  1) (#\M 1 -1) (#\S 1  1))
                                 ((#\A 0  0) (#\M -1 -1) (#\M -1  1) (#\S 1 -1) (#\S 1  1))
                                 ((#\A 0  0) (#\S -1 -1) (#\M -1  1) (#\S 1 -1) (#\M 1  1))
                                 ((#\A 0  0) (#\S -1 -1) (#\S -1  1) (#\M 1 -1) (#\M 1  1))
                                 ))


(defun matches? (grid pattern i j)
  (looping
    (doseq ((ch di dj) pattern)
      (always! (eql (@ grid (list (+ i di) (+ j dj))) ch)))))

(defun count-matching (grid patterns)
  (let ((rows (1+ (reduce 'max (hash-table-keys grid) :key #'car)))
        (cols (1+ (reduce 'max (hash-table-keys grid) :key #'cadr))))
    (looping
      (dotimes (i rows)
        (dotimes (j cols)
          (dolist (p patterns)
            (count! (matches? grid p i j))))))))

(define-solution (2024 4) (grid parse-grid)
  (values (count-matching grid *xmas-patterns*)
          (count-matching grid *x-mas-patterns*)))

(define-test (2024 03) (2536 1875))
