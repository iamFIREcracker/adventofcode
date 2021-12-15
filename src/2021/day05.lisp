(defpackage :aoc/2021/05 #.cl-user::*aoc-use*)
(in-package :aoc/2021/05)


(defun parse-lines (data)
  (mapcar #'parse-line data))

(defun parse-line (string)
  (mapcar #'parse-integer (cl-ppcre:all-matches-as-strings "\\d+" string)))


(defun part1 (lines)
  (count-overlaps (remove-if #'diagonalp lines)))

(defun diagonalp (line)
  (destructuring-bind (x1 y1 x2 y2) line
    (and (/= x1 x2) (/= y1 y2))))

(defun count-overlaps (lines &optional (grid (make-hash-table :test 'equal)))
  (dolist (l lines)
    (destructuring-bind (x1 y1 x2 y2) l
      (let ((dx (<=> x2 x1))
            (dy (<=> y2 y1))
            (n (max (abs (- x2 x1)) (abs (- y2 y1)))))
        (loop repeat (1+ n)
              for xx = x1 then (+ xx dx)
              for yy = y1 then (+ yy dy) do
              (incf (gethash (list xx yy) grid 0))))))
  (loop for o being the hash-values of grid count (> o 1)))

(defun part2 (lines) (count-overlaps lines))


(define-solution (2021 05) (lines parse-lines)
  (values (part1 lines) (part2 lines)))

(define-test (2021 05) (6397 22335))
