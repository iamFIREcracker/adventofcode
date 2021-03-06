(defpackage :aoc/2016/19 #.cl-user::*aoc-use*)
(in-package :aoc/2016/19)

(defun parse-num-elfes (data)
  (parse-integer (first data)))

(defun part1 (players)
  (let ((playing (ncycle (loop for i from 1 upto players collect i))))
    (loop repeat players
          for remaining = playing then (cdr remaining) do
          (setf (cdr remaining) (cddr remaining))
          finally (return (first remaining)))))

(defun part2 (players)
  (let ((playing (ncycle (loop for i from 1 upto players collect i))))
    (loop with middle = (nthcdr (1- (floor players 2)) playing)
          for size from players downto 1
          for remaining = middle then (if (evenp size) (cdr remaining) remaining) do
          (setf (cdr remaining) (cddr remaining))
          finally (return (first remaining)))))

(define-solution (2016 19) (elves parse-num-elfes)
  (values
    (part1 elves)
    (part2 elves)))

(define-test (2016 19) (1834903 1420280))
