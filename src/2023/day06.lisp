(defpackage :aoc/2023/06 #.cl-user::*aoc-use*)
(in-package :aoc/2023/06)

(defvar *data* '((59 597) (79 1234) (65 1032) (75 1328)))
#+#:part2(setf *data* '((59796575 597123410321328)))

(defun max-distance (time hold)
  (* hold (- time hold)))

(defun count-wins (input)
  (destructuring-bind (time record) input
    (looping
      (dorangei (hold 0 time)
        (count! (> (max-distance time hold) record))))))
#+#:excluded (count-wins '(7 9))

(reduce #'* *data* :key #'count-wins)
