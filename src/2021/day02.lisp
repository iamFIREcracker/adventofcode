(defpackage :aoc/2021/02 #.cl-user::*aoc-use*)
(in-package :aoc/2021/02)


(defun parse-instructions (data) (mapcar #'parse-instruction data))

(defun parse-instruction (string)
  (cl-ppcre:register-groups-bind ((#'as-keyword dir) (#'parse-integer delta))
      ("(\\w+) (\\d+)" string)
    (cons dir delta)))
(defun dir (i) (car i))
(defun delta (i) (cdr i))


(defun part1 (instructions &aux (horiz 0) (depth 0))
  (dolist (i instructions (* horiz depth))
    (ecase (dir i)
      (:forward (incf horiz (delta i)))
      (:up (decf depth (delta i)))
      (:down (incf depth (delta i))))))


(defun part2 (instructions &aux (horiz 0) (depth 0) (aim 0))
  (dolist (i instructions (* horiz depth))
    (ecase (dir i)
      (:forward (incf horiz (delta i)) (decf depth (* (delta i) aim)))
      (:up (incf aim (delta i)))
      (:down (decf aim (delta i))))))


(define-solution (2021 02) (instructions parse-instructions)
  (values (part1 instructions) (part2 instructions)))

(define-test (2021 02) (1648020 1759818555))
#+#:excluded (test-run)
