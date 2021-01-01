(defpackage :aoc/2015/03 #.cl-user::*aoc-use*)
(in-package :aoc/2015/03)

(defun parse-instruction (ch)
  (ecase ch (#\< #c(-1 0)) (#\^ #c(0 1)) (#\> #c(1 0)) (#\v #c(0 -1))))

(defun parse-input (lines)
  (map 'list #'parse-instruction (first lines)))

(defun part1 (instructions)
  (loop with pos = 0 and houses = (list 0)
        for d in instructions
        do (pushnew (incf pos d) houses)
        finally (return (length houses))))

(defun part2 (instructions)
  (loop with santa = 0 and robot = 0 and houses = (list 0)
        for d in instructions for santa-turn-p = t then (not santa-turn-p)
        if santa-turn-p do (pushnew (incf santa d) houses)
        else do (pushnew (incf robot d) houses)
        finally (return (length houses))))

(define-solution (2015 3) (instructions parse-input)
  (values (part1 instructions) (part2 instructions)))

(define-test (2015 3) (2565 2639))
