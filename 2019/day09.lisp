(defpackage :aoc/2019/09 #.cl-user::*aoc-use*)
(in-package :aoc/2019/09)

(defstruct (boost (:constructor make-boost%))
  program
  in
  out)

(defun make-boost (program)
  (let* ((in (make-queue))
         (out (make-queue))
         (program (intcode:make-program (copy-hash-table (intcode:program-memory program)) in out)))
    (make-boost% :program program :in in :out out)))

(defun boost-run (boost input)
  (enqueue input (boost-in boost))
  (intcode:program-run (boost-program boost))
  (dequeue (boost-out boost)))

(define-problem (2019 9) (program intcode:read-program)
  (values
    (boost-run (make-boost program) 1)
    (boost-run (make-boost program) 2)))

(1am:test test-2019/09
  (multiple-value-bind (part1 part2) (problem-run)
    (1am:is (= 3280416268 part1))
    (1am:is (= 80210 part2))))
