(defpackage :aoc/2019/09 #.cl-user::*aoc-use*)
(in-package :aoc/2019/09)

(defun boost-run (data input &aux (program (intcode:read-program data)))
  (enqueue input (intcode:program-in program))
  (intcode:program-run program)
  (dequeue (intcode:program-out program)))

(define-solution (2019 9) (data)
  (values
    (boost-run data 1)
    (boost-run data 2)))

(define-test (2019 9) (3280416268 80210))
