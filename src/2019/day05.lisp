(defpackage :aoc/2019/05 #.cl-user::*aoc-use*)
(in-package :aoc/2019/05)

(defun diagnostics-run (data input &aux (program (intcode:read-program data)))
  (enqueue input (intcode:program-in program))
  (intcode:program-run program)
  (loop
    :for o = (dequeue (intcode:program-out program))
    :while (zerop o) ; skip all the successful (i.e. 0) tests
    :finally (return o)))

(define-problem (2019 5) (data)
  (values
    (diagnostics-run data 1)
    (diagnostics-run data 5)))

(1am:test test-2019/05
  (multiple-value-bind (part1 part2) (problem-run)
    (1am:is (= 9938601 part1))
    (1am:is (= 4283952 part2))))
