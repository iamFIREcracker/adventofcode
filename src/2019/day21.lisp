(defpackage :aoc/2019/21 #.cl-user::*aoc-use*)
(in-package :aoc/2019/21)

(defstruct (droid (:constructor make-droid%))
  program
  in
  out)

(defun make-droid (program)
  (let* ((program (intcode:make-program (copy-hash-table (intcode:program-memory program)))))
    (make-droid% :program program
                 :in (intcode:program-in program)
                 :out (intcode:program-out program))))

(defun droid-configure (d code)
    (loop
      :for instruction :in code
      :do (loop
            :for c :across instruction
            :do (enqueue (char-code c) (droid-in d))
            :finally (enqueue (char-code #\Newline) (droid-in d)))))

(defun droid-run (d code)
    (droid-configure d code)
    (intcode:program-run (droid-program d))
    (loop
      :until (queue-empty-p (droid-out d))
      :for v = (dequeue (droid-out d))
      :when (>= v 255) :return v
      :do (format T "~a" (code-char v))))

(define-problem (2019 21) (program intcode:read-program)
  (values 
    ;; jump if:
    ;; - the next tile is empty
    ;; - the third tile is empty, but the fourth is not
    (let ((code '("NOT A T"

                  "NOT C J"
                  "AND D J"
                  "OR T J"

                  "WALK")))
      (droid-run (make-droid program) code))
    ;; jump if:
    ;; - the next tile is empty
    ;; - the third tile is empty, the fourth is not, the eight is not
    ;;   (there was a case where you would land on a single tile,
    ;;   be forced to jump and land on a hole!)
    ;; - there is a hole 2 tiles ahead, but it's safe to land in 4, so we can
    ;;   jump immediately
    (let ((code '("NOT A T"

                  "NOT C J"
                  "AND D J"
                  "AND H J"
                  "OR J T"

                  "NOT B J"
                  "AND D J"
                  "OR T J"

                  "RUN")))
      (droid-run (make-droid program) code))))

(1am:test test-2019/21
  (multiple-value-bind (part1 part2) (problem-run)
    (1am:is (= 19355391 part1))
    (1am:is (= 1143770635 part2))))
