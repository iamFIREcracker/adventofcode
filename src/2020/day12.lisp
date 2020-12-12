(defpackage :aoc/2020/12 #.cl-user::*aoc-use*)
(in-package :aoc/2020/12)

(defun parse-instruction (string)
  (cons (char string 0) (parse-integer string :start 1)))

(defun parse-instructions (data)
  (mapcar #'parse-instruction data))

(defun part1 (instructions)
  (loop with pos = 0 with dir = #c(1 0) for (ch . n) in instructions do
        (ecase ch
          (#\N (incf pos (* n #c(0 1))))
          (#\S (incf pos (* n #c(0 -1))))
          (#\E (incf pos (* n #c(1 0))))
          (#\W (incf pos (* n #c(-1 0))))
          (#\L (setf dir (* dir (expt #c(0 1) (/ n 90)))))
          (#\R (setf dir (* dir (expt #c(0 -1) (/ n 90)))))
          (#\F (incf pos (* n dir))))
        finally (return (manhattan-distance pos 0))))

(defun part2 (instructions)
  (loop with pos = 0 with dir = #c(10 1) for (ch . n) in instructions do
        (ecase ch
          (#\N (incf dir (* n #c(0 1))))
          (#\S (incf dir (* n #c(0 -1))))
          (#\E (incf dir (* n #c(1 0))))
          (#\W (incf dir (* n #c(-1 0))))
          (#\L (setf dir (* dir (expt #c(0 1) (/ n 90)))))
          (#\R (setf dir (* dir (expt #c(0 -1) (/ n 90)))))
          (#\F (incf pos (* n dir))))
        finally (return (manhattan-distance pos 0))))

(define-solution (2020 12) (instructions parse-instructions)
  (values (part1 instructions) (part2 instructions)))

(define-test (2020 12) (521 22848))
