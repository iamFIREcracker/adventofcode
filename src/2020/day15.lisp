(defpackage :aoc/2020/15 #.cl-user::*aoc-use*)
(in-package :aoc/2020/15)

(defun parse-numbers (data)
  (mapcar #'parse-integer (split-sequence:split-sequence #\, (first data))))

(defun play (numbers turn-max &aux
                     (seen-at-1 (make-hash-table))
                     (seen-at-2 (make-hash-table)))
  (loop for n in numbers for turn from 1 do (setf (gethash n seen-at-1) turn))
  (loop with last = (first (reverse numbers))
        for turn from (1+ (length numbers)) to turn-max
        for turn-1 = (gethash last seen-at-1)
        for turn-2 = (gethash last seen-at-2)
        if (not turn-2) do (setf last 0) else do (setf last (- turn-1 turn-2))
        do (setf (gethash last seen-at-2) (gethash last seen-at-1)
                 (gethash last seen-at-1) turn)
        finally (return last)))

(define-solution (2020 15) (numbers parse-numbers)
  (values (play numbers 2020) (play numbers 30000000)))

(define-test (2020 15) (1238 3745954))
