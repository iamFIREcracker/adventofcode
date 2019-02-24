(defpackage :aoc/2017/05 #.cl-user::*aoc-use*)
(in-package :aoc/2017/05)

(define-problem (2017 5) (data parse-integers)
  (values
    (loop
      :with maze = (make-array (length data) :initial-contents data)
      :with i = 0
      :while (array-in-bounds-p maze i)
      :for next = (+ i (aref maze i))
      :counting 1
      :do (incf (aref maze i))
      :do (setf i next))
    (loop
      :with maze = (make-array (length data) :initial-contents data)
      :with i = 0
      :while (array-in-bounds-p maze i)
      :for next = (+ i (aref maze i))
      :counting 1
      :do (incf (aref maze i) (if (>= (aref maze i) 3) -1 1))
      :do (setf i next))))
  

(1am:test test-2017/05
  (multiple-value-bind (part1 part2) (problem-run)
    (1am:is (= 372139 part1))
    (1am:is (= 29629538 part2))))
