(defpackage :aoc/2017/05 #.cl-user::*aoc-use*)
(in-package :aoc/2017/05)

(define-solution (2017 5) (data parse-integers)
  (values
    (loop
      :with maze = (make-array (length data) :initial-contents data)
      :with i = 0
      :while (array-in-bounds-p maze i)
      :for next = (+ i (aref maze i))
      :summing 1
      :do (incf (aref maze i))
      :do (setf i next))
    (loop
      :with maze = (make-array (length data) :initial-contents data)
      :with i = 0
      :while (array-in-bounds-p maze i)
      :for next = (+ i (aref maze i))
      :summing 1
      :do (incf (aref maze i) (if (>= (aref maze i) 3) -1 1))
      :do (setf i next))))

(define-test (2017 5) (372139 29629538))
