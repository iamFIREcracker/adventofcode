(defpackage :aoc/2017/17 #.cl-user::*aoc-use*)
(in-package :aoc/2017/17)

(define-problem (2017 17) (data read-integer)
  (values
    (flet ((next-value (dl)
             (dlink-content (dlink-next dl))))
      (loop
        :with buffer = (make-ring 0)
        :for n :from 1 :upto 2017
        :do (ring-movef buffer data)
        :do (ring-insertf buffer n)
        :finally (return (next-value (ring-current buffer)))))
    (loop
      :with curr = 0
      :with last
      :for n :from 1 :upto 50000000
      :for next = (mod (+ curr data) n)
      :do (setf curr (1+ next))
      :when (= curr 1) :do (setf last n)
      :finally (return last))))

(1am:test test-2017/17
  (multiple-value-bind (part1 part2) (problem-run)
    (1am:is (= 1914 part1))
    (1am:is (= 41797835 part2))))
