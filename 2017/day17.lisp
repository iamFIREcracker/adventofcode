(defpackage :aoc/2017/17 #.cl-user::*aoc-use*)
(in-package :aoc/2017/17)

(define-problem (2017 17) (data read-integer)
  (flet ((next-value (dl)
           (dlink-content (dlink-next dl)))
         (prev-value (buffer)
           (dlink-content (dlink-prev (ring-current buffer)))))
    (loop
      :with buffer = (make-ring 0)
      :with cell-0 = (ring-current buffer)
      :with part1
      :for n :from 1 :upto 50000000
      :do (ring-movef buffer data)
      :do (ring-insertf buffer n)
      :do (when (= n 2017)
            (setf part1 (prl (next-value (ring-current buffer)))))
      :finally (return (values part1 (next-value cell-0))))))

(1am:test test-2017/17
  (multiple-value-bind (part1 part2) (problem-run)
    (1am:is (= 1914 part1))
    (1am:is (= 41797835 part2))))
