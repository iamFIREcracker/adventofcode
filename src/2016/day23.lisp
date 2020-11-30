(defpackage :aoc/2016/23 #.cl-user::*aoc-use*)
(in-package :aoc/2016/23)

(define-problem (2016 23) (data)
  (values
    (assembunnycode:run (assembunnycode:parse-program data)
                        (make-array 4 :initial-contents (list 7 0 0 0)))
    (swallow (assembunnycode:run (assembunnycode:parse-program data)
                                 (make-array 4 :initial-contents (list 12 0 0 0))))))

(1am:test test-2016/23
  (multiple-value-bind (part1 part2) (problem-run)
    (1am:is (= 12860 part1))
    (swallow (1am:is (= 479009420 part2)))))
