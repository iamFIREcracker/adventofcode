(defpackage :aoc/2017/09 #.cl-user::*aoc-use*)
(in-package :aoc/2017/09)

(define-problem (2017 9) (data first)
  (let ((stream (coerce data 'simple-array)))
    (recursively ((i 0)
                  (groups 0)
                  (garbagep NIL)
                  (part-1 0)
                  (part-2 0))
      (unless (array-in-bounds-p stream i)
        (return-from recur (values part-1 part-2)))
      (let ((c (aref stream i)))
        (cond ((and garbagep (eql c #\!)) (recur (+ i 2) groups T part-1 part-2))
              ((and garbagep (eql c #\>)) (recur (1+ i) groups NIL part-1 part-2))
              (garbagep (recur (1+ i) groups garbagep part-1 (1+ part-2)))
              ((eql c #\{) (recur (1+ i) (1+ groups) garbagep part-1 part-2))
              ((eql c #\<) (recur (1+ i) groups T part-1 part-2))
              ((eql c #\}) (recur (1+ i) (1- groups) garbagep (+ part-1 groups) part-2))
              (T (recur (1+ i) groups garbagep part-1 part-2)))))))

(1am:test test-2017/09
  (multiple-value-bind (part1 part2) (problem-run)
    (1am:is (= 11347 part1))
    (1am:is (= 5404 part2))))
