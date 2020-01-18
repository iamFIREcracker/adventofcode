(defpackage :aoc/2018/02 #.cl-user::*aoc-use*)
(in-package :aoc/2018/02)


(define-problem (2018 2) (data)
  (values
    (loop
      :for id :in data
      :for freqs = (frequencies id)
      :counting (hash-table-find 2 freqs) :into twos
      :counting (hash-table-find 3 freqs) :into threes
      :finally (return (* twos threes)))
    (multiple-value-bind (a b)
        (loop
          :for (a . remaining) :on data
          :for b = (find 1 remaining :key (partial-1 #'hamming-distance a))
          :do (when b
                (return (values a b))))
      (let ((i (mismatch a b)))
        (concatenate 'string
                     (subseq a 0 i)
                     (subseq a (1+ i)))))))

(1am:test test-2018/02
  (multiple-value-bind (part1 part2) (problem-run)
    (1am:is (= 5390 part1))
    (1am:is (string= "nvosmkcdtdbfhyxsphzgraljq" part2))))
