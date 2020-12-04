(defpackage :aoc/2018/02 #.cl-user::*aoc-use*)
(in-package :aoc/2018/02)


(define-solution (2018 2) (data)
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

(define-test (2018 2) (5390 "nvosmkcdtdbfhyxsphzgraljq"))
