(defpackage :aoc
  (:use :cl :1am :split-sequence)
  (:export
    :summation
    :dividesp
    :aesthetic-string
    :sorted
    :nsorted
    :frequencies
    :hamming-distance
    :manhattan-distance
    :recursively
    :recur
    :curry
    :dorange
    :dovector

    :hash-table-find
    :hash-table-keys
    :hash-table-values
    :print-hash-table

    :make-disjointset
    :disjointset-value
    :disjointset-rank
    :disjointset-parent
    :disjointset-find
    :disjointset-union

    :make-dlink
    :dlink-current
    :dlink-prev
    :dlink-next
    :dlink-removef
    :dlink-insertf

    :with-gensyms
    :mkstr
    :mkstrc
    :pr
    :prl

    :read-all
    :parse-integers
    :read-integer

    :define-problem))


(defparameter *aoc-use* '(:use :cl :1am :split-sequence :aoc))
