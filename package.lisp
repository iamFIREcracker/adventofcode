(defpackage :aoc
  (:use :cl :1am :split-sequence)
  (:export
    :summation
    :aesthetic-string
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

    :with-gensyms
    :mkstr
    :mkstrc
    :pr
    :prl

    :read-all
    :parse-integers

    :define-problem))


(defparameter *aoc-use* '(:use :cl :1am :split-sequence :aoc))
