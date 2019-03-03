(defpackage :aoc
  (:use :cl :1am :split-sequence)
  (:export
    :summation
    :maximization
    :dividesp
    :aesthetic-string
    :hexadecimal-string
    :hexadecimal-binary
    :sorted
    :nsorted
    :frequencies
    :hamming-distance
    :manhattan-distance
    :curry

    :complex-rotate-cw
    :complex-rotate-ccw

    :recursively
    :recur
    :gathering
    :gather

    :dorange
    :doirange
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
    :distinct-disjointsets

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
