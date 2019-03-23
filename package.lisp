(defpackage :aoc
  (:use :cl :1am :split-sequence)
  (:export
    :summation
    :maximization
    :minimization
    :dividesp
    :alphabet
    :aesthetic-string
    :hexadecimal-string
    :hexadecimal-binary
    :sorted
    :nsorted
    :frequencies
    :group-by
    :unique-only
    :hamming-distance
    :manhattan-distance

    :_
    :partial-1
    :_1
    :_2
    :partial-2

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
    :hash-table-insert
    :print-hash-table

    :make-disjointset
    :disjointset-value
    :disjointset-rank
    :disjointset-parent
    :disjointset-find
    :disjointset-union
    :distinct-disjointsets

    :make-dlink
    :dlink-content
    :dlink-prev
    :dlink-next
    :dlink-removef
    :dlink-insertf

    :make-ring
    :ring-current
    :ring-movef
    :ring-removef
    :ring-insertf

    :make-hq
    :hq-popf
    :hq-insertf

    :a-star
    :a-star-backtrack

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
