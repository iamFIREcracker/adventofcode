(defpackage :aoc
  (:use :cl :pmdb :1am :split-sequence :aoc.quickutils)
  (:export
    :summation
    :maximization
    :minimization
    :maximizing
    :minimizing
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
    :all-permutations

    :_
    :partial-1
    :_1
    :_2
    :partial-2
    :defun/memo

    :with-complex-parts
    :complex-rotate-cw
    :complex-rotate-ccw
    :modn
    :digits-reverse
    :str-digits
    :<=>

    :recursively
    :recur
    :><
    :-><
    :with-slots-as-list

    :dorange
    :doirange
    :dovector

    :hash-table-find
    :hash-table-insert
    :hash-table-contains-key-p
    :print-hash-table
    :print-hash-table-map

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

    :make-queue
    :queue-empty-p
    :enqueue
    :dequeue

    :make-summedarea-table
    :st-area-of

    :adjacents
    :a-star
    :a-star-backtrack
    :a-star-neighbors-cost-auto-increment
    :bfs
    :dijkstra
    :floyd
    :binary-search

    :mkstrc
    :gathering
    :gather

    :read-all
    :parse-integers
    :read-integer

    :define-problem))


(defparameter *aoc-use* '(:use :cl :1am :split-sequence :aoc :aoc.quickutils))
