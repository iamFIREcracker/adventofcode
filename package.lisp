(defpackage :aoc
  (:use :cl :aoc.quickutils :aoc.quickutils.local :hset :dset)
  (:export
    :array-elements
    :find-min
    :find-max
    :dividesp
    :hexadecimal-string
    :hexadecimal->binary
    :make-counter
    :frequencies
    :hamming-distance
    :manhattan-distance
    :all-permutations
    :as-keyword
    :split

    :_
    :partial-1
    :_1
    :_2
    :partial-2
    :defun/memo
    :memoizing
    :unless-already-seen

    :with-complex-parts
    :complex-rotate-cw
    :complex-rotate-ccw
    :<=>

    :><
    :-><
    :with-slots-as-list

    :hash-table-insert
    :print-hash-table
    :print-hash-table-map

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
    :hq-empty-p
    :hq-pop
    :hq-insert

    :make-queue
    :queue-empty-p
    :enqueue
    :dequeue

    :aref-or
    :make-summedarea-table
    :st-area-of

    :adjacents
    :dijkstra
    :a*
    :search-state
    :search-cost
    :search-path
    :search-costs-table
    :search-unit-cost
    :bfs
    :floyd
    :binary-search

    :mkstrc
    :mkstrs
    :gathering
    :gather

    :read-all
    :parse-integers
    :read-integer
    :parse-char
    :extract-positive-integers
    :extract-integers
    :extract-forms

    :define-solution
    :define-test))


(defparameter *aoc-use* '(:use :cl :aoc :aoc.quickutils :aoc.quickutils.local :hset :dset))
