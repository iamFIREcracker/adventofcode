(defpackage :aoc
  (:use :cl :pmdb :aoc.quickutils :aoc.quickutils.local :hset :dset)
  (:export
    :array-elements
    :find-min
    :find-max
    :dividesp
    :hexadecimal-string
    :hexadecimal-binary
    :frequencies
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
    :<=>

    :><
    :-><
    :with-slots-as-list

    :dorange
    :doirange
    :dovector

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
    :hq-pop
    :hq-insert

    :make-queue
    :queue-empty-p
    :enqueue
    :dequeue

    :make-summedarea-table
    :st-area-of

    :adjacents
    :a*
    :search-unit-cost
    :bfs
    :floyd
    :binary-search

    :mkstrc
    :gathering
    :gather

    :read-all
    :parse-integers
    :read-integer
    :parse-char

    :define-solution
    :define-test
    :swallow))


(defparameter *aoc-use* '(:use :cl :pmdb :aoc :aoc.quickutils :aoc.quickutils.local :hset :dset))
