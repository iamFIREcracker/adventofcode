(ql:quickload :1am)
(ql:quickload :split-sequence)

(defpackage :aoc
  (:use :cl :1am :split-sequence)
  (:export
    :summation
    :aesthetic-string
    :frequencies
    :hamming-distance
    :curry
    :dorange
    :dovector

    :hash-table-find
    :hash-table-keys
    :hash-table-values
    :print-hash-table

    :with-gensyms
    :mkstr
    :mkstrc
    :pr
    :prl

    :read-all
    :parse-with
    :parse-integers

    :define-problem))
    

(defparameter *aoc-use* '(:use :cl :1am :split-sequence :aoc))
