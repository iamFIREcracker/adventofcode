(asdf:defsystem #:aoc
  :description "Advent of Code solutions, in Common Lisp"
  :author "Matteo Landi <matteo@matteolandi.net>"
  :license  "MIT"
  :version "0.0.1"
  :serial t
  :depends-on(#:1am
              #:split-sequence)
  :components ((:file "package")
               (:file "utils")
               (:file "2017/day01")
               (:file "2017/day02")
               (:file "2017/day03")
               (:file "2017/day04")
               (:file "2018/day01")
               (:file "2018/day02")
               (:file "2018/day03")
               (:file "2018/day05")
               (:file "2018/day08")
               (:file "2018/day09")
               (:file "2018/day25")))
