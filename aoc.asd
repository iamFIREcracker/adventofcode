(asdf:defsystem #:aoc
  :description "Advent of Code solutions, in Common Lisp"
  :author "Matteo Landi <matteo@matteolandi.net>"
  :license  "MIT"
  :version "0.0.1"
  :serial t
  :depends-on(#:1am
              #:split-sequence)
  :components
  ((:module "vendor"
    :serial t
    :components ((:file "quickutils")))
   (:file "package")
   (:file "utils")
   (:file "intcode")
   (:file "2017/day01")
   (:file "2017/day02")
   (:file "2017/day03")
   (:file "2017/day04")
   (:file "2017/day05")
   (:file "2017/day06")
   (:file "2017/day07")
   (:file "2017/day08")
   (:file "2017/day09")
   (:file "2017/day10")
   (:file "2017/day11")
   (:file "2017/day12")
   (:file "2017/day13")
   (:file "2017/day14")
   (:file "2017/day15")
   (:file "2017/day16")
   (:file "2017/day17")
   (:file "2017/day18")
   (:file "2017/day19")
   (:file "2017/day20")
   (:file "2017/day21")
   (:file "2017/day22")
   (:file "2017/day23")
   (:file "2017/day24")
   (:file "2017/day25")
   (:file "2018/day01")
   (:file "2018/day02")
   (:file "2018/day03")
   (:file "2018/day05")
   (:file "2018/day08")
   (:file "2018/day09")
   (:file "2018/day11")
   (:file "2018/day22")
   (:file "2018/day25")
   (:file "2019/day01")
   (:file "2019/day02")
   (:file "2019/day03")
   (:file "2019/day04")
   (:file "2019/day05")
   (:file "2019/day06")
   (:file "2019/day07")
   (:file "2019/day08")
   (:file "2019/day09")
   (:file "2019/day10")
   (:file "2019/day11")
   (:file "2019/day12")
   (:file "2019/day13")
   (:file "2019/day14")
   (:file "2019/day15")
   (:file "2019/day16")
   (:file "2019/day17")
   (:file "2019/day18")
   (:file "2019/day19")
   (:file "2019/day20")
   (:file "2019/day21")
   (:file "2019/day22")
   (:file "2019/day23")
   (:file "2019/day24")
   (:file "2019/day25")))
