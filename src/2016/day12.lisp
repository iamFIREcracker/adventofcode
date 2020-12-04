(defpackage :aoc/2016/12 #.cl-user::*aoc-use*)
(in-package :aoc/2016/12)

(define-solution (2016 12) (program assembunnycode:parse-program)
  (values
    (assembunnycode:run program (list 0 0 0 0))
    (assembunnycode:run program (list 0 0 1 0))))

(define-test (2016 12) (318083 9227737))
