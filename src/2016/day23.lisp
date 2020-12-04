(defpackage :aoc/2016/23 #.cl-user::*aoc-use*)
(in-package :aoc/2016/23)

(define-solution (2016 23) (data)
  (values
    (assembunnycode:run (assembunnycode:parse-program data)
                        (list 7 0 0 0))
    (swallow (assembunnycode:run (assembunnycode:parse-program data)
                                 (list 12 0 0 0)))))

(define-test (2016 23) (12860  479009420))
