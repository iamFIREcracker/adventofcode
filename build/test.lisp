(handler-case (ql:quickload :aoc/tests)
  (error (a) (format t "caught error ~s~%~a~%" a a) (uiop:quit 17)))

(handler-case (time (asdf:test-system :aoc))
  (error (a)
    (format T "caught error ~s~%~a~%" a a) (uiop:quit 13)))
