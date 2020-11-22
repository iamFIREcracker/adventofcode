(ql:quickload :aoc/tests :verbose T)

(let ((exit-code 0))
  (handler-case (time (asdf:test-system :aoc))
    (error (c)
      (format T "~&~A~%" c)
      (setf exit-code 1)))
  (uiop:quit exit-code))
