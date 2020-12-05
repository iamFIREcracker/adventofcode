(defpackage :aoc/2020/05 #.cl-user::*aoc-use*)
(in-package :aoc/2020/05)

(defun parse-seat (string)
  (parse-integer
    (map 'string
         #'(lambda (ch)
            (ecase ch
              ((#\F #\L) #\0)
              ((#\B #\R) #\1)))
         string)
    :radix 2))

(defun parse-seats (data)
  (mapcar #'parse-seat data))

(define-solution (2020 5) (data)
  (loop with seats = (sort (parse-seats data) #'<) with part2
        for curr in seats for next in (rest seats)
        when (= (- next curr) 2) do (setf part2 (1+ curr))
        finally (return (values curr part2))))

(define-test (2020 5) (908 619))
