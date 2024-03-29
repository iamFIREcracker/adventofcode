(defpackage :aoc/2016/13 #.cl-user::*aoc-use*)
(in-package :aoc/2016/13)

(defvar *favorite-number* nil)

(defun magic (x y)
  (+ (* x x)
     (* 3 x)
     (* 2 x y)
     y
     (* y y)))

(defun openp (x y)
  (loop :with number = (+ (magic x y) *favorite-number*)
        :for index :below (integer-length number)
        :count (logbitp index number) :into bits
        :finally (return (evenp bits))))

(defun neighbors (pos)
  (loop :for next :in (adjacents pos)
        :when (and (>= (realpart next) 0)
                   (>= (imagpart next) 0)
                   (openp (realpart next) (imagpart next)))
        :collect next))

(defun part1 (favorite-number end-state)
  (let ((*favorite-number* favorite-number))
    (search-cost (a* #c(1 1)
                     :goal-state end-state
                     :neighbors (search-unit-cost #'neighbors)
                     :heuristic (partial-1 #'manhattan-distance end-state)))))

(defun part2 (favorite-number)
  (let ((*favorite-number* favorite-number))
    (hash-table-count (search-costs-table
                        (bfs #c(1 1)
                             :neighbors #'neighbors
                             :prune #'(lambda (state cost)
                                       (declare (ignore state))
                                       (> cost 50)))))))

(define-solution (2016 13) (data)
  (values
    (part1 (parse-integer (first data)) #c(31 39))
    (part2 (parse-integer (first data)))))

(define-test (2016 13) (96 141))
