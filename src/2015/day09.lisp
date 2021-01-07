(defpackage :aoc/2015/09 #.cl-user::*aoc-use*)
(in-package :aoc/2015/09)

(defun string-min (string1 string2)
  (if (string< string1 string2)
    (values string1 string2)
    (values string2 string1)))

(defun parse-connection (string)
  (cl-ppcre:register-groups-bind (from to (#'parse-integer distance))
      ("(\\w+) to (\\w+) = (\\d+)" string)
    (multiple-value-bind (a b) (string-min from to)
      (list a b distance))))

(defun parse-connections (lines &aux table)
  (loop for string in lines for (from to distance) = (parse-connection string)
        for existing = (assoc from table :test #'string=)
        if existing do (push (cons to distance) (rest existing))
        else do (push (list from (cons to distance)) table))
  table)

(defun all-cities (connections)
  (remove-duplicates
    (loop for (from . cities) in connections collect from append
          (loop for (to) in cities collect to))
    :test #'string=))

(defun distance (connections from to)
  (multiple-value-bind (a b) (string-min from to)
    (let ((row (rest (assoc a connections :test #'string=))))
      (rest (assoc b row :test #'string=)))))

(defun route-distance (connections route)
  (loop for from in route and to in (rest route)
        sum (distance connections from to)))

(defun part1 (connections &aux (cities (all-cities connections)))
  (reduce #'min (all-permutations cities)
          :key (partial-1 #'route-distance connections)))

(defun part2 (connections &aux (cities (all-cities connections)))
  (reduce #'max (all-permutations cities)
          :key (partial-1 #'route-distance connections)))

(define-solution (2015 9) (connections parse-connections)
  (values (part1 connections) (part2 connections)))

(define-test (2015 9) (141 736))
