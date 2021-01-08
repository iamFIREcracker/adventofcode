(defpackage :aoc/2015/13 #.cl-user::*aoc-use*)
(in-package :aoc/2015/13)

(defun parse-note (string)
  (cl-ppcre:register-groups-bind (person1 action (#'parse-integer amount) person2)
      ("(\\w+) would (lose|gain) (\\d+) happiness units by sitting next to (\\w+)." string)
    (list person1 person2 (* amount (if (string= action "lose") -1 1)))))

(defun parse-notes (lines &aux notes)
  (loop for string in lines for (person1 person2 delta) = (parse-note string)
        for existing = (assoc person1 notes :test #'string=)
        if existing do (push (cons person2 delta) (rest existing))
        else do (push (list person1 (cons person2 delta)) notes))
  notes)
(defun all-people (notes) (mapcar #'first notes))

(defun delta-happiness (notes person1 person2)
  (flet ((lookup (person1 person2)
           (let ((row (rest (assoc person1 notes :test #'string=))))
             (rest (assoc person2 row :test #'string=)))))
    (+ (lookup person1 person2) (lookup person2 person1))))

(defun circular-table-happyness (notes table)
  (loop for prev = (car (last table)) then curr
        for curr in table
        sum (delta-happiness notes prev curr)))

(defun part1 (notes)
  (let ((people (all-people notes)))
    (reduce #'max (all-permutations people)
            :key (partial-1 #'circular-table-happyness notes))))

(defun linear-table-happyness (notes table)
  (loop for prev in table for curr in (rest table)
        sum (delta-happiness notes prev curr)))

(defun part2 (notes)
  (let ((people (all-people notes)))
    (reduce #'max (all-permutations people)
            :key (partial-1 #'linear-table-happyness notes))))

(define-solution (2015 13) (notes parse-notes)
  (values (part1 notes) (part2 notes)))

(define-test (2015 13) (618 601))
