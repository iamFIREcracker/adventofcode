(defpackage :aoc/2023/06 #.cl-user::*aoc-use*)
(in-package :aoc/2023/06)

(defun parse-records (&optional (strings (uiop:read-file-lines #P"src/2023/day06.txt")))
  (destructuring-bind (times records) strings
    (mapcar #'list
            (extract-positive-integers times)
            (extract-positive-integers records))))

(defun max-distance (time hold)
  (* hold (- time hold)))

(defun count-wins (input)
  (destructuring-bind (time record) input
    (looping
      (dorangei (hold 0 time)
        (count! (> (max-distance time hold) record))))))

(defun massage-input (&optional (strings (uiop:read-file-lines #P"src/2023/day06.txt")))
  (mapcar [remove-if-not #'digit-char-p _] strings))


(define-solution (2023 06) (strings)
  (values (reduce #'* (parse-records strings) :key #'count-wins)
          (bnd1 strings (massage-input strings)
            (reduce #'* (parse-records strings) :key #'count-wins))))

(define-test (2023 06) (220320 34454850))
