(defpackage :aoc/2024/01 #.cl-user::*aoc-use*)
(in-package :aoc/2024/01)

(defun parse-location-lists (&optional (strings (uiop:read-file-lines #P"src/2024/day01.txt")))
  (let (x1 x2)
    (dolist (s strings)
      (destructuring-bind (n1 n2) (mapcar #'parse-integer (cl-ppcre:all-matches-as-strings "\\d+" s))
        (push n1 x1)
        (push n2 x2)))
    (list x1 x2)))
#+#:excluded (parse-location-lists)

(defun solve1 (&optional (strings (uiop:read-file-lines #P"src/2024/day01.txt")))
  (destructuring-bind (x1 x2) (parse-location-lists strings)
    (setf x1 (sort (copy-seq x1) #'<)
          x2 (sort (copy-seq x2) #'<))
    (looping
      (doseqs ((n1 x1) (n2 x2))
        (sum! (abs (- n1 n2)))))))
#+#:excluded (solve1)

(defun solve2 (&optional (strings (uiop:read-file-lines #P"src/2024/day01.txt")))
  (destructuring-bind (x1 x2) (parse-location-lists strings)
    (looping
      (dolist (n1 x1)
        (sum! (* (count n1 x2) n1))))))
#+#:excluded (solve2)
