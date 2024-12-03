(defpackage :aoc/2024/03 #.cl-user::*aoc-use*)
(in-package :aoc/2024/03)

(defun read-muls (&optional (s (uiop:read-file-string #P"src/2024/day03.txt")))
  (cl-ppcre:all-matches-as-strings "mul\\(\\d{1,3},\\d{1,3}\\)" s))
(read-muls)
(mapcar #'extract-integers *)
(mapcar [* (car _) (cadr _)] *)
(reduce '+ *)

(defun read-muls2 (&optional (s (uiop:read-file-string #P"src/2024/day03.txt")))
  (cl-ppcre:all-matches-as-strings "(mul\\(\\d{1,3},\\d{1,3}\\)|do\\(\\)|don't\\(\\))" s))

(looping
  (let1 do t
    (dolist (ins (read-muls2))
      (cond ((string= ins "do()") (setf do t))
            ((string= ins "don't()") (setf do nil))
            (t (sum! (apply '* (extract-integers ins))))))))
