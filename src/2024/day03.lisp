(defpackage :aoc/2024/03 #.cl-user::*aoc-use*)
(in-package :aoc/2024/03)


(defun parse-instructions (&optional (strings (uiop:read-file-lines #P"src/2024/day03.txt")))
  (let1 s (format nil "~{~A~%~}" strings)
    (cl-ppcre:all-matches-as-strings "mul\\(\\d{1,3},\\d{1,3}\\)|do\\(\\)|don't\\(\\)" s)))
#+#:excluded (parse-instructions)


(define-solution (2024 03) (instructions parse-instructions)
  (let ((part1 0) (part2 0) (do t))
    (dolist (ins instructions)
      (cond ((string= ins "do()") (setf do t))
            ((string= ins "don't()") (setf do nil))
            (t (let1 mul (apply '* (extract-integers ins))
                 (incf part1 mul)
                 (when do
                   (incf part2 mul))))))
    (values part1 part2)))

(define-test (2024 03) (170068701 78683433))
