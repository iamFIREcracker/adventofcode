(defpackage :aoc/2015/05 #.cl-user::*aoc-use*)
(in-package :aoc/2015/05)

(defun nice-string-p (string)
  (and (>= (length (cl-ppcre:all-matches-as-strings "[aeiou]" string)) 3)
       (cl-ppcre:all-matches-as-strings "(\\w)\\1" string)
       (not (cl-ppcre:all-matches-as-strings "(ab|cd|pq|xy)" string))))

(defun part1 (strings)
  (count-if #'nice-string-p strings))

(defun improved-nice-string-p (string)
  (and (cl-ppcre:all-matches-as-strings "(.)(.).*\\1\\2" string)
       (cl-ppcre:all-matches-as-strings "(.).\\1" string)))

(defun part2 (strings)
  (count-if #'improved-nice-string-p strings))

(define-solution (2015 5) (strings)
  (values (part1 strings) (part2 strings)))

(define-test (2015 5) (238 69))
