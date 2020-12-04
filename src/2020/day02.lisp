(defpackage :aoc/2020/02 #.cl-user::*aoc-use*)
(in-package :aoc/2020/02)

(defun parse-password (string)
  (cl-ppcre:register-groups-bind ((#'parse-integer n m) (#'parse-char ch) text)
      ("(\\d+)-(\\d+) (\\w): (\\w+)" string)
    (list n m ch text)))

(defun parse-passwords (data)
  (mapcar #'parse-password data))

(defun valid-password-part1-p (min max ch text)
  (<= min (count ch text) max))

(defun valid-password-part2-p (pos1 pos2 ch text)
  (let ((ch1 (aref text (1- pos1)))
        (ch2 (aref text (1- pos2))))
    (cond ((char= ch1 ch) (char/= ch2 ch))
          ((char= ch2 ch) (char/= ch1 ch)))))

(define-solution (2020 2) (passwords parse-passwords)
  (loop for (n m ch text) in passwords
        count (valid-password-part1-p n m ch text) into part1
        count (valid-password-part2-p n m ch text) into part2
        finally (return (values part1 part2))))

(define-test (2020 2) (422 451))
