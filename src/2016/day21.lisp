(defpackage :aoc/2016/21 #.cl-user::*aoc-use*)
(in-package :aoc/2016/21)

(defun swap-position (string x y)
  (let ((copy (copy-seq string)))
    (rotatef (aref copy x) (aref copy y))
    copy))

(defun swap-letter (string c1 c2)
  (let ((copy (copy-seq string))
        (pos1 (position c1 string))
        (pos2 (position c2 string)))
    (rotatef (aref copy pos1) (aref copy pos2))
    copy))

(defun rotate-left (string steps &aux (steps (mod steps (length string))))
  (concatenate 'string
               (subseq string steps)
               (subseq string 0 steps)))

(defun rotate-right (string steps)
  (let ((tail-size (- (length string) (mod steps (length string)))))
    (concatenate 'string
                 (subseq string tail-size)
                 (subseq string 0 tail-size))))

(defun rotate-on-position (string c)
  (let ((index (position c string)))
    (rotate-right string (if (>= index 4) (+ index 2) (1+ index)))))

(defun reverse-in-between (string start end)
  (concatenate 'string
               (subseq string 0 start)
               (reverse (subseq string start (1+ end)))
               (subseq string (1+ end))))

(defun move (string x y)
  (let ((with-x-popped (concatenate 'string
                           (subseq string 0 x)
                           (subseq string (1+ x)))))
    (concatenate 'string
                 (subseq with-x-popped 0 y)
                 (string (aref string x))
                 (subseq with-x-popped y))))

(defun parse-swap-position (string)
  (cl-ppcre:register-groups-bind ((#'parse-integer x y))
      ("swap position (\\d+) with position (\\d+)" string)
    (list #'swap-position x y)))

(defun parse-swap-letter (string)
  (cl-ppcre:register-groups-bind ((#'parse-char c1 c2))
      ("swap letter (\\w) with letter (\\w)" string)
    (list #'swap-letter c1 c2)))

(defun parse-rotate-leftright (string)
  (cl-ppcre:register-groups-bind (dir (#'parse-integer x))
      ("rotate (left|right) (\\d+) step" string)
    (list (if (string= dir "left") #'rotate-left #'rotate-right) x)))

(defun parse-rotate-on-position (string)
  (cl-ppcre:register-groups-bind ((#'parse-char c))
      ("rotate based on position of letter (\\w)" string)
    (list #'rotate-on-position c)))

(defun parse-reverse-in-between (string)
  (cl-ppcre:register-groups-bind ((#'parse-integer start end))
      ("reverse positions (\\d+) through (\\d+)" string)
    (list #'reverse-in-between start end)))

(defun parse-move (string)
  (cl-ppcre:register-groups-bind ((#'parse-integer x y))
      ("move position (\\d+) to position (\\d+)" string)
    (list #'move x y)))

(defun parse-scramble-op (string)
  (or (parse-swap-position string)
      (parse-swap-letter string)
      (parse-rotate-leftright string)
      (parse-rotate-on-position string)
      (parse-reverse-in-between string)
      (parse-move string)
      (error string)))

(defun parse-scramble-script (data)
  (mapcar #'parse-scramble-op data))

(defun scramble (script input)
  (loop for string = input then (apply fun string args)
        for (fun . args) in script
        finally (return string)))

(defun permutations-string (string)
  (mapcar #'(lambda (x) (coerce x 'string))
          (all-permutations (coerce string 'list))))

(define-problem (2016 21) (script parse-scramble-script)
  (values (scramble script "abcdefgh")
          (loop for input in (permutations-string "abcdefgh")
                when (string= (scramble script input) "fbgdceah")
                return input)))

(1am:test test-2016/21
  (multiple-value-bind (part1 part2) (problem-run)
    (1am:is (string= "gfdhebac" part1))
    (1am:is (string= "dhaegfbc" part2))))
