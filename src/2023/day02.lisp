(defpackage :aoc/2023/02 #.cl-user::*aoc-use*)
(in-package :aoc/2023/02)

(defun parse-game (s)
  (destructuring-bind (id-part game-part) (split-sequence:split-sequence #\: s)
    (cons (first (extract-positive-integers id-part))
          (looping
            (dolist (set-part (split-sequence:split-sequence #\; game-part))
              (collect!
                (looping
                  (dolist (reveal-part (split-sequence:split-sequence #\, set-part))
                    (cl-ppcre:register-groups-bind (cubes (#'as-keyword color))
                        ("(\\d+) (red|green|blue)" reveal-part)
                      (collect! (cons (make-keyword color) (parse-integer cubes))))))))))))

(defun parse-games (&optional (strings (uiop:read-file-lines #P"src/2023/day02.txt")))
  (mapcar #'parse-game strings))


(defun possible? (game)
  (every (lambda (set)
           (and (<= (or (cdr (assoc :red set)) 0) 12)
                (<= (or (cdr (assoc :green set)) 0) 13)
                (<= (or (cdr (assoc :blue set)) 0) 14)))
         (cdr game)))


(defun min-cubes (game)
  (flet ((by-color (color set)
           (or (cdr (assoc color set)) 0)))
    (looping
      (dolist (c (list :red :green :blue))
        (collect! (reduce #'max (cdr game) :key [by-color c _]))))))

(defun power (cubes) (reduce #'* cubes))


(define-solution (2023 02) (games parse-games)
  (values (reduce #'+ (remove-if-not #'possible? games) :key #'first)
          (reduce #'+ (mapcar #'min-cubes games) :key #'power)))

(define-test (2023 02) (2256 74229))
