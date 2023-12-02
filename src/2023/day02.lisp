(defpackage :aoc/2023/02 #.cl-user::*aoc-use*)
(in-package :aoc/2023/02)

(defun parse-game (s)
  (bnd1 ((id-part game-part) (split-sequence:split-sequence #\: s))
    (cons (parse-integer (second (split-sequence:split-sequence #\Space id-part)) :junk-allowed t)
          (looping
            (dolist (set-part (split-sequence:split-sequence #\; game-part))
              (collect!
                (looping
                  (dolist (reveal-part (split-sequence:split-sequence #\, set-part))
                    (if (search "blue" reveal-part)
                      (collect! (cons :blue (read-from-string reveal-part))))
                    (if (search "green" reveal-part)
                      (collect! (cons :green (read-from-string reveal-part))))
                    (if (search "red" reveal-part)
                      (collect! (cons :red (read-from-string reveal-part)))) ))))))))
#+#:excluded (parse-game "Game 1: 3 blue, 4 red; 1 red, 2 green, 6 blue; 2 green")
#+#:excluded (assoc :red (car (cdr *)))
(defun possible? (game)
  (every (lambda (set)
           (and (<= (or (cdr (assoc :red set)) 0) 12)
                (<= (or (cdr (assoc :green set)) 0) 13)
                (<= (or (cdr (assoc :blue set)) 0) 14)))
         (cdr game)))
#+#:excluded (possible? (parse-game "Game 1: 3 blue, 4 red; 1 red, 2 green, 6 blue; 2 green"))
(uiop:read-file-lines #P"src/2023/day02.txt")
(mapcar #'parse-game *)
(remove-if-not #'possible? *)
(reduce #'+ * :key #'first)

(defun power (game)
  (reduce #'* '(:red :green :blue)
          :key (lambda (color)
                 (reduce #'max (cdr game)
                         :key (lambda (set) (or (cdr (assoc color set)) 0))))))
