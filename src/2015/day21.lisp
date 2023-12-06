(defpackage :aoc/2015/21 #.cl-user::*aoc-use*)
(in-package :aoc/2015/21)

(defparameter *weapons* '(("Dagger"        8     4       0)
                          ("Shortsword"   10     5       0)
                          ("Warhammer"    25     6       0)
                          ("Longsword"    40     7       0)
                          ("Greataxe"     74     8       0)))

(defparameter *armors* '(("Leather"      13     0       1)
                         ("Chainmail"    31     0       2)
                         ("Splintmail"   53     0       3)
                         ("Bandedmail"   75     0       4)
                         ("Platemail"   102     0       5)))

(defparameter *rings* '(("Damage +1"    25     1       0)
                        ("Damage +2"    50     2       0)
                        ("Damage +3"   100     3       0)
                        ("Defense +1"   20     0       1)
                        ("Defense +2"   40     0       2)
                        ("Defense +3"   80     0       3)))

(defun parse-boss (&optional (lines (uiop:read-file-lines #P"src/2015/day21.txt")))
  (extract-positive-integers (format nil "~{~A ~}" lines)))

(defun all-items-combinations ()
  (loop for w in *weapons* append
        (loop for a in (cons (list "" 0 0 0) *armors*) append
              (loop for r1 in (cons (list "" 0 0 0) *rings*) append
                    (loop for r2 in (cons (list "" 0 0 0) *rings*)
                          collect (mapcar
                                    #'+
                                    (rest w)
                                    (rest a)
                                    (rest r1)
                                    (rest r2)))))))

(defun player-wins-p (player boss)
  (destructuring-bind (p-hits p-damage p-armor) player
    (destructuring-bind (b-hits b-damage b-armor) boss
      (let ((p-turns-to-win (/ b-hits (max (- p-damage b-armor) 1)))
            (b-turns-to-win (/ p-hits (max (- b-damage p-armor) 1))))
        (<= p-turns-to-win b-turns-to-win)))))

(defun part1 (boss)
  (loop for (cost damage armor) in (all-items-combinations)
        when (player-wins-p (list 100 damage armor) boss)
        minimize cost))

(defun part2 (boss)
  (loop for (cost damage armor) in (all-items-combinations)
        unless (player-wins-p (list 100 damage armor) boss)
        maximize cost))

(define-solution (2015 21) (boss parse-boss)
  (values (part1 boss) (part2 boss)))

(define-test (2015 21) (91 158))
