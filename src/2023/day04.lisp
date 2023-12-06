(defpackage :aoc/2023/04 #.cl-user::*aoc-use*)
(in-package :aoc/2023/04)


(defun winning-numbers (s)
  (bnd1 (numbers (second (split-sequence:split-sequence #\: s)))
    (destructuring-bind (winning yours) (split-sequence:split-sequence #\| numbers)
      (intersection (extract-positive-integers winning)
                    (extract-positive-integers yours)))))

(defun card-points (s) (ash 1 (1- (length (winning-numbers s)))))
#+#:excluded (card-points "Card 1: 41 48 83 86 17 | 83 86  6 31 17  9 48 53")

(mapcar #'card-points (uiop:read-file-lines #P"src/2023/day04.txt"))
(reduce #'+ *)
20667


(defun part2 (&optional (strings (uiop:read-file-lines #P"src/2023/day04.txt")))
  (bnd* ((cards-count (make-array (length strings) :initial-element 1))
         (curr -1))
    (looping
      (dolist (s strings)
        (incf curr)
        (bnd* ((current-card-count (aref cards-count curr)))
          (sum! current-card-count)
          (dotimes (i (length (winning-numbers s)))
            (incf (aref cards-count (+ curr i 1)) current-card-count)))))))
(part2)
5833065
