(defpackage :aoc/2018/14 #.cl-user::*aoc-use*)
(in-package :aoc/2018/14)

(defun parse-recipes-count (data)
  (parse-integer (first data)))

(defstruct (chocolate-chart (:conc-name cc-)
                            (:constructor make-cc%))
  scoreboard
  scoreboard-size
  tail
  elf-1
  elf-2)

(defun make-cc (&aux (scoreboard (ncycle (list 3 7))))
  (make-cc% :scoreboard scoreboard
            :scoreboard-size 2
            :tail (nthcdr 1 scoreboard)
            :elf-1 (nthcdr 0 scoreboard)
            :elf-2 (nthcdr 1 scoreboard)))


(defun part1 (recipes-count &aux (cc (make-cc)))
  (loop
    while (<= (cc-scoreboard-size cc) (+ recipes-count 10))
    do (cc-tick cc)
    finally (return (let ((scores (subseq (cc-scoreboard cc)
                                          recipes-count
                                          (+ recipes-count 10))))
                      (scores->int scores)))))

(defun cc-tick (cc)
  (with-slots (scoreboard scoreboard-size tail elf-1 elf-2) cc
    (let ((scores (reverse (digits (+ (car elf-1) (car elf-2))))))
      (setf scoreboard-size (+ scoreboard-size (length scores))
            (cdr tail) scores
            tail (last scores)
            (cdr (last scores)) scoreboard
            elf-1 (nthcdr (1+ (car elf-1)) elf-1)
            elf-2 (nthcdr (1+ (car elf-2)) elf-2)))))

(defun scores->int (scores)
  (parse-integer (format nil "~{~d~}" scores)))


(defun part2 (recipes-count &aux
                            (target (reverse (digits recipes-count)))
                            (target-size (length target))
                            (cc (make-cc))
                            (offset 0)
                            (head (cc-scoreboard cc)))
  (loop
    until (equal (subseq head 0 target-size) target) do
    (cc-tick cc)
    (when (> (cc-scoreboard-size cc) target-size)
      (setf head (nthcdr 1 head)
            offset (1+ offset))))
  offset)


(define-solution (2018 14) (recipes-count parse-recipes-count)
  (values (part1 recipes-count) (part2 recipes-count)))

(define-test (2018 14) (1191216109 20268576))
