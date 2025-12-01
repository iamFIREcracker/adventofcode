(defpackage :aoc/2025/01 #.cl-user::*aoc-use*)
(in-package :aoc/2025/01)

(defun read-rotations (&optional (strings (uiop:read-file-lines #P"src/2025/day01.txt")))
  (looping
    (dolist (s strings)
      (destructuring-bind (a b) (split s 1)
        (collect! (list (if (string= a "R") 1 -1)
                        (parse-integer b)))))))

(defun part1 (&optional (rots (read-rotations)))
  (let1 curr 50
    (looping
      (doseq ((dir clicks) rots)
        (setf curr (mod (+ curr (* dir clicks)) 100))
        (count! (= curr 0))))))

(defun part2 (&optional (rots (read-rotations)))
  (let1 curr 50
    (looping
      (doseq ((dir clicks) rots)
        (repeat clicks
          (setf curr (mod (+ curr dir) 100))
          (count! (= curr 0)))))))


(define-solution (2025 01) (rots read-rotations)
  (values (part1 rots)
          (part2 rots)))

(define-test (2025 01) (1135 6558))
