(defpackage :aoc/2025/03 #.cl-user::*aoc-use*)
(in-package :aoc/2025/03)

(defun read-joltage-ratings (&optional (strings (uiop:read-file-lines #P"src/2025/day03.txt")))
  (looping
    (dolist (s strings)
      (collect!
        (looping
          (doseq (ch s)
            (collect! (parse-integer (spr ch)))))))))


(defun find-largest-possible-joltage (n bb
                                      &aux (bb1 (sort (remove-duplicates bb) '>)))
  (recursively ((n n)
                (start 0)
                (acc 0))
    (cond
      ((= n 0) (return-from find-largest-possible-joltage acc))
      (t (dolist (j bb1)
           (let1 pos (position j bb :start start)
             (when pos
               (recur (1- n) (1+ pos) (+ (* acc 10) j)))))))))


(define-solution (2025 03) (banks read-joltage-ratings)
  (values
    (looping
      (doseq (bb banks)
        (sum! (find-largest-possible-joltage 2 bb))))
    (looping
      (doseq (bb banks)
        (sum! (find-largest-possible-joltage 12 bb))))))

(define-test (2025 03) (17109 169347417057382))
