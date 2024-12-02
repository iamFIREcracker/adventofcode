(defpackage :aoc/2024/02 #.cl-user::*aoc-use*)
(in-package :aoc/2024/02)

(defun parse-reports (&optional (strings (uiop:read-file-lines #P"src/2024/day02.txt")))
  (looping
    (dolist (s strings)
      (collect! (mapcar #'parse-integer (split-sequence:split-sequence #\Space s))))))
#+#:excluded (parse-reports)

(defun all-increasing? (seq)
  (equal (sort (copy-seq seq) #'<) seq))

(defun all-decreasing? (seq)
  (equal (sort (copy-seq seq) #'>) seq))

(defun deltas-in-range? (seq)
  (loop for (a b . rest) on seq
        if (not b) return t
        never (and b (or (= a b) (> (abs (- a b)) 3)))))

(defun safe? (seq)
  (and (or (all-increasing? seq)
           (all-decreasing? seq))
       (deltas-in-range? seq)))
#+#:excluded (count-if #'safe? (parse-reports))
#+#:excluded 236

(defun safe-with-one-level-tolerance? (seq)
  (dotimes (i (length seq))
    (when (safe? (concatenate 'list (subseq seq 0 i) (subseq seq (1+ i))))
      (return-from safe-with-one-level-tolerance? t))))
#+#:excluded (count-if #'safe-with-one-level-tolerance? (parse-reports))
#+#:excluded 308
