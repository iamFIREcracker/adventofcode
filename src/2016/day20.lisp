(defpackage :aoc/2016/20 #.cl-user::*aoc-use*)
(in-package :aoc/2016/20)

(defparameter *ip-max* 4294967295)

(defun parse-range (string)
  (mapcar #'parse-integer (split-sequence:split-sequence #\- string)))

(defun parse-ranges (data)
  (mapcar #'parse-range data))

(defun range< (range1 range2)
  (destructuring-bind (left1 right1) range1
    (destructuring-bind (left2 right2) range2
      (or (< left1 left2)
          (and (= left1 left2)
               (<= right1 right2))))))

(defun merge-overlapping (ranges)
  (let ((sorted (sort (copy-seq ranges) #'range<)))
    (loop with (merged-start merged-end) = (first sorted)
          for (start end) in (rest sorted)
          if (> (1- start) merged-end)
            collect (list merged-start merged-end) into ranges
            and do (setf merged-start start merged-end end)
          else do (setf merged-end (max end merged-end))
          finally (return (append ranges
                                  (list (list merged-start merged-end)))))))

(define-solution (2016 20) (ranges parse-ranges)
  (loop with part1 with part2 = (1+ *ip-max*)
        for (start end) in (merge-overlapping ranges)
        for size = (1+ (- end start)) do
        (when (not part1) (setf part1 (1+ end)))
        (decf part2 size)
        finally (return (values part1 part2))))

(define-test (2016 20) (31053880 117))
