(defpackage :aoc/2023/09 #.cl-user::*aoc-use*)
(in-package :aoc/2023/09)

'(&optional (strings (uiop:read-file-lines #P"src/2023/day09.txt")))

(defun deltas (nums)
  (loop for n in nums by #'cdr for m in (cdr nums) by #'cdr
        collect (- m n)))

(defun extrapolate (nums)
  (looping
    (while (some (complement #'zerop) nums)
      (sum! (car (last nums)))
      (setf nums (deltas nums)))))
; (mapcar #'extract-integers (uiop:read-file-lines #P"src/2023/day09.txt"))
; (mapcar #'extrapolate *)
; (reduce #'+ *)
; 1930746032

(defun extrapolate-2 (nums)
  (bnd1 (dd (looping
              (while (some (complement #'zerop) nums)
                (collect! (first nums))
                (setf nums (deltas nums)))))
    (bnd1 (num 0)
      (dolist (d (reverse dd) num)
        (setf num (- d num))))))
; (mapcar #'extract-integers (uiop:read-file-lines #P"src/2023/day09.txt"))
; (mapcar #'extrapolate-2 *)
; (reduce #'+ *)
