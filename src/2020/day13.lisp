(defpackage :aoc/2020/13 #.cl-user::*aoc-use*)
(in-package :aoc/2020/13)

(defun read-buses (string &aux (offset 0) buses)
  (dolist (item (cl-ppcre:split "," string) buses)
    (when (string/= item "x")
      (push (cons (parse-integer item) offset) buses))
    (incf offset)))
(defun id (bus) (car bus))
(defun offset (bus) (cdr bus))

(defun read-notes (data)
  (cons (parse-integer (first data)) (read-buses (second data))))
(defun earliest-departure (notes) (car notes))
(defun buses (notes) (cdr notes))

(defun wait-time (earliest-departure bus-period)
  (let ((mod (mod earliest-departure bus-period)))
    (if (zerop mod) 0 (- bus-period mod))))

(defun part1 (notes)
  (multiple-value-call #'*
    (find-min
      (mapcar #'id (buses notes))
      :key (partial-1 #'wait-time (earliest-departure notes)))))

(defun part2 (buses)
  (loop with step = 1 and ts = 0
        for (id . offset) in buses do
        (loop until (zerop (wait-time (+ ts offset) id))
              do (incf ts step))
        (mulf step id)
        finally (return ts)))

(define-solution (2020 13) (notes read-notes)
  (values (part1 notes) (part2 (buses notes))))

(define-test (2020 13) (203 905694340256752))
