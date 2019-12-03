(defpackage :aoc/2019/03 #.cl-user::*aoc-use*)
(in-package :aoc/2019/03)

(defun steps (d n)
  "XXX
  CL-USER> (make-list 10 :initial-element 8)
   (8 8 8 8 8 8 8 8 8 8)"
  (cond ((string= "R" d) (loop :for i :to (1- n) :collecting (complex 1)))
        ((string= "U" d) (loop :for i :to (1- n) :collecting (complex 0 1)))
        ((string= "L" d) (loop :for i :to (1- n) :collecting (complex (- 1) 0)))
        ((string= "D" d) (loop :for i :to (1- n) :collecting (complex 0 (- 1))))))

(defun parse-direction (str)
  (let ((d (subseq str 0 1))
        (n (parse-integer (subseq str 1))))
    (steps d n)))

(defun flatten (ls)
  "Borrowed from Paul Graham's OnLisp"
  (labels ((mklist (x) (if (listp x) x (list x))))
    (mapcan #'(lambda (x) (if (atom x) (mklist x) (flatten x))) ls)))

(defun read-wires (data)
  (loop
    :for str :in data
    :for list = (split-sequence:split-sequence #\, str)
    :for list-of-steps = (mapcar #'parse-direction list)
    :collecting (flatten list-of-steps)))

(defun solve-part1 (wires &aux (seen (make-hash-table)))
  (loop
    :with curr = #C(0 0)
    :for d :in (first wires)
    :do (incf curr d)
    :do (setf (gethash curr seen) curr))
  (loop
    :with curr = #C(0 0)
    :for d :in (second wires)
    :do (incf curr d)
    :when (gethash curr seen)
    :collect it :into cross
    :finally (return (minimization cross :key (partial-1 #'manhattan-distance _ #C(0 0))))))

(defun solve-part2 (wires &aux (seen (make-hash-table)))
  (loop
    :with curr = #C(0 0)
    :for d :in (first wires)
    :for n = 1 :then (+ n 1)
    :do (incf curr d)
    :when (not (gethash curr seen))
    :do (setf (gethash curr seen) n))
  (loop
    :with curr = #C(0 0)
    :for d :in (second wires)
    :for n = 1 :then (+ n 1)
    :do (incf curr d)
    :when (gethash curr seen)
    :collect (+ n (gethash curr seen)) :into cross
    :finally (return (minimization cross))))

(define-problem (2019 3) (wires read-wires)
  (values
    (solve-part1 wires)
    (solve-part2 wires)))

(1am:test test-2019/03
  (multiple-value-bind (part1 part2) (problem-run)
    (1am:is (= 232 part1))
    (1am:is (= 6084 part2))))
