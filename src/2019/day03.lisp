(defpackage :aoc/2019/03 #.cl-user::*aoc-use*)
(in-package :aoc/2019/03)

(defun parse-direction (str)
  (let ((d (subseq str 0 1))
        (n (parse-integer (subseq str 1))))
    (cond ((string= "R" d) (make-list n :initial-element (complex 1)))
          ((string= "U" d) (make-list n :initial-element (complex 0 1)))
          ((string= "L" d) (make-list n :initial-element (complex (- 1) 0)))
          ((string= "D" d) (make-list n :initial-element (complex 0 (- 1)))))))

(defun read-wires (data)
  (loop
    :for wire :in data
    :for list = (split-sequence:split-sequence #\, wire)
    :for list-of-steps = (reduce #'append (mapcar #'parse-direction list))
    :collecting list-of-steps))

(defun walk (wires &optional part2 &aux (seen (make-hash-table)))
  (loop
    :for d :in (first wires)
    :for curr = d :then (+ curr d)
    :for n = 1 :then (+ n 1)
    :when (not (gethash curr seen))
    :do (setf (gethash curr seen) n))
  (loop
    :for d :in (second wires)
    :for curr = d :then (+ curr d)
    :for n = 1 :then (+ n 1)
    :for distance = (gethash curr seen)
    :when distance :collect (if part2 (+ distance n) curr) :into crosses
    :finally (return (minimization crosses :key (if part2
                                                  #'identity
                                                  (partial-1 #'manhattan-distance _ #C(0 0)))))))

(define-solution (2019 3) (wires read-wires)
  (values
    (walk wires)
    (walk wires T)))

(define-test (2019 3) (232 6084))
