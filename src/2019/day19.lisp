(defpackage :aoc/2019/19 #.cl-user::*aoc-use*)
(in-package :aoc/2019/19)

(defstruct (drone (:constructor make-drone%))
  program
  in
  out)

(defun make-drone (program)
  (let* ((program (intcode:make-program (copy-hash-table (intcode:program-memory program)))))
    (make-drone% :program program
                 :in (intcode:program-in program)
                 :out (intcode:program-out program))))

(defun pulledp (drone pos)
  (enqueue (realpart pos) (drone-in drone))
  (enqueue (- (imagpart pos)) (drone-in drone))
  (intcode:program-run (drone-program drone))
  (= 1 (dequeue (drone-out drone))))

(defun beam-upper-border (map)
  ;; XXX figure out a way to calculate deltas
  (declare (ignore map))
  (let ((pos #C(4 -3))
        (deltas (ncycle (list #C(1 -1) #C(1 -1) #C(1 -1) #C(1 0)))))
    (lambda ()
      (let ((next-pos (+ pos (first deltas))))
        (setf deltas (cdr deltas)
              pos next-pos)))))

(define-solution (2019 19) (program intcode:read-program)
  (let* ((map (make-hash-table)))
    (dorange (i 0 50)
      (dorange (j 0 50)
        (let ((pos (complex i (- j))))
          (hash-table-insert map pos (if (pulledp (make-drone program) pos) #\# #\.)))))
    (values
      (count #\# (hash-table-values map))
      (loop
        :with size = 100
        :with points-gen = (beam-upper-border map)
        :for top-right = (funcall points-gen)
        :for top-left = (- top-right (1- size))
        :for bottom-left = (- top-left (complex 0 (1- size)))
        :when (and (pulledp (make-drone program) top-right)
                   (pulledp (make-drone program) top-left)
                   (pulledp (make-drone program) bottom-left))
        :return (+ (* 10000 (realpart top-left)) (- (imagpart top-left)))))))

(define-test (2019 19) (220 10010825))
