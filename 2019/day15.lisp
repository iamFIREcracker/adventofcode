(defpackage :aoc/2019/15 #.cl-user::*aoc-use*)
(in-package :aoc/2019/15)

(defparameter *interactive* NIL)

(defstruct (droid (:constructor make-droid%))
  program
  in
  out)

(defun make-droid (program)
  (let* ((program (intcode:make-program (copy-hash-table (intcode:program-memory program)))))
    (make-droid% :program program
                 :in (intcode:program-in program)
                 :out (intcode:program-out program))))

(defun next-dir (dir)
  (case dir
    (3 1)
    (1 4)
    (4 2)
    (2 3)))

(defun opposite-dir (dir)
  (case dir
    (3 4)
    (1 2)
    (4 3)
    (2 1)))

(defun next-pos (pos dir)
  ;;; why does this throw a warning?!
  (let* ((shift (case dir
                  (3 #C(-1 0))
                  (1 #C(0 1))
                  (4 #C(1 0))
                  (2 #C(0 -1))))
         (next-pos (+ pos shift)))
    next-pos))

(defun left-of (dir)
  (case dir
    (3 2)
    (1 3)
    (4 1)
    (2 4)))

(defun backwardp (pos next-pos back)
  (multiple-value-bind (value exists)
      (gethash pos back)
    (when exists
      (= next-pos value))))

(defun next-pos-and-dir (pos dir back)
  (let ((next-pos (next-pos pos dir)))
    (values
      next-pos
      (if (backwardp pos next-pos back)
        (next-dir (opposite-dir dir))
        (left-of dir)))))

(defun print-map (start oxygen h)
  (print-hash-table-map h (lambda (value pos)
                            (cond ((= start pos) #\S)
                                  ((= oxygen pos) #\O)
                                  ((null value) #\#)
                                  (T value)))))

(defun explore (droid)
  (loop
    :with map = (make-hash-table)
    :with program = (droid-program droid)
    :with back = (make-hash-table)
    :with start = 0
    :with oxygen
    :with pos = start
    :with dir = 3
    :initially (enqueue dir (droid-in droid))
    :initially (setf (gethash 0 map) #\S)
    :for time = 1 :then (1+ time)
    :for running = (intcode:program-run program)
    :for status = (dequeue (droid-out droid))
    :when (= status 0) :do (setf (gethash (next-pos pos dir) map) #\#
                                 dir (next-dir dir))
    :when (= status 1) :do (multiple-value-bind (next-pos next-dir)
                               (next-pos-and-dir pos dir back)
                             (unless (gethash next-pos back)
                               (setf (gethash next-pos back) pos))
                             (setf (gethash pos map) #\.
                                   pos next-pos
                                   dir next-dir))
    :when (= status 2) :do (multiple-value-bind (next-pos next-dir)
                               (next-pos-and-dir pos dir back)
                             (setf oxygen pos)
                             (unless (gethash next-pos back)
                               (setf (gethash next-pos back) pos))
                             (setf (gethash pos map) #\.
                                   pos next-pos
                                   dir next-dir))
    :while (< time 5000) :do (enqueue dir (droid-in droid))
    :finally (return (values start oxygen map))))

(defun neighbors (map pos)
  (loop
    :for dir :in (list 1 2 3 4)
    :for next-pos = (next-pos pos dir)
    :for value = (gethash next-pos map)
    :unless (eql value #\#)
    :collect next-pos))

(define-problem (2019 15) (program intcode:read-program)
  (let ((droid (make-droid program)))
    (multiple-value-bind (start oxygen map)
        (explore droid)
      (values
        (multiple-value-bind (end-state cost-so-far)
            (a-star start
                    :goal-state oxygen
                    :neighbors (a-star-neighbors-cost-auto-increment (partial-1 #'neighbors map))
                    :heuristic (partial-1 #'manhattan-distance _ oxygen))
          (1+ (gethash end-state cost-so-far))) ;; XXX why 1+?
        (multiple-value-bind (end-state cost-so-far)
            (bfs oxygen
                 :neighbors (partial-1 #'neighbors map))
          (declare (ignore end-state))
          (1+ (maximization (hash-table-values cost-so-far)))))))) ;; XXX why 1+?

(1am:test test-2019/15
  (multiple-value-bind (part1 part2) (problem-run)
    (1am:is (= 336 part1))
    (1am:is (= 360 part2))))
