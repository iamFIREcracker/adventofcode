(defpackage :aoc/2019/11 #.cl-user::*aoc-use*)
(in-package :aoc/2019/11)

(defstruct (robot (:constructor make-robot%))
  program
  in
  out
  direction)

(defun make-robot (program)
  (let* ((in (make-queue))
         (out (make-queue))
         (program (intcode:make-program (copy-hash-table (intcode:program-memory program)) in out)))
    (make-robot% :program program :in in :out out :direction #C(0 1))))

(defun robot-rotate (robot dir-change)
  (setf (robot-direction robot) (case dir-change
                                  (0 (complex-rotate-ccw (robot-direction robot)))
                                  (1 (complex-rotate-cw (robot-direction robot))))))

(defun robot-run (robot input)
  (enqueue input (robot-in robot))
  (let ((still-running (intcode:program-run (robot-program robot))))
    (when still-running
      (prog1
        (dequeue (robot-out robot))
        (let ((dir-change (dequeue (robot-out robot))))
          (robot-rotate robot dir-change))))))

(defun paint-panels (initial-color robot &aux (panels (make-hash-table)))
  (prog1 panels
    (loop
      :with pos = 0
      :initially (hash-table-insert panels pos initial-color)
      :for input = (gethash pos panels 0)
      :for color = (robot-run robot input)
      :while color
      :do (hash-table-insert panels pos color)
      :do (incf pos (robot-direction robot))
      :finally (return panels))))


(defun print-registration-identifier (panels)
  (let ((min-x (minimization (hash-table-keys panels) :key #'realpart))
        (max-x (maximization (hash-table-keys panels) :key #'realpart))
        (min-y (minimization (hash-table-keys panels) :key #'imagpart))
        (max-y (maximization (hash-table-keys panels) :key #'imagpart)))
    (doirange (y max-y min-y -1)
      (doirange (x min-x max-x)
        (format T "~a" (if (eql 1 (gethash (complex x y) panels))
                         #\#
                         #\Space)))
      (format T "~&"))))

(define-problem (2019 11) (program intcode:read-program)
  (values
    (hash-table-count (paint-panels 0 (make-robot program)))
    (print-registration-identifier (paint-panels 1 (make-robot program)))))

(1am:test test-2019/11
  (multiple-value-bind (part1 part2) (problem-run)
    (declare (ignore part2)) ; XXX figure out a way to test for: ZCGRHKLB
    (1am:is (= 1747 part1))))
