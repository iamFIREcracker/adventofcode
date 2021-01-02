(defpackage :aoc/2015/06 #.cl-user::*aoc-use*)
(in-package :aoc/2015/06)

(defun parse-coordinate-pair (string)
  (cl-ppcre:register-groups-bind ((#'parse-integer top left bottom right))
      ("(\\d+),(\\d+) through (\\d+),(\\d+)" string)
    (list top left bottom right)))

(defun parse-instruction (string)
  (cl-ppcre:register-groups-bind (action (#'parse-coordinate-pair pair))
      ("(?:turn )?(\\w+) (.+)" string)
    (cons (make-keyword (string-upcase action)) pair)))

(defun parse-instructions (lines)
  (mapcar #'parse-instruction lines))

(defun make-lights-grid (instructions)
  (let ((grid (make-array '(1000 1000) :initial-element nil)))
    (loop for (action top left bottom right) in instructions do
          (loop for row from top upto bottom do
                (loop for col from left upto right
                      for already-lit = (aref grid row col) do
                      (ecase action
                        (:toggle (setf (aref grid row col) (not already-lit)))
                        (:on (setf (aref grid row col) t))
                        (:off (setf (aref grid row col) nil))))))
    grid))

(defun part1 (instructions)
  (count t (array-elements (make-lights-grid instructions))))

(defun make-improved-lights-grid (instructions)
  (let ((grid (make-array '(1000 1000) :initial-element 0)))
    (loop for (action top left bottom right) in instructions do
          (loop for row from top upto bottom do
                (loop for col from left upto right do
                      (ecase action
                        (:toggle (incf (aref grid row col) 2))
                        (:on (incf (aref grid row col) 1))
                        (:off (unless (zerop (aref grid row col))
                                (decf (aref grid row col) 1)))))))
    grid))

(defun part2 (instructions)
  (reduce #'+ (array-elements (make-improved-lights-grid instructions))))

(define-solution (2015 6) (instructions parse-instructions)
  (values (part1 instructions) (part2 instructions)))

(define-test (2015 6) (569999 17836115))
