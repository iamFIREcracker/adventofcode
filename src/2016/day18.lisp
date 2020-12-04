(defpackage :aoc/2016/18 #.cl-user::*aoc-use*)
(in-package :aoc/2016/18)

(defun add-gutters (string)
  (format nil ".~A." string))

(defun parse-trap-row (data &aux (string (first data)))
  (add-gutters string))

(defun its-a-trap-p (row index)
  ;; Then, a new tile is a trap only in one of the following situations:
  ;;
  ;; - Its left and center tiles are traps, but its right tile is not.
  ;; - Its center and right tiles are traps, but its left tile is not.
  ;; - Only its left tile is a trap.
  ;; - Only its right tile is a trap.
  ;;
  ;; In any other situation, the new tile is safe.
  ;;
  ;; It turns out (thanks Karnaugh) that this can be simplified to:
  ;;
  ;; - Are left/right the same? Then it's safe
  ;; - Otherwise, it's a trap
  (char/= (aref row (1- index)) (aref row (1+ index))))

(defun next (row)
  (let ((next (make-string (length row) :initial-element #\.)))
    (loop repeat (- (length row) 2)
          for index from 1 do
          (setf (aref next index) (if (its-a-trap-p row index) #\^ #\.)))
    next))

(defun generate (row times)
  (loop repeat times
        summing (count #\. row) into safe do
        (setf row (next row))
        finally (return (- safe (* times 2))))) ; offset 2 gutters per row

(define-solution (2016 18) (state parse-trap-row)
  (values
    (generate state 40)
    (generate state 400000)))

(define-test (2016 18) (1989 19999894))
