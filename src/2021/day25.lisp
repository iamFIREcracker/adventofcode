(defpackage :aoc/2021/25 #.cl-user::*aoc-use*)
(in-package :aoc/2021/25)


(defun parse-map (lines &aux (rows (length lines)) (cols (length (car lines))))
  (make-array (list rows cols) :element-type 'character :initial-contents lines))


(defun tick (curr &aux
                  (rows (array-dimension curr 0))
                  (cols (array-dimension curr 1))
                  (next (copy-array curr))
                  moved)
  (flet ((is (ch row col) (eql ch (aref curr row col)))
         (swap (row1 col1 row2 col2)
           (rotatef (aref next row1 col1) (aref next row2 col2))
           (setf moved t)))
    (dotimes (row rows)
      (dotimes (col cols)
        (let ((col-next (mod (1+ col) cols)))
          (when (and (is #\> row col) (is #\. row col-next))
            (swap row col-next row col)))))
    (setf curr next next (copy-array curr))
    (dotimes (row rows)
      (dotimes (col cols)
        (let ((row-next (mod (1+ row) rows)))
          (when (and (is #\v row col) (is #\. row-next col))
            (swap row-next col row col))))))
  (values next moved))


(defun part1 (map &aux (moved t))
  (loop while moved count t do (setf (values map moved) (tick map))))

#+#:excluded (defun print-map (rows cols map)
  (loop for row below rows do
        (loop for col below cols do
              (princ (gethash (list row col) map)))
        (terpri))
  (terpri))

(define-solution (2021 25) (input parse-map) (part1 input))

(define-test (2021 25) (532))
