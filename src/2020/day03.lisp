(defpackage :aoc/2020/03 #.cl-user::*aoc-use*)
(in-package :aoc/2020/03)

(defun parse-map (data)
  (let ((rows (length data)) (columns (length (first data))))
    (make-array (list rows columns) :initial-contents data)))

(defun count-hits (map delta-col delta-row)
  (loop with (rows cols) = (array-dimensions map)
        for row below rows by delta-row
        for col = 0 then (mod (+ col delta-col) cols)
        count (char= (aref map row col) #\#)))

(define-solution (2020 3) (map parse-map)
  (values
    (count-hits map 3 1)
    (loop for (delta-col delta-row) in '((1 1) (3 1) (5 1) (7 1) (1 2))
          collect (count-hits map delta-col delta-row) into hits
          finally (return (reduce #'* hits)))))

(define-test (2020 3) (254 1666768320))
