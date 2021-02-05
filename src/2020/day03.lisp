(defpackage :aoc/2020/03 #.cl-user::*aoc-use*)
(in-package :aoc/2020/03)

(defun parse-map (data)
  (let ((rows (length data)) (columns (length (first data))))
    (make-array (list rows columns) :initial-contents data)))

(defun count-hits (map delta-col delta-row)
  ;; I was previously destructuring `rows` and `cols` inside the for loop:
  ;;
  ;;   (loop with (rows cols) = (array-dimensions map)
  ;;         ...
  ;;
  ;; That however gets expanded in such a way that the compiler would
  ;; throw a style warning:
  ;;
  ;; - LOOP's destructuring bind would not signal an error if the list you are
  ;; DESTRUCTURING-BIND against does not have enough elements; it would bind
  ;; remaining variables to NIL instead
  ;; - This means that both `rows` and `cols` can be NIL
  ;; - `rows` is used after the keyword `:below`, and because of that it is
  ;; expected to be a NUMBER
  ;; - But NIL is not a NUMBER, hence the warning
  ;;
  ;; Technically the compiler is right, but what a pain...
  (destructuring-bind (rows cols) (array-dimensions map)
    (loop for row below rows by delta-row
          for col = 0 then (mod (+ col delta-col) cols)
          count (char= (aref map row col) #\#))))

(define-solution (2020 3) (map parse-map)
  (values
    (count-hits map 3 1)
    (loop for (delta-col delta-row) in '((1 1) (3 1) (5 1) (7 1) (1 2))
          collect (count-hits map delta-col delta-row) into hits
          finally (return (reduce #'* hits)))))

(define-test (2020 3) (254 1666768320))
