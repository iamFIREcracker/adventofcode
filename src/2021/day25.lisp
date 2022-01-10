(defpackage :aoc/2021/25 #.cl-user::*aoc-use*)
(in-package :aoc/2021/25)

; (uiop:read-file-form "src/2021/day25.txt")
; (uiop:read-file-forms "src/2021/day25.txt")
; (uiop:read-file-line "src/2021/day25.txt")

(defun parse-map (data &aux
                       (rows (length data))
                       (cols (length (car data)))
                       (map (make-hash-table :test 'equal)))
  (loop for r below rows
        for string in data do
        (loop for c below cols
              for ch across string do
              (setf (gethash (list r c) map) ch)))
  (cons (cons rows cols) map))

(defun tick (rows cols curr &aux moved)
  (let ((next (copy-hash-table curr)))
    (flet ((is-. (row col) (eql (gethash (list row col) curr) #\.))
           (is-> (row col) (eql (gethash (list row col) curr) #\>))
           (is-v (row col) (eql (gethash (list row col) curr) #\v)))
      (loop for row below rows do
            (loop for col below cols
              for col-next = (mod (1+ col) cols)
              when (and (is-> row col) (is-. row col-next)) do
              (setf moved t
                    (gethash (list row col-next) next) #\>
                    (gethash (list row col) next) #\.)))
      (setf curr next next (copy-hash-table curr))
      (loop for row below rows do
            (loop for col below cols
              for row-next = (mod (1+ row) rows)
              when (and (is-v row col) (is-. row-next col)) do
              (setf moved t
                    (gethash (list row-next col) next) #\v
                    (gethash (list row col) next) #\.)))
      (values next moved))))

(defun part1 (input &aux (moved t))
  (destructuring-bind ((rows . cols) . map) input
    (loop while moved count t do (setf (values map moved) (tick rows cols map)) #+#:excluded (print-map rows cols map))))

(defun print-map (rows cols map)
  (loop for row below rows do
        (loop for col below cols do
              (princ (gethash (list row col) map)))
        (terpri))
  (terpri))
#+#:excluded (time (part1 (parse-map (uiop:read-file-lines "src/2021/day25.txt"))))
#+#:excluded (define-solution (2021 25) (octopus parse-octopuses) (flash-dance octopuses))

#+#:excluded (define-test (2021 25) (1665 234))
#+#:excluded (time (test-run))
#+#:excluded (sb-ext:gc)
