(defpackage :aoc/2022/14 #.cl-user::*aoc-use*)
(in-package :aoc/2022/14)


#+#:excluded (&optional (program (uiop:read-file-forms #P"src/2022/day14.txt")))
#+#:excluded (uiop:read-file-forms #P"scratch.txt")


(defun parse-cave (&optional (file #P"src/2022/day14.txt"))
  (let ((cave (make-hash-table :test 'equal)))
    (dolist (s (uiop:read-file-lines file))
      (loop for (from-col from-row to-col to-row) on (extract-positive-integers s) by #'cddr
            while to-row do  (loop with delta-row = (<=> to-row from-row)
                                   with delta-col = (<=> to-col from-col)
                                   for row = from-row then (+ row delta-row)
                                   for col = from-col then (+ col delta-col)
                                   do (setf (gethash (list row col) cave) #\# ) ; FTF? Space
                                   while (or (/= row to-row) (/= col to-col)))))
    cave))

;; parsing digits... not numbers
;; while and instead of while or
#+#:excluded (parse-cave #P"scratch.txt")
(defun print-cave (cave)
  (loop for (row col) being the hash-keys of cave
        minimize row into row-min maximize row into row-max
        minimize col into col-min maximize col into col-max
        finally (loop for row from (- row-min 3) to (+ row-max 3)
                      do (loop for col from (- col-min 8) to (+ col-max 8)
                               do (princ (aif (gethash (list row col) cave) it #\Space)))
                      (terpri))))
#+#:excluded (print-cave (parse-cave #P"scratch.txt"))

(defun deposit (cave row-max row col)
  (if (> row row-max)
    (return-from deposit))
  (if (gethash (list 0 500) cave)
    (return-from deposit))
  (loop for (delta-row delta-col) in '((1 0) (1 -1) (1 1))
        for row-next = (+ row delta-row) for col-next = (+ col delta-col)
        if (and (not (gethash (list row-next col-next) cave))
             (/= row-next row-max)) do (return-from deposit (deposit cave row-max row-next col-next)))

  (setf (gethash (list row col) cave) #\o)

  t)

#+#:excluded (defun adjust-cave (cave)
               (loop for (r c) being the hash-keys of cave
                     minimize r into row-min maximize r into row-max
                     minimize c into col-min maximize c into col-max
                     finally (loop for c from (- col-min 10) to (+ col-max 10)
                                   do (setf (gethash (list (+ row-max 2) c) cave) #\# )))
               cave)
#+#:excluded (adjust-cave (parse-cave #P"scratch.txt"))
#+#:excluded (print-cave *)
#;
(loop with cave = (parse-cave #P"scratch.txt")
      with row-max = (loop for (r) being the hash-keys of cave maximize r)
      while (deposit cave row-max 0 500) sum 1)
(loop with cave = (parse-cave #+#:excluded #P"scratch.txt")
      with row-max = (+ (loop for (r) being the hash-keys of cave maximize r) 2)
      while (deposit cave row-max 0 500) sum 1)
; (loop with cave = (parse-cave #P"scratch.txt") while (deposit cave (loop for (r) being the hash-keys of cave maximize r) 0 500) sum 1)
