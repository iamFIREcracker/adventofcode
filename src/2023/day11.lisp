(defpackage :aoc/2023/11 #.cl-user::*aoc-use*)
(in-package :aoc/2023/11)


(defun parse-image (&optional (expansion-size 2)
                              (strings (uiop:read-file-lines #P"src/2023/day11.txt")))
  (bnd* ((rows (length strings))
         (cols (length (first strings)))
         (image (make-hash-table :test 'equal))
         (di (make-array rows :initial-element 0))
         (dj (make-array cols :initial-element 0)))
    (flet ((row (i) (nth i strings))
           (col (j) (looping (dorange (i 0 rows) (collect! (char (nth i strings) j))))))
      ; for each row, count all the rows expansions up until that row
      (dorange (i 0 rows)
        (setf (aref di i) (looping
                            (dorange (ii 0 i)
                              (count! (every [char= _ #\.] (row ii)))))))
      ; for each col, count all the cols expansions up until that col
      (dorange (j 0 cols)
        (setf (aref dj j) (looping
                            (dorange (jj 0 j)
                              (count! (every [char= _ #\.] (col jj)))))))
      ; parse galaxies, keeping track of expansions
      (dolist+ ((i s) (enumerate strings))
        (dolist+ ((j ch) (enumerate s))
          (when (char= ch #\# )
            (bnd* ((ii (+ i (* (aref di i) (1- expansion-size))))
                   (jj (+ j (* (aref dj j) (1- expansion-size)))))
              (setf (gethash (list ii jj) image) ch)))))
      image)))


(defun sum-all-distances (&optional (image (parse-image)))
  (looping
    (dosublists ((g1 . rest) (hash-table-keys image))
      (dolist (g2 rest)
        (sum! (manhattan-distance g1 g2))))))


(define-solution (2023 11) (strings)
  (values (sum-all-distances (parse-image 2 strings))
          (sum-all-distances (parse-image 1000000 strings))))

(define-test (2023 11) (9550717 648458253817))
