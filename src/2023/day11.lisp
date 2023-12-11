(defpackage :aoc/2023/11 #.cl-user::*aoc-use*)
(in-package :aoc/2023/11)


(defun parse-image (&optional (strings (uiop:read-file-lines #P"src/2023/day11.txt")))
  (bnd* ((rows (length strings))
         (cols (length (first strings)))
         (image (make-hash-table :test 'equal))
         (ix (make-array rows :initial-element 0))
         (jx (make-array cols :initial-element 0)))
    (flet ((row (i) (nth i strings))
           (col (j) (looping (dorange (i 0 rows) (collect! (char (nth i strings) j))))))
      (dorange (i 0 rows)
        (setf (aref ix i) (or (looping
                                (dorange (ii 0 i)
                                  (count! (every [char= _ #\.] (row ii)))))
                              0)))
      (dorange (j 0 cols)
        (setf (aref jx j) (or (looping
                                (dorange (jj 0 j)
                                  (count! (every [char= _ #\.] (col jj)))))
                              0)))
      (dolist+ ((i s) (enumerate strings))
        (dolist+ ((j ch) (enumerate s))
          (when (char= ch #\# )
            (setf (gethash (list (+ i (* (aref ix i) (1- 1000000))) (+ j (* (aref jx j) (1- 1000000)))) image) ch))))
      image)))

#+#:excluded (loop for (g1 . rest) on (hash-table-keys (parse-image)) sum
                   (loop for g2 in rest sum (manhattan-distance g1 g2)))
