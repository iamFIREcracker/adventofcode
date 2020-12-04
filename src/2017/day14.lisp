(defpackage :aoc/2017/14 #.cl-user::*aoc-use*
  (:import-from :aoc/2017/10 :knot-hash))
(in-package :aoc/2017/14)

(defun make-grid (data)
  (loop
    :for i :below 128
    :for string = (mkstr data #\- i)
    :for hash = (knot-hash string)
    :for binary = (hexadecimal-binary hash)
    :collect binary))

(defun init-sets (grid)
  (loop
    :for j :below (length grid)
    :for row :in grid
    :append (loop
              :for i :below (length row)
              :for c = (aref row i)
              :when (eql c #\1)
              :collect (let ((key (complex i j)))
                         (make-disjointset key)))))

(define-solution (2017 14) (data first)
  (let* ((grid (make-grid data))
         (sets (init-sets grid)))
    (values
      (loop
        :for row :in grid
        :summing (count #\1 row))
      (progn
        (loop
          :for (a . rest) :on sets
          :do (loop
                :for b :in rest
                :when (= 1 (manhattan-distance (disjointset-value a) (disjointset-value b)))
                :do (disjointset-union a b)))
        (length (distinct-disjointsets sets))))))

(define-test (2017 14) (8316 1074))
