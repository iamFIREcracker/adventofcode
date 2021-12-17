(defpackage :aoc/2017/14 #.cl-user::*aoc-use*
  (:import-from :aoc/2017/10 :knot-hash))
(in-package :aoc/2017/14)

(defun make-grid (data)
  (loop
    :for i :below 128
    :for string = (mkstr data #\- i)
    :for hash = (knot-hash string)
    :for binary = (hexadecimal->binary hash)
    :collect binary))

(defun init-sets (grid)
  (loop
    :for j :below (length grid)
    :for row :in grid
    :append (loop
              :for i :below (length row)
              :for c = (aref row i)
              :when (eql c #\1)
              :collect (make-dset (complex i j)))))

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
                :when (= 1 (manhattan-distance (dset-value a) (dset-value b)))
                :do (dset-union a b))
          :finally (return (length
                             (remove-duplicates
                               sets :key #'dset-find :test 'eq))))))))

(define-test (2017 14) (8316 1074))
