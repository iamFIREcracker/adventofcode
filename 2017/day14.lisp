(defpackage :aoc/2017/14 #.cl-user::*aoc-use*)
(in-package :aoc/2017/14)

(defun binary-string (seq &aux ret)
  (dovector (c seq (apply 'mkstr (nreverse ret))) ;; XXX rewrite this, please
    (push (format NIL "~4,'0b" (parse-integer (mkstr c) :radix 16)) ret)))

(defun make-grid (data)
  (loop
    :for i :upto 127
    :for string = (mkstr data #\- i)
    :for hash = (aoc/2017/10:knot-hash string)
    :for binary = (binary-string hash)
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

(define-problem (2017 14) (data first)
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
        (length
          (remove-duplicates sets
                              :key #'disjointset-find
                              :test 'eq))))))

(1am:test test-2017/14
  (multiple-value-bind (part1 part2) (problem-run)
    (1am:is (= 8316 part1))
    (1am:is (= 1074 part2))))
