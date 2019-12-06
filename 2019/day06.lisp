(defpackage :aoc/2019/06 #.cl-user::*aoc-use*)
(in-package :aoc/2019/06)

(defun parse-orbits (data &aux (orbits (make-hash-table :test 'equal)))
  (loop
    :for dep :in data
    :for (parent child) = (split-sequence:split-sequence #\) dep)
    :for children = (gethash parent orbits)
    :do (if children
          (nconc children (list child))
          (setf (gethash parent orbits) (list child))))
  orbits)

(defun total-orbits (orbits)
  (recursively ((curr "COM")
                (total 0))
    (+ total (loop
               :for child :in (gethash curr orbits)
               :summing (recur child (+ 1 total))))))

(defun find-planet (orbits target)
  (recursively ((curr "COM")
                path)
    (cond ((equal curr target) (reverse (cons target path)))
          (T (loop
               :for child :in (gethash curr orbits)
               :when (recur child (cons curr path))
               :return it)))))  

(defun you-to-santa (orbits)
  (loop
    :with path1 = (find-planet orbits "YOU")
    :with path2 = (find-planet orbits "SAN")
    :for (s1 . rest1) :on path1
    :for (s2 . rest2) :on path2
    :while (equal s1 s2)
    :finally (return (+ (length rest1)
                        (length rest2)))))

(define-problem (2019 6) (orbits parse-orbits)
  (values
    (total-orbits orbits)
    (you-to-santa orbits)))

(1am:test test-2019/06
  (multiple-value-bind (part1 part2) (problem-run)
    (1am:is (= 268504 part1))
    (1am:is (= 409 part2))))
