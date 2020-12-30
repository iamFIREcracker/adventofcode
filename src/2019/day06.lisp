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
  (labels ((recur (curr total)
             (+ total (loop
                        :for child :in (gethash curr orbits)
                        :summing (recur child (+ 1 total))))))
    (recur "COM" 0)))

(defun find-planet-path (orbits target)
  (labels ((recur (curr path)
             (cond ((equal curr target) (reverse (cons target path)))
                   (T (loop
                        :for child :in (gethash curr orbits)
                        :when (recur child (cons curr path))
                        :return it)))))
    (recur "COM" nil)))

(defun you-to-santa (orbits)
  (loop
    :with path1 = (find-planet-path orbits "YOU")
    :with path2 = (find-planet-path orbits "SAN")
    :for (s1 . rest1) :on path1
    :for (s2 . rest2) :on path2
    :while (equal s1 s2)
    :finally (return (+ (length rest1)
                        (length rest2)))))

(define-solution (2019 6) (orbits parse-orbits)
  (values
    (total-orbits orbits)
    (you-to-santa orbits)))

(define-test (2019 6) (268504 409))
