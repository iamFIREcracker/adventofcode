(defpackage :aoc/2018/08 #.cl-user::*aoc-use*)
(in-package :aoc/2018/08)

(defstruct node
  children
  metadata)

(defun parse-tree (x)
  (let ((remaining (copy-seq x)))
    (recursively ()
      (let ((nchildren (pop remaining))
            (nmetadata (pop remaining)))
        (make-node
          :children (loop
                      :for i :below nchildren
                      :collecting (recur) :into ret
                      :finally (return (make-array (length ret)
                                                   :initial-contents ret)))
          :metadata (loop
                      :for i :below nmetadata
                      :collecting (pop remaining)))))))


(defun tree-value (node &aux (children (node-children node)))
  (if (zerop (length children))
    (summation (node-metadata node))
    (loop
      :for meta :in (node-metadata node)
      :for index = (1- meta)
      :when (array-in-bounds-p children index)
      :summing (tree-value (aref children index)))))


(define-problem (2018 8) (data first)
  (let ((tree (parse-tree (parse-integers (split-sequence:split-sequence #\Space data)))))
    (values
      (recursively ((node tree))
        (+
          (summation (node-children node) :key #'recur)
          (summation (node-metadata node))))
      (tree-value tree))))

(1am:test test-2018/08
  (multiple-value-bind (part1 part2) (problem-run)
    (1am:is (= 40036 part1))
    (1am:is (= 21677 part2))))
