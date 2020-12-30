(defpackage :aoc/2018/08 #.cl-user::*aoc-use*)
(in-package :aoc/2018/08)

(defstruct node
  children
  metadata)

(defun parse-tree (x)
  (let ((remaining (copy-seq x)))
    (labels ((recur ()
               (let ((nchildren (pop remaining))
                     (nmetadata (pop remaining)))
                 (make-node
                   :children (loop
                               :repeat nchildren
                               :collecting (recur) :into ret
                               :finally (return (make-array (length ret)
                                                            :initial-contents ret)))
                   :metadata (loop
                               :repeat nmetadata
                               :collecting (pop remaining))))))
      (recur))))

(defun tree-value (node &aux (children (node-children node)))
  (if (zerop (length children))
    (reduce #'+ (node-metadata node))
    (loop
      :for meta :in (node-metadata node)
      :for index = (1- meta)
      :when (array-in-bounds-p children index)
      :summing (tree-value (aref children index)))))


(define-solution (2018 8) (data first)
  (let ((tree (parse-tree (parse-integers (split-sequence:split-sequence #\Space data)))))
    (values
      (labels ((recur (node)
                 (+
                   (reduce #'+ (node-children node) :key #'recur)
                   (reduce #'+ (node-metadata node)))))
        (recur tree))
      (tree-value tree))))

(define-test (2018 8) (40036 21677))
