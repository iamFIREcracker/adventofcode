(defpackage :aoc/2017/12 #.cl-user::*aoc-use*)
(in-package :aoc/2017/12)

(defstruct program-links
  from
  linked)

(defun parse-program-links (x)
  (flet ((parse-program-links (s &aux (splits (split-sequence:split-sequence #\Space s)))
           (let ((from (parse-integer (first splits)))
                 (linked (mapcar (partial-1 #'parse-integer _ :junk-allowed T) (rest (rest splits)))))
             (make-program-links :from from
                                 :linked linked))))
    (mapcar #'parse-program-links x)))

(define-solution (2017 12) (data parse-program-links)
  (flet ((init-groups (&aux (sets (make-hash-table)))
           (dolist (pipe data sets)
             (let ((from (program-links-from pipe)))
               (hash-table-insert sets from (make-disjointset from)))))
         (join-groups (groups from linked)
           (dolist (to linked)
             (disjointset-union (gethash from groups)
                                (gethash to groups)))))
    (loop
      :with groups = (init-groups)
      :with group-0 = (gethash 0 groups)
      :for link :in data
      :for from = (program-links-from link)
      :for linked = (program-links-linked link)
      :do (join-groups groups from linked)
      :finally (return (let ((sets (hash-table-values groups)))
                         (values
                           (count (disjointset-find group-0)
                                  sets
                                  :key #'disjointset-find
                                  :test 'eq)
                           (length (distinct-disjointsets sets))))))))

(define-test (2017 12) (378 204))
