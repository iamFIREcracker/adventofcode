(defpackage :aoc/2024/05 #.cl-user::*aoc-use*)
(in-package :aoc/2024/05)

(defun parse-input (&optional (strings (uiop:read-file-lines #P"src/2024/day05.txt")))
  (destructuring-bind (rules updates) (split-sequence:split-sequence "" strings :test 'equal)
    (list (prog1-let (before-than (make-hash-table))
            (doseq ((a b) (mapcar #'extract-positive-integers rules))
              (push b (gethash a before-than))))
          (mapcar #'extract-positive-integers updates))))

;; The problem description clearly states that any ordering rule involving pages
;; not included in the update should not be included, so we just have to rely
;; on the explicit rules, without any recursion.
(defun before? (rules p1 p2)
  (member p2 (gethash p1 rules)))

(defun correctly-ordered? (rules update)
  (looping
    (doseqs ((a update)
             (b (rest update)))
      (always! (before? rules a b)) )))

(defun middle-page (update)
  (nth (floor (length update) 2) update))


(define-solution (2024 5) (input parse-input)
  (destructuring-bind (rules updates) input
    (multiple-value-bind (ok !ok) (partition-if [correctly-ordered? rules %] updates)
      (values (reduce #'+ ok :key 'middle-page)
              (flet ((sort-pages (update)
                       (sort update [before? rules %1 %2])))
                (reduce #'+ (mapcar #'sort-pages !ok) :key 'middle-page))))))

(define-test (2024 5) (4689 6336))
