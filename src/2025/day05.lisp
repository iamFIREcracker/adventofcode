(defpackage :aoc/2025/05 #.cl-user::*aoc-use*)
(in-package :aoc/2025/05)

(defun read-ingredients-database (&optional (strings (uiop:read-file-lines #P"src/2025/day05.txt")))
  (destructuring-bind (ranges ids)
                      (split-sequence:split-sequence "" strings :test 'equal)
    (list
      (looping
        (dolist (s ranges)
          (destructuring-bind (a b) (split-sequence:split-sequence #\- s)
            (collect! (list (parse-integer a) (parse-integer b))))))
      (mapcar #'parse-integer ids))))


(defun fresh? (ranges id)
  (looping
    (doseq ((a b) ranges)
      (thereis! (<= a id b)))))


(defun overlap? (r1 r2)
  (destructuring-bind (a1 b1) r1
    (destructuring-bind (a2 b2) r2
      (and (<= a1 b2)
           (>= b1 a2)))))

(defun %merge (r1 r2)
  (destructuring-bind (a1 b1) r1
    (destructuring-bind (a2 b2) r2
      (list (min a1 a2)
            (max b1 b2)))))

(defun simplify (ranges)
  (prog1-let ranges2 nil
    (dolist (r (sort (copy-seq ranges) '< :key #'car))
      (cond ((not ranges2) (push r ranges2))
            ((overlap? (car ranges2) r) (setf (car ranges2) (%merge (car ranges2) r)))
            (t (push r ranges2))))))


(define-solution (2025 05) (db read-ingredients-database)
  (destructuring-bind (ranges ids) db
    (values
      (looping
        (dolist (id ids)
          (count! (fresh? ranges id))))
      (looping
        (doseq ((a b) (simplify ranges))
          (sum! (1+ (- b a))))))))

(define-test (2025 05) (598 360341832208407))

