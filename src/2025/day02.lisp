(defpackage :aoc/2025/02 #.cl-user::*aoc-use*)
(in-package :aoc/2025/02)

(defun read-ranges (&optional (strings (uiop:read-file-lines #P"src/2025/day02.txt")))
  (looping
    (dolist (s (split-sequence:split-sequence #\, (first strings)))
      (destructuring-bind (a b) (split-sequence:split-sequence #\- s)
        (collect! (list (parse-integer a)
                        (parse-integer b)))))))

(defun sum-ids-if (&optional (ranges (read-ranges)) (predicate 'invalid?))
  (looping
    (doseq ((a b) ranges)
      (dorangei (n a b)
        (when (funcall predicate n)
          (sum! n))))))


(defun invalid? (n)
  (let1 s (spr n)
    (and (evenp (length s))
         (destructuring-bind (a b) (split s (/ (length s) 2))
           (string= a b)))))

(defun invalid-p2? (n &aux (s (spr n)))
  (looping
    (dorange (d 1 (length s))
      (when (dividesp d (length s))
        (destructuring-bind (first . rest) (subdivide s d)
          (thereis!
            (looping
              (dolist (other rest)
                (always! (equal first other))))))))))


(define-solution (2025 02) (ranges read-ranges)
  (values (sum-ids-if ranges 'invalid?)
          (sum-ids-if ranges 'invalid-p2?)))

(define-test (2025 02) (5398419778 15704845910))
