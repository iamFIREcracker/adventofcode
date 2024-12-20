(defpackage :aoc/2024/19 #.cl-user::*aoc-use*)
(in-package :aoc/2024/19)

(defun parse-input (&optional (strings (uiop:read-file-lines #P"src/2024/day19.txt")))
  (destructuring-bind ((patterns) designs) (split-sequence:split-sequence "" strings :test 'string=)
    (list (cl-ppcre:split ", " patterns)
          designs)))
#+#:excluded (parse-input)


(defun design-possible? (patterns design)
  (let1 memo (make-hash-table :test 'equal)
    (recursively ((start 0))
      (memoizing (memo start)
        (cond ((= (length design) start) 1)
              ((null patterns) 0)
              (t (looping
                   (dolist (p patterns)
                     (when (string-starts-with-p p design :start start)
                       (sum! (recur (+ (length p) start))))))))))))


(define-solution (2024 19) (input parse-input)
  (destructuring-bind (patterns designs) input
    (values (count-if #'plusp designs :key [design-possible? patterns _])
            (reduce #'+ designs :key [design-possible? patterns _]))))

(define-test (2024 19) (350 769668867512623))
