(defpackage :aoc/2024/19 #.cl-user::*aoc-use*)
(in-package :aoc/2024/19)

#;
(defun parse-input (&optional (strings (uiop:read-file-lines #P"src/2024/day19.txt")))
  (destructuring-bind ((patterns) designs) (split-sequence:split-sequence "" strings :test 'string=)
    (list (cl-ppcre:split ", " patterns)
          designs)))
#+#:excluded (parse-input)

(destructuring-bind (patterns designs) (parse-input)
  (looping
    (dolist (design designs)
      (let1 memo (make-hash-table :test 'equal)
        (labels ((recur (patterns design)
                   (memoizing (memo patterns design)
                     (cond ((zerop (length design)) t)
                           ((null patterns) nil)
                           (t (looping
                                (dolist (p patterns)
                                  (when (string-starts-with-p p design)
                                    (thereis! (recur patterns (subseq design (length p))))))))))))
          (count! (recur patterns design)))))))

(destructuring-bind (patterns designs) (parse-input)
  (looping
    (dolist (design designs)
      (let1 memo (make-hash-table :test 'equal)
        (labels ((recur (patterns design)
                   (memoizing (memo patterns design)
                     (cond ((zerop (length design)) 1)
                           ((null patterns) 0)
                           (t (looping
                                (dolist (p patterns)
                                  (when (string-starts-with-p p design)
                                    (sum! (recur patterns (subseq design (length p))))))))))))
          (sum! (recur patterns design)))))))
