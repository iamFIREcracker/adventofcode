(defpackage :aoc/2024/05 #.cl-user::*aoc-use*)
(in-package :aoc/2024/05)

#;

(defun parse-input (&optional (strings (uiop:read-file-lines #P"src/2024/day05.txt")))
  (destructuring-bind (rules pages) (split-sequence:split-sequence "" strings :test 'equal)
    (list (aprog1 (make-hash-table)
            (doseq ((a b) (mapcar #'extract-positive-integers rules))
              (push b (gethash a it))))
            (mapcar #'extract-positive-integers pages))))
#+#:excluded (parse-input)

(defun before? (rules p1 p2)
  (recursively ((a p1) seen)
    (if (= a p2) (return-from before? t))
    (unless (member a seen)
      (dolist (b (gethash a rules))
        (recur b (cons b seen))))))

(defun correctly-ordered? (rules pages)
  (looping
    (doseqs ((a pages)
             (b (rest pages)))
      (always! (before? rules a b)))))

#+#:excluded (destructuring-bind (rules updates) (parse-input)
               (looping
                 (dolist (ok (remove-if-not [correctly-ordered? rules _] updates))
                   (sum! (@ ok (floor (length ok) 2))))))

#+#:excluded (destructuring-bind (rules updates) (parse-input)
               (looping
                 (dolist (!ok (remove-if [correctly-ordered? rules _] updates))
                   (let1 ok (sort !ok (fn (a b) (before? rules a b)))
                     (sum! (@ ok (floor (length ok) 2)))))))
