(defpackage :aoc/2024/07 #.cl-user::*aoc-use*)
(in-package :aoc/2024/07)

#;
(defun parse-equations (&optional (strings (uiop:read-file-lines #P"src/2024/day07.txt")))
  (mapcar #'extract-positive-integers strings))
#+#:excluded (parse-equations)

(defun || (a b)
  (parse-integer (spr a b)))

(defun solvable? (test numbers &optional (operators '(+ * ||)))
  (recursively ((test test) (numbers numbers))
    (cond ((= (length numbers) 1) (= (first numbers) test))
          ((minusp test) nil)
          (t (destructuring-bind (a b . numbers1) numbers
               (looping
                 (dolist (op operators)
                   (let1 x (funcall op a b)
                     (thereis! (recur test
                                      (cons x numbers1)))))))))))

(looping
  (doseq ((test . numbers) (parse-equations))
    (when (solvable? test numbers)
      (sum! test))))
