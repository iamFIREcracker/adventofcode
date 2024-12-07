(defpackage :aoc/2024/07 #.cl-user::*aoc-use*)
(in-package :aoc/2024/07)

(defun parse-equations (&optional (strings (uiop:read-file-lines #P"src/2024/day07.txt")))
  (mapcar #'extract-positive-integers strings))
#+#:excluded (parse-equations)

(defun solvable? (test numbers &optional (operators '(+ *)))
  (recursively ((test test) (numbers numbers))
    (cond ((= (length numbers) 1) (= (first numbers) test))
          (t (destructuring-bind (a b . numbers1) numbers
               (looping
                 (dolist (op operators)
                   (let1 x (funcall op a b)
                     (thereis! (recur test
                                      (cons x numbers1)))))))))))


(defun || (a b)
  (parse-integer (spr a b)))


(define-solution (2024 7) (input parse-equations)
  (values (looping
            (doseq ((test . numbers) input)
              (when (solvable? test numbers)
                (sum! test))))
          (looping
            (doseq ((test . numbers) input)
              (when (solvable? test numbers '(+ * ||))
                (sum! test))))))

(define-test (2024 07) (1298300076754 248427118972289))
