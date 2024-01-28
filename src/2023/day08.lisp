(defpackage :aoc/2023/08 #.cl-user::*aoc-use*)
(in-package :aoc/2023/08)

(defun parse-input (&optional (strings (aoc::read-problem-input 2023 08)))
  (destructuring-bind (instructions _ . network) strings
    (declare (ignore _))
    (flet ((node (s) (mapcar #'as-keyword (cl-ppcre:all-matches-as-strings "\\w+" s))))
      (cons (ncycle (coerce instructions 'list))
            (mapcar #'node network)))))


(defun count-steps (start end? &optional (input (parse-input)))
  (destructuring-bind (instructions . network) input
    (recursively ((curr start)
                  (instructions instructions)
                  (steps 0))
      (if (funcall end? curr)
        steps
        (destructuring-bind (left right) (assoc-value network curr)
          (ecase (car instructions)
            (#\L (recur left (cdr instructions) (1+ steps)))
            (#\R (recur right (cdr instructions) (1+ steps)))))))))


(define-solution (2023 08) (input parse-input)
  (values (count-steps :aaa [eq _ :zzz] input)
          (destructuring-bind (_ . network) input
            (declare (ignore _))
            (bnd* ((start-nodes (keep-if [string-ends-with-p "A" (mkstr _)]
                                         (alist-keys network) ))
                   (end? [string-ends-with-p "Z" (mkstr _)]))
              (apply #'lcm (mapcar [count-steps _ end? input] start-nodes))))))

(define-test (2023 08) (21409 21165830176709))
