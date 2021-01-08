(defpackage :aoc/2015/12 #.cl-user::*aoc-use*)
(in-package :aoc/2015/12)

(defun parse-jso (lines)
  (st-json:read-json-from-string (first lines)))

(defun part1 (jso)
  (labels ((recur (x)
             (cond ((stringp x) 0)
                   ((numberp x) x)
                   ((consp x) (reduce #'+ x :key #'recur))
                   (t (let ((sum 0))
                        (st-json:mapjso #'(lambda (k v)
                                           (declare (ignore k))
                                           (incf sum (recur v)))
                                        x)
                        sum)))))
    (recur jso)))

(defun part2 (jso)
  (labels ((recur (x)
             (cond ((stringp x) 0)
                   ((numberp x) x)
                   ((consp x) (reduce #'+ x :key #'recur))
                   (t (let ((sum 0))
                        (st-json:mapjso #'(lambda (k v)
                                           (declare (ignore k))
                                           (when (equal v "red")
                                             (return-from recur 0))
                                           (incf sum (recur v)))
                                        x)
                        sum)))))
    (recur jso)))

(define-solution (2015 12) (jso parse-jso)
  (values (part1 jso) (part2 jso)))

(define-test (2015 12) (156366 96852))
