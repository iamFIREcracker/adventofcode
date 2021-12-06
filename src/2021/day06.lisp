(defpackage :aoc/2021/06 #.cl-user::*aoc-use*)
(in-package :aoc/2021/06)


(defun parse-generations (data &aux
                               (generations (make-array 9 :initial-element 0))
                               (ages (cl-ppcre:all-matches-as-strings "\\d" (first data))))
  (loop for v in ages do (incf (aref generations (parse-integer v))))
  generations)


(defun evolve (generations days &aux (gg (copy-seq generations)))
  (dotimes (_ days (reduce #'+ gg))
    (let ((n (aref gg 0)))
      (dotimes (i 8)
        (setf (aref gg i) (aref gg (1+ i))))
      (setf (aref gg 8) n)
      (incf (aref gg 6) n))))


(define-solution (2021 06) (generations parse-generations)
  (values (evolve generations 80) (evolve generations 256)))

(define-test (2021 06) (371379 1674303997472))
