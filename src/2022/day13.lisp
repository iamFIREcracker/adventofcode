(defpackage :aoc/2022/13 #.cl-user::*aoc-use*)
(in-package :aoc/2022/13)


#+#:excluded (&optional (program (uiop:read-file-forms #P"src/2022/day13.txt")))
#+#:excluded (uiop:read-file-forms #P"scratch.txt")

(defun parse-pairs (&optional (strings (uiop:read-file-lines #P"src/2022/day13.txt")))
  (let ((pairs (split-sequence:split-sequence "" strings :test #'string=)))
    (flet ((pair (s)
             (read-from-string (substitute #\) #\] (substitute #\Space #\, (substitute #\( #\[ s))))))
      (mapcar [mapcar #'pair _] pairs))))

(defun solve (&optional (pairs (parse-pairs)))
  (loop for (v1 v2) in pairs for i from 1 when (in-order-p v1 v2) sum (pr i)))

(defun in-order-p (v1 v2)
  (labels ((recur  (v1 v2)
             (cond ((and (numberp v1) (numberp v2)) (<=> v1 v2))
                   ((and (listp  v1) (listp v2)) (loop for e1 in v1 for e2 in v2
                                                       for order = (recur e1 e2)
                                                       if (/= order 0) return order
                                                       finally (return (recur (length v1) (length v2)))))
                   ((numberp v1) (recur (list v1) v2))
                   ((numberp v2) (recur v1 (list v2))))))
    (= (recur v1 v2) -1)))

#; Scratch
(solve (parse-pairs #+#:excluded (uiop:read-file-lines #P"scratch.txt")))
;; cannot just check the lenghts
6239

'((()) (3))

(reduce #'* (loop for i from 1
       for p in (sort (append (loop for (p1 p2) in (parse-pairs) collect p1 collect p2)
                              (list '((2)) '((6))))
                      #'in-order-p)
       when (member p (list '((2)) '((6))) :test 'equal) collect i))

(solve (parse-pairs #+#:excluded (uiop:read-file-lines #P"scratch.txt")))
