(defpackage :aoc/2022/21 #.cl-user::*aoc-use*)
(in-package :aoc/2022/21)

(defun parse-monkey (s)
  (bnd1 (parts (split-sequence:split-sequence #\Space s))
    (list* (subseq (car parts) 0 4)
           (cdr parts))))

(defun monkeys (&optional (file #P"src/2022/day21.txt"))
  (bnd1 (map (make-hash-table :test 'equal))
    (loop for (name . rest) in (mapcar #'parse-monkey (uiop:read-file-lines file))
          do (setf (gethash name map) rest))
    map))

(defun meval (monkeys m)
  (bnd1 (args (gethash m monkeys))
    (cond ((= (length args) 1) (parse-integer (car args)))
          (t (destructuring-bind (rand1 rator rand2) args
               (funcall (symb rator)
                        (meval monkeys rand1)
                        (meval monkeys rand2)))))))

(defun meval2 (monkeys m)
  (bnd1 (args (gethash m monkeys))
    (cond ((string= m "humn") "x")
          ((= (length args) 1) (parse-integer (car args)))
          (t (destructuring-bind (rand1 rator rand2) args
               (let* ((rand1 (meval2 monkeys rand1))
                      (rand2 (meval2 monkeys rand2))
                      (can-run? (and (numberp rand1) (numberp rand2))))
                 (if can-run?
                   (funcall (symb rator) rand1 rand2)
                   (list (symb rator) rand1 rand2))))))))


(defparameter *inverse* '((+ -) (- +) (* /) (/ *)))
(defun inverse (sym) (cadr (assoc sym *inverse*)))

(defun solve= (val expr)
  (if (stringp expr)
    val
    (destructuring-bind (rator rand1 rand2) expr
      (let ((inverse-rator (inverse rator)))
        (pr val '= expr )
        (pr inverse-rator)
        (ecase rator
          ((* +) (if (numberp rand1)
                   (solve= (funcall inverse-rator val rand1) rand2)
                   (solve= (funcall inverse-rator val rand2) rand1)))
          ((- /) (if (numberp rand1)
                   (solve= (funcall rator rand1 val) rand2)
                   (solve= (funcall inverse-rator val rand2) rand1))))))))

#; Scratch
(monkeys)
(print-hash-table (monkeys))
(meval (monkeys) "root")
(loop for n from -10000 upto 10000
      when (meval2 (monkeys) "root" n) return it)

(solve= 12 '(- 1 "x"))

(meval2 (monkeys) "lccz" "x")
(meval2 (monkeys) "pttp" "x")


(let ((mm (monkeys)))
  (let ((rhs (meval2 mm (caddr (gethash "root" mm))))
        (lhs (meval2 mm (car (gethash "root" mm)))))
    (solve= rhs lhs)))

(meval2 (monkeys) (car (gethash "root" (monkeys))) "x")
(meval2 (monkeys) (caddr (gethash "root" (monkeys))) "x")

(&optional (file #P"src/2022/day21.txt"))
#P"scratch.txt"
; flip all the operations
3916491093442 -- too low
