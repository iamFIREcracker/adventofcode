(defpackage :aoc/2020/18 #.cl-user::*aoc-use*)
(in-package :aoc/2020/18)

(defparameter *operators-with-precedence* nil)

(defun parse-value (ch) (parse-integer (string ch)))

(defun parse-operand (string &aux (ch (char string 0)))
  (cond ((digit-char-p ch) (list (parse-value ch) (subseq string 1)))
        ((char= ch #.(char "(" 0))
         (destructuring-bind (e rest) (parse-expression (subseq string 1))
           (list e (subseq rest 1)))) ; skip closing parenthesis
        (t (error "Unexpected text: ~s" string))))

(defun parse-operator (string &aux (ch (char string 0)))
  (list (find-symbol (string ch)) (subseq string 1)))

(defun parse-expression (string)
  (loop with (expr rest) = (parse-operand string)
        until (or (zerop (length rest)) (char= (char rest 0) #.(char ")" 0)))
        for (rator rator-rest) = (parse-operator rest) do
        (if (find rator *operators-with-precedence*)
          (destructuring-bind (rand rand-rest)
              (parse-operand rator-rest)
            (setf expr (list rator expr rand) rest rand-rest))
          (destructuring-bind (expr1 expr1-rest)
              (parse-expression rator-rest)
            (setf expr (list rator expr expr1) rest expr1-rest)))
        finally (return (list expr rest))))

(defun parse (string &aux (string (remove #\Space string)))
  (first (parse-expression (remove #\Space string))))

(define-solution (2020 18) (data)
  (values
    (let ((*operators-with-precedence* '(+ *)))
      (reduce #'+ (mapcar #'parse data) :key 'eval))
    (let ((*operators-with-precedence* '(+)))
      (reduce #'+ (mapcar #'parse data) :key #'eval))))

(define-test (2020 18) (50956598240016 535809575344339))
