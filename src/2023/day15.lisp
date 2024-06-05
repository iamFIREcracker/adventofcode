(defpackage :aoc/2023/15 #.cl-user::*aoc-use*)
(in-package :aoc/2023/15)

(defun hash (s)
  (bnd1 value 0
    (doseq (ch s)
      (setf value (rem (* (+ (char-code ch) value) 17) 256)))
    value))

(defun part1 (&optional (s (first (aoc::read-problem-input 2023 15))))
  (reduce #'+ (split-sequence:split-sequence #\, s) :key #'hash))


(defun part2 (&optional (s (first (aoc::read-problem-input 2023 15))))
  (bnd1 boxes (make-array 256 :initial-element nil)
    (dolist (part (split-sequence:split-sequence #\, s))
      (cl-ppcre:register-groups-bind (label op num)
          ("(\\w+)(-|=)(\\d*)" part)
        (bnd1 i (hash label)
          (if (string= op "-")
            (setf (aref boxes i) (delete-if [string= (car _) label] (aref boxes i)))
            (aif (assoc label (aref boxes i) :test #'string=)
              (setf (cdr it) (parse-integer num))
              (push (cons label (parse-integer num)) (aref boxes i)))))))
    (focusing-power boxes)))

(defun focusing-power (boxes)
  (looping
    (doseq ((i box) (enumerate boxes :start 1))
      (doseq ((j (label . num)) (enumerate (reverse box) :start 1))
        (sum! (* i j num))))))


(define-solution (2023 15) (s first)
  (values (part1 s) (part2 s)))

(define-test (2023 15) (510013 268497))
