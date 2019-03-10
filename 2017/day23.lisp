(defpackage :aoc/2017/23 #.cl-user::*aoc-use*)
(in-package :aoc/2017/23)

(defun i-sub (x y &aux (pos (aoc/2017/18:reg-name-to-pos x)))
  (decf (aref aoc/2017/18:*registers* pos) (aoc/2017/18:value-or-reg-content y)))

(defun i-jnz (x y)
  (unless (zerop (aoc/2017/18:value-or-reg-content x))
    (incf (aref aoc/2017/18:*registers* aoc/2017/18:ip-pos) (1- (aoc/2017/18:value-or-reg-content y)))))

(define-problem (2017 23) (data)
  (setf aoc/2017/18:*instructions-by-name* `(("set" ,#'aoc/2017/18:i-set)
                                             ("sub" ,#'i-sub)
                                             ("mul" ,#'aoc/2017/18:i-mul)
                                             ("jnz" ,#'i-jnz)))
  (values
    (loop
      :with instructions = (aoc/2017/18:parse-instructions data)
      :with current = (aoc/2017/18:make-program 0 instructions)
      :with aoc/2017/18:*registers* = (aoc/2017/18:program-registers current)
      :for name = (aoc/2017/18:program-next-instruction-name current)
      :count (string= "mul" name)
      :while name
      :do (aoc/2017/18:program-exec-next-instruction current))))


(1am:test test-2017/23
  (multiple-value-bind (part1) (problem-run)
    (1am:is (= 6241 part1))))
