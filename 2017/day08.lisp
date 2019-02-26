(defpackage :aoc/2017/08 #.cl-user::*aoc-use*)
(in-package :aoc/2017/08)

(defstruct (jump-instruction (:conc-name NIL))
  reg-inc
  inc-delta
  comp-op
  reg-comp-1
  value-comp)


(defun != (a b)
  (not (= a b)))

(defun parse-jump-instructions (x)
  (let ((comp-ops `(("<"  ,#'<)
                    (">"  ,#'>)
                    (">=" ,#'>=)
                    ("==" ,#'=)
                    ("<=" ,#'<=)
                    ("!=" ,#'!=))))
    (labels ((parse-instruction (s)
               (let* ((splits (split-sequence:split-sequence #\Space s))
                      (reg-inc (first splits))
                      (inc-op (second splits))
                      (amount (parse-integer (third splits)))
                      (inc-delta (if (equal inc-op "inc") amount (- amount)))
                      (comp-op (second (assoc (sixth splits) comp-ops :test 'equal)))
                      (reg-comp-1 (fifth splits))
                      (value-comp (parse-integer (seventh splits))))
                 (make-jump-instruction :reg-inc reg-inc
                                        :inc-delta inc-delta
                                        :comp-op comp-op
                                        :reg-comp-1 reg-comp-1
                                        :value-comp value-comp))))
      (mapcar #'parse-instruction x))))

(define-problem (2017 8) (data parse-jump-instructions)
  (labels ((get-reg-value (registers reg)
             (gethash reg registers 0))
           (inc-reg (registers reg delta)
             (incf (gethash reg registers 0) delta))
           (max-reg-value (registers)
             (reduce #'max (hash-table-values registers))))
    (loop
      :with registers = (make-hash-table :test 'equal)
      :for instruction :in data ; XXX isn't there a better way to handle this?
      :for reg-inc = (reg-inc instruction)
      :for inc-delta = (inc-delta instruction)
      :for op = (comp-op instruction)
      :for reg-comp-1 = (reg-comp-1 instruction)
      :for value-comp = (value-comp instruction)
      :when (funcall op (get-reg-value registers reg-comp-1) value-comp)
      :maximizing (inc-reg registers reg-inc inc-delta) :into max
      :finally (return (values (max-reg-value registers)
                               max)))))

(1am:test test-2017/08
  (multiple-value-bind (part1 part2) (problem-run)
    (1am:is (= 4888 part1))
    (1am:is (= 7774 part2))))
