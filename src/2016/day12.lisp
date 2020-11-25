(defpackage :aoc/2016/12 #.cl-user::*aoc-use*)
(in-package :aoc/2016/12)

(defvar *regs* nil)

(defun reg-name-index (reg)
  (- (char-code reg) (char-code #\a)))

(defun reg (name)
  (aref *regs* (reg-name-index name)))

(defun (setf reg) (value name)
  (setf (aref *regs* (reg-name-index name)) value))

(defun reg-or-value (x)
  (if (numberp x) x (reg x)))

(defun i-cpy (x reg)
  (prog1 1 (setf (reg reg) (reg-or-value x))))

(defun i-inc (reg) (prog1 1 (incf (reg reg))))

(defun i-dec (reg) (prog1 1 (decf (reg reg))))

(defun i-jnz (x offset)
  (if (zerop (reg-or-value x)) 1 offset))

(defun parse-value (string)
  (parse-integer string :junk-allowed t))

(defun parse-reg (string)
  (aref string 0))

(defun parse-reg-or-value (string)
  (or (parse-value string) (parse-reg string)))

(defun parse-cpy (string)
  (let ((parts (split-sequence:split-sequence #\Space string)))
    (when (string= (first parts) "cpy")
      (list #'i-cpy (parse-reg-or-value (second parts)) (parse-reg (third parts))))))

(defun parse-inc (string)
  (let ((parts (split-sequence:split-sequence #\Space string)))
    (when (string= (first parts) "inc")
      (list #'i-inc (parse-reg (second parts))))))

(defun parse-dec (string)
  (let ((parts (split-sequence:split-sequence #\Space string)))
    (when (string= (first parts) "dec")
      (list #'i-dec (parse-reg (second parts))))))

(defun parse-jnz (string)
  (let ((parts (split-sequence:split-sequence #\Space string)))
    (when (string= (first parts) "jnz")
      (list #'i-jnz (parse-reg-or-value (second parts)) (parse-value (third parts))))))

(defun parse-instruction (string)
  (or (parse-cpy string) (parse-inc string) (parse-dec string) (parse-jnz string)))

(defun parse-program (data)
  (map 'vector #'parse-instruction data))

(defun run (program regs)
  (let ((*regs* regs))
    (loop :with ip = 0
          :while (< ip (length program))
          :for (fun . args) = (aref program ip)
          :do (incf ip (apply fun args)))
    (reg #\a)))

(define-problem (2016 12) (program parse-program)
  (values
    (run program (make-array 4 :initial-contents (list 0 0 0 0)))
    (run program (make-array 4 :initial-contents (list 0 0 1 0)))))

(1am:test test-2016/12
  (multiple-value-bind (part1 part2) (problem-run)
    (1am:is (= 318083 part1))
    (1am:is (= 9227737 part2))))
