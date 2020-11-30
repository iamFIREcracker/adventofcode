(defpackage :assembunnycode
  (:use :cl :pmdb :aoc)
  (:export
    :parse-program
    :run))
(in-package :assembunnycode)

(defvar *regs* nil)
(defvar *ip* nil)
(defvar *program* nil)

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

(defun i-jnz (x y)
  (if (zerop (reg-or-value x)) 1 (reg-or-value y)))

(defun i-tgl (x)
  (prog1 1
    (let* ((offset (+ (reg-or-value x) *ip*)))
      (when (array-in-bounds-p *program* offset)
        (destructuring-bind (fun . args) (aref *program* offset)
          (setf (car (aref *program* offset))
                (ecase (length args)
                  (1 (if (eq fun #'i-inc) #'i-dec #'i-inc))
                  (2 (if (eq fun #'i-jnz) #'i-cpy #'i-jnz)))))))))

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
      (list #'i-jnz (parse-reg-or-value (second parts)) (parse-reg-or-value (third parts))))))

(defun parse-tgl (string)
  (let ((parts (split-sequence:split-sequence #\Space string)))
    (when (string= (first parts) "tgl")
      (list #'i-tgl (parse-reg-or-value (second parts))))))

(defun parse-instruction (string)
  (or (parse-cpy string) (parse-inc string) (parse-dec string) (parse-jnz string)
      (parse-tgl string)))

(defun parse-program (data)
  (map 'vector #'parse-instruction data))

(defun run (program regs)
  (let ((*regs* regs)
        (*ip* 0)
        (*program* program))
    (loop while (< *ip* (length program))
          for (fun . args) = (aref program *ip*)
          do (incf *ip* (handler-case (apply fun args)
                          (type-error (c)
                            (format t "Error when evaluating ~a with ~a: ~s ~s~%" fun args c c)
                            1))))
    (reg #\a)))
