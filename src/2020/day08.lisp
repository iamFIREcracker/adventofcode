(defpackage :aoc/2020/08 #.cl-user::*aoc-use*)
(in-package :aoc/2020/08)

(defun parse-instruction (string)
  (destructuring-bind (name arg)
      (split-sequence:split-sequence #\Space string)
    (cons (make-keyword (string-upcase name)) (parse-integer arg))))

(defun parse-program (data)
  (map 'vector #'parse-instruction data))

(defun run-program (program)
  (loop with ip = 0 with acc = 0
        for (name . arg) = (aref program ip)
        do (incf ip (if (eql name :jmp) arg 1))
        when (eql name :acc) do (incf acc arg)
        when (member ip seen) return (values acc)
        when (= ip (length program)) return (values acc t)
        collect ip into seen))

(defun toggle-instruction (program i)
  (let ((name (car (aref program i))))
    (unless (eql name :acc)
      (setf (car (aref program i))
            (if (eql name :jmp) :nop :jmp)))))

(define-solution (2020 8) (program parse-program)
  (values
    (run-program program)
    (loop for i below (length program)
          when (toggle-instruction program i) do
          (multiple-value-bind (acc clear-exit-p)
              (run-program program)
            (when clear-exit-p (return acc)))
          (toggle-instruction program i))))

(define-test (2020 8) (1087 780))
