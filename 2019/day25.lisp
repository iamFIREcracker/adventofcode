(defpackage :aoc/2019/25 #.cl-user::*aoc-use*)
(in-package :aoc/2019/25)

(defun serialized-command (str)
  (map 'list #'char-code (mkstr str #\Newline)))

(defun push-input (p input &aux (command (serialized-command input)))
  (dolist (c command)
    (enqueue c (intcode:program-in p))))

(define-problem (2019 25) (program intcode:read-program)
  (values
    (loop
      :with seen = (make-hash-table)
      :do (intcode:program-run program)
      :do (loop
            :until (queue-empty-p (intcode:program-out program))
            :do (format T "~a" (code-char (dequeue (intcode:program-out program)))))
      :do (push-input program (read-line)))))

; (1am:test test-2019/25
;   (multiple-value-bind (part1) (problem-run)
;     (1am:is (= 20483 part1)))) XXX how can we programmatically solve this?!
