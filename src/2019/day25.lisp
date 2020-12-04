(defpackage :aoc/2019/25 #.cl-user::*aoc-use*)
(in-package :aoc/2019/25)

(defun serialized-command (str)
  (map 'list #'char-code (mkstr str #\Newline)))

(defun push-input (p input &aux (command (serialized-command input)))
  (dolist (c command)
    (enqueue c (intcode:program-in p))))

(define-solution (2019 25) (program intcode:read-program)
  (values
    (loop
      :do (intcode:program-run program)
      :do (loop
            :until (queue-empty-p (intcode:program-out program))
            :do (format T "~a" (code-char (dequeue (intcode:program-out program)))))
      :do (push-input program (read-line)))))

; (define-test (2019 25) (20483)) XXX how can we programmatically solve this?!
