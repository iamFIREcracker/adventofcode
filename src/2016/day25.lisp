(defpackage :aoc/2016/25 #.cl-user::*aoc-use*)
(in-package :aoc/2016/25)

(define-condition not-clock-signal () ())
(define-condition clock-signal-indeed () ())

(defun generates-clock-signal-p (input program)
  (let ((remaining 10)
        (expected (ncycle (list 0 1))))
    (handler-case
        (handler-bind ((assembunnycode:program-output
                         (lambda (c)
                           (if (/= (assembunnycode:output-value c) (car expected))
                             (error 'not-clock-signal)
                             (progn
                               (setf remaining (1- remaining)
                                     expected (cdr expected))
                               (when (zerop remaining)
                                 (signal 'clock-signal-indeed)))))))
          (assembunnycode:run program (list input 0 0 0)))
      (not-clock-signal () nil)
      (clock-signal-indeed () t))))

(define-problem (2016 25) (program assembunnycode:parse-program)
  (loop for n from 0
        when (generates-clock-signal-p n program) return n))

(1am:test test-2016/25
  (multiple-value-bind (part1) (problem-run)
    (1am:is (= 198 part1))))
