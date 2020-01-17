(defpackage :pmdb
  (:use :cl)
  (:export
    :pr
    :prl))
(in-package :pmdb)

(defun pr (&rest args)
  "Print `args` readably, separated by spaces and followed by a newline.
  Returns the first argument, so you can just wrap it around a form without
  interfering with the rest of the program.
  This is what `print` should have been.

  https://github.com/sjl/cl-losh/blob/master/DOCUMENTATION.markdown#pr-macro
  "
  (format t "~{~S~^ ~}~%" args) ; Within the body segment, ~^ acts like pprint-exit-if-list-exhausted.
  (finish-output)
  (first args))

(defmacro prl (&rest args)
  "Print `args` labeled and readably.
  Each argument form will be printed, then evaluated and the result printed.
  One final newline will be printed after everything.
  Returns the last result.
  Examples:
    (let ((i 1)
          (l (list 1 2 3)))
      (prl i (second l)))
    ; =>
    i 1
    (second l) 2

  https://github.com/sjl/cl-losh/blob/master/DOCUMENTATION.markdown#prl-macro
  "
  `(prog1
     (progn ,@(mapcar (lambda (arg) `(pr ',arg ,arg)) args))
     (terpri)
     (finish-output)))
