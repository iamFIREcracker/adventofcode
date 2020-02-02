(defpackage :pmdb
  (:use :cl)
  (:export
    :pr
    :prl
    :defun/profiled
    :profile))
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

(defparameter *profiling-data* NIL)
(defparameter *profiled-functions-stack* NIL)

(defmacro defun/profiled (name args &body body)
  "Wraps DEFUN to automatically push into *profiling-data* the function execution time."
  (let ((start (gensym)))
    `(defun ,name ,args
       (let ((,start (get-internal-run-time)))
         (unwind-protect (progn (push ',name *profiled-functions-stack*) ,@body)
           (push (list *profiled-functions-stack* ,start (get-internal-run-time)) *profiling-data*)
           (pop *profiled-functions-stack*))))))

(defun compile-profiling-data ()
  (loop
    :with profiling-table = (make-hash-table)
    :with count-table = (make-hash-table)
    :for (label-stack start end) in *profiling-data*
    :for label = (intern (format NIL "~{~a~^/~}" (reverse label-stack)))
    :for ticks = (- end start)
    :summing ticks :into total
    :do (progn
          (incf (gethash label profiling-table 0) ticks)
          (incf (gethash label count-table 0)))
    :finally (return
               (sort
                 (loop
                   :for label :being :the :hash-keys :in profiling-table
                   :collect (let* ((ticks (gethash label profiling-table))
                                   (time (/ ticks INTERNAL-TIME-UNITS-PER-SECOND))
                                   (count (gethash label count-table))
                                   (ticks-per (round (/ ticks count)))
                                   (time-per (/ ticks-per INTERNAL-TIME-UNITS-PER-SECOND))
                                   (%-of-total (round (* 100 (/ ticks total)))))
                              (list %-of-total label ticks time count ticks-per time-per)))
                 #'> :key #'first))))

(defun show-profiling-data ()
  (format *trace-output* "Profiled functions:~&")
  (loop for (%-of-total label ticks time count ticks-per time-per) in (compile-profiling-data) do
       (format *trace-output* "~6,2f% ~a: ~d ticks (~fs) over ~d calls for ~d (~fs) per.~&"
               %-of-total label ticks time count ticks-per time-per)))

(defmacro profile (&body body)
  "Similarly to TIME, PROFILE logs additional information about DEFUN/PROFILED functions' execution
  time.

  For each DEFUN/PROFILED function, PROFILE will output:

  - The % of time the CPU was busy running the function, instead of the others
  - Total number of ticks elapsed when running (see GET-INTERNAL-RUN-TIME)
  - Total number of seconds elapsed when running (see INTERNAL-TIME-UNITS-PER-SECOND)
  - Number of function calls
  - Average number of ticks per call
  - Average number of seconds per call."
  `(let ((*profiling-data* ()))
     (unwind-protect (progn ,@body)
       (show-profiling-data))))
