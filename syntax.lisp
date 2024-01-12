(in-package :aoc)

(eval-when (:compile-toplevel :load-toplevel :execute)
  (defun opening-braket-reader (stream char)
    (declare (ignore char))
    (let ((arg (intern "_")))
      `(lambda (&optional ,arg)
         (declare (ignorable ,arg))
         (,@(read-delimited-list #\] stream t)))))
  (set-macro-character #\[ #'opening-braket-reader)
  ; Using #\) messes up with the editor -,-
  (set-macro-character #\] (get-macro-character #.(char ")" 0))))

#+#:excluded (read (make-string-input-stream "[format t \"Hello, ~a!\" _]"))

(eval-when (:compile-toplevel :load-toplevel :execute)
  (defun sharp-semicolon-reader (stream sub-char numarg)
    (declare (ignore sub-char numarg))
    (loop :while (read-line stream nil nil))
    (values))
  (set-dispatch-macro-character #\# #\; #'sharp-semicolon-reader))
