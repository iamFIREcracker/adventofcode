(in-package "MLUTILS")

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
