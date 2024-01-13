(in-package "MLUTILS")


(eval-when (:compile-toplevel :load-toplevel :execute)

  (defun opening-braket-reader (stream char)
    (declare (ignore char))
    (let ((arg (intern "_")))
      `(lambda (&optional ,arg)
         (declare (ignorable ,arg))
         (,@(read-delimited-list #\] stream t)))))

  (defun sharp-semicolon-reader (stream sub-char numarg)
    (declare (ignore sub-char numarg))
    (loop :while (read-line stream nil nil))
    (values))


  (named-readtables:defreadtable :mlutils-syntax
    (:merge :standard)
    (:macro-char #\[ #'opening-braket-reader)
    (:macro-char #\] (get-macro-character #\)))
    (:dispatch-macro-char #\# #\; #'sharp-semicolon-reader))

  )
