(in-package "MLUTILS")


(defun opening-braket-reader (stream char)
  "Reads a partial function application from `stream`, and emits a LAMBDA
expression for it.

A partial function application is a list of forms delimited by a ] character
(in the READ-DELIMITED-LIST sense)

Inside `body`:

- _, %, or %1 can be used to access the first argument of the generated lambda.
- %2, %3, %n can be used to access respectively the second, third, and n-th
  argument of the generated lambda.
- %& can be used to access all the arguments of the generated lambda.

Examples:

    ([print %] )"
  (declare (ignore char))
  (let ((body (read-delimited-list #\] stream t))
        (args (loop for i from 1 to 3 collect (intern (format nil "%~d" i))))
        (arg0% (intern "%"))
        (arg0_ (intern "_")))
    `(lambda (&optional ,@args)
       (declare (ignorable ,@args))
       (let ((,arg0% ,(first args))
             (,arg0_ ,(first args)))
         (declare (ignorable ,arg0% ,arg0_))
         ,body))))

(defun tilde-reader (stream sub-char numarg)
  "#~foo.bar => (bar foo)"
  (declare (ignore sub-char numarg))
  (let ((following (read stream)))
    (if (position #\. (string following))
        (let ((parts (split-sequence:split-sequence #\. (string following))))
          (reduce (lambda (acc cur) (list (intern cur) acc)) (cdr parts) :initial-value (intern (car parts))))
        following)))

#+#:excluded (with-input-from-string (s "foo.car")
               (tilde-reader s #\~))
#+#:excluded (with-input-from-string (s "(list :~>)")
               (tilde-reader s #\~))

(defun sharp-semicolon-reader (stream sub-char numarg)
  (declare (ignore sub-char numarg))
  (loop :while (read-line stream nil nil))
  (values))


#+#:excluded (get-dispatch-macro-character #\# #\~)
;; https://stackoverflow.com/questions/30699851/what-does-the-non-terminating-p-argument-of-set-macro-character-do
(named-readtables:defreadtable :mlutils-syntax
  (:merge :standard)
  ;; set NON-TERMINATING-P so that LASS e[prop] syntax
  ;; keeps on working as expected
  (:macro-char #\[ #'opening-braket-reader t)
  (:macro-char #\] (get-macro-character (char ")" 0)))
  (:dispatch-macro-char #\# #\~ #'tilde-reader)
  (:dispatch-macro-char #\# #\; #'sharp-semicolon-reader)
  #+#:excluded (:macro-char #\" #'pythonic-string-reader::read-multiline-string t)
  )
