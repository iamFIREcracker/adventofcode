(defpackage #:3am
  (:use #:cl)
  (:import-from #:1am #:is #:signals #:run #:*tests*)
  (:export #:test #:is #:signals #:run #:*tests*))
(in-package #:3am)

(defmacro test (name &body body)
  "Define a test function, add it to `*tests*' and then run it immediately.

  Note: the test is immediately run only if the macro is directly evaluated in
  the REPL, i.e., if neither `*compile-file-pathname*' nor `*load-pathname*'
  are set."
  `(prog1 (1am:test ,name ,@body)
     (unless (or *compile-file-pathname* *load-pathname*)
       (,name))))

(defmacro expands1 (form expansion)
  "Assert that `form' expands (in the MACROEXPAND-1 sense) to `expansion'."
 `(is (equal (macroexpand-1 ',form) ',expansion)))
