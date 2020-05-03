(eval-when (:compile-toplevel :load-toplevel :execute)
  (unless (find-package "AOC.QUICKUTILS")
    (defpackage "AOC.QUICKUTILS"
      (:documentation "Package that contains Quickutil utility functions.")
      (:use :cl))))

(in-package "AOC.QUICKUTILS")

;; need to define this here so sbcl will shut the hell up about it being
;; undefined when compiling quickutils.lisp.  computers are trash.
(defparameter *utilities* nil)
