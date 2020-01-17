(defclass auto-module (module) ())

(defmethod component-children ((self auto-module))
  (mapcar (lambda (p) (make-instance 'cl-source-file :type "lisp"
                        :pathname p
                        :name (pathname-name p)
                        :parent (component-parent self)))
          (directory-files (component-pathname self)
                           (make-pathname :directory nil :name *wild* :type "lisp"))))

(asdf:defsystem #:aoc
  :description "Advent of Code solutions, in Common Lisp"
  :author "Matteo Landi <matteo@matteolandi.net>"
  :license  "MIT"
  :version "0.0.1"
  :serial t
  :depends-on (

              #:1am
              #:split-sequence

              )
  :components
  ((:module "vendor"
    :serial t
    :components ((:file "quickutils")))
   (:file "pmdb")
   (:file "package")
   (:file "utils")
   (:file "intcode")
   (:auto-module "2017")
   (:module "2018" :serial t
    :components ((:file "day01")
                 (:file "day02")
                 (:file "day03")
                 (:file "day05")
                 (:file "day08")
                 (:file "day09")
                 (:file "day11")
                 (:file "day22")
                 (:file "day25")))
   (:auto-module "2019"))
  :in-order-to ((test-op (test-op :aoc/tests))))

(asdf:defsystem :aoc/tests
  :description "Advent of Code solutions, in Common Lisp"
  :author "Matteo Landi <matteo@matteolandi.net>"
  :license  "MIT"
  :version "0.0.1"
  :depends-on (#:aoc)
  :perform (test-op (o c) (uiop:symbol-call :1am '#:run)))
