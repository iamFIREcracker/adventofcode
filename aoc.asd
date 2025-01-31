(pushnew (merge-pathnames (parse-namestring "vendor/cl-classified/")
           *default-pathname-defaults*)
         asdf:*central-registry*)

(defclass auto-module (module)
  ((file-cache :initform (make-hash-table))))

(defmethod component-children ((self auto-module)
                               &aux (file-cache (slot-value self 'file-cache)))
  (mapcar (lambda (p &aux (existing (gethash p file-cache)))
            (if existing
                existing
                (setf (gethash p file-cache)
                      (make-instance 'cl-source-file :type "lisp"
                                     :pathname p
                                     :name (pathname-name p)
                                     :parent self))))
          (directory-files (component-pathname self)
                           (make-pathname :directory nil :name *wild* :type "lisp"))))

(asdf:defsystem #:aoc
  :description "Advent of Code solutions, in Common Lisp"
  :author "Matteo Landi <matteo@matteolandi.net>"
  :license  "MIT"
  :version "0.0.1"
  :serial t
  :depends-on (

              #:classified
              #:1am
              #:cl-ppcre
              #:md5
              #:pileup
              #:split-sequence
              #:st-json

              )
  :components
  ((:module "vendor"
    :serial t
    :components ((:file "quickutils-package")
                 (:file "quickutils")
                 (:module "mlsyntax" :serial t
                      :components ((:file "mlsyntax")))))
   (:module "upstream"
    :serial t
    :components ((:file "quickutils-local")
                 (:file "hset")
                 (:file "dset")))
   (:file "package")
   (:file "syntax")
   (:module "src"
    :serial t
    :components ((:file "utils")
                 (:file "intcode")
                 (:file "assembunnycode")
                 (:file "gameoflife")
                 (:auto-module "2015")
                 (:auto-module "2016")
                 (:auto-module "2017")
                 (:module "2018" :serial t
                     :components ((:file "day01")
                                  (:file "day02")
                                  (:file "day03")
                                  (:file "day04")
                                  (:file "day05")
                                  (:file "day06")
                                  (:file "day07")
                                  (:file "day08")
                                  (:file "day09")
                                  (:file "day10")
                                  (:file "day11")
                                  (:file "day12")
                                  (:file "day13")
                                  (:file "day14")
                                  (:file "day17")
                                  (:file "day18")
                                  (:file "day20")
                                  (:file "day22")
                                  (:file "day23")
                                  (:file "day25")))
                 (:auto-module "2019")
                 (:auto-module "2020")
                 (:auto-module "2021")
                 (:auto-module "2022")
                 (:auto-module "2023")
                 (:auto-module "2024")
                 )))
  :in-order-to ((test-op (test-op :aoc/tests))))

(asdf:defsystem :aoc/tests
  :description "Advent of Code solutions, in Common Lisp"
  :author "Matteo Landi <matteo@matteolandi.net>"
  :license  "MIT"
  :version "0.0.1"
  :depends-on (#:aoc)
  :perform (test-op (o c) (uiop:symbol-call :aoc '#:run)))
