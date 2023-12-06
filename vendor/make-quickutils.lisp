(pushnew '(merge-pathnames (parse-namestring "quickutil/quickutil-utilities/")
           *default-pathname-defaults*)
         asdf:*central-registry*)

(ql:quickload 'quickutil)

(qtlc:save-utils-as
  "quickutils.lisp"
  :utilities '(

               :aif
               :awhen
               :bnd*
               :bnd1
               :copy-array
               :copy-hash-table
               :digits
               :divf
               :dolist+
               :dorange
               :dorangei
               :doseq
               :flatten
               :hash-table-alist
               :hash-table-key-exists-p
               :hash-table-keys
               :hash-table-values
               :if-let
               :iota
               :looping
               :make-keyword
               :mkstr
               :mulf
               :ncycle
               :repeat
               :string-starts-with-p
               :symb
               :void
               :when-let
               :while
               :with-gensyms

               )
  :package "AOC.QUICKUTILS")
