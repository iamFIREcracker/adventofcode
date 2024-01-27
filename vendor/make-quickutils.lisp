(pushnew '(merge-pathnames (parse-namestring "quickutil/quickutil-utilities/")
           *default-pathname-defaults*)
         asdf:*central-registry*)

(ql:quickload 'quickutil)

(qtlc:save-utils-as
  "quickutils.lisp"
  :utilities '(

               #+nil :d-b
               #+nil :m-v-b
               #+nil :split
               #+nil :split-if
               #+nil :split-if-not
               :keep-if
               :keep-if-not

               :aand
               :aif
               :alist-keys
               :alist-values
               :assoc-value
               :awhen
               :bnd*
               :bnd1
               :copy-array
               :copy-hash-table
               :digits
               :divf
               :doalist
               :dohash
               :dolists
               :dorange
               :dorangei
               :doseq
               :doseqs
               :dosublists
               :enumerate
               :flatten
               :hash-table-alist
               :hash-table-key-exists-p
               :hash-table-keys
               :hash-table-values
               :if-not
               :if-let
               :iota
               :looping
               :make-keyword
               :mkstr
               :mulf
               :ncycle
               :recursively
               :repeat
               :string-ends-with-p
               :string-starts-with-p
               :subdivide
               :subseq-
               :symb
               :void
               :when-let
               :when-not
               :while
               :with-gensyms

               )
  :package "AOC.QUICKUTILS")
