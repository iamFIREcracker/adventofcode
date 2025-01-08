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

               :@
               :aand
               :aif
               :alist
               :alist-keys
               :alist-values
               :appendf
               :aprog1
               :assoc-value
               :awhen
               :bnd*
               :bnd1
               :continuable
               :copy-array
               :copy-hash-table
               :dbg
               :dbgl
               :defaccessor
               :digits
               :divf
               :doalist
               :doeseq
               :doesublists
               :dohash
               :dohashk
               :dohashv
               :dolists
               :dorange
               :dorangei
               :doseq
               :doseqs
               :dosublists
               :enumerate
               :flatten
               :fn
               :hash-table-alist
               :hash-table-key-exists-p
               :hash-table-keys
               :hash-table-values
               :if-let
               :if-not
               :iota
               :keep-if
               :keep-if-not
               :last-elt
               :let1
               :looping
               :make-keyword
               :mklist
               :mkstr
               :mulf
               :ncycle
               :partition-if
               :pcase
               :plist-keys
               :plist-values
               :pmx
               :pr
               :prn
               :prog1-let
               :prs
               :psx
               :random-elt
               :recursively
               :removef
               :repeat
               :retriable
               :shuffle
               :spr
               :sprn
               :sprs
               :string-ends-with-p
               :string-starts-with-p
               :subdivide
               :subseq-
               :symb
               :take
               :undefclass
               :undefconstant
               :undefmacro
               :undefmethod
               :undefpackage
               :undefparameter
               :undefun
               :undefvar
               :until
               :value-at
               :void
               :when-let
               :when-not
               :while
               :while-not
               :with-gensyms
               :xor
               :zapf
               :~>
               :~>>

               )
  :package "AOC.QUICKUTILS")
