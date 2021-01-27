(ql:quickload 'quickutil)

(qtlc:save-utils-as
  "quickutils.lisp"
  :utilities '(
               :copy-hash-table
               :divf
               :flatten
               :hash-table-alist
               :hash-table-keys
               :hash-table-values
               :hash-table-key-exists-p
               :if-let
               :iota
               :make-keyword
               :mkstr
               :mulf
               :ncycle
               :symb
               :void
               :when-let
               :with-gensyms
               )
  :package "AOC.QUICKUTILS")
