(ql:quickload 'quickutil)

(qtlc:save-utils-as
  "quickutils.lisp"
  :utilities '(
               :copy-hash-table
               :flatten
               :hash-table-alist
               :hash-table-keys
               :hash-table-values
               :if-let
               :iota
               :mkstr
               :ncycle
               :symb
               :void
               :when-let
               :with-gensyms
               )
  :package "AOC.QUICKUTILS")
