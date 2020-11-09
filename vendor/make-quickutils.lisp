(ql:quickload 'quickutil)

(qtlc:save-utils-as
  "quickutils.lisp"
  :utilities '(
               :copy-hash-table
               :digits
               :flatten
               :hash-table-alist
               :hash-table-keys
               :hash-table-values
               :iota
               :mkstr
               :ncycle
               :symb
               :void
               :with-gensyms)
  :package "AOC.QUICKUTILS")
