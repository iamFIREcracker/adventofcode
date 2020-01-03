(ql:quickload 'quickutil)

(qtlc:save-utils-as
  "quickutils.lisp"
  :utilities '(
               :mkstr
               :symb
               :with-gensyms)
  :package "AOC.QUICKUTILS")
