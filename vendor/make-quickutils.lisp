(ql:quickload 'quickutil)

(qtlc:save-utils-as
  "quickutils.lisp"
  :utilities '(
               :mkstr
               :symb
               :void
               :with-gensyms)
  :package "AOC.QUICKUTILS")
