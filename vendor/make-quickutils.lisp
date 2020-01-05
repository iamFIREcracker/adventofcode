(ql:quickload 'quickutil)

(qtlc:save-utils-as
  "quickutils.lisp"
  :utilities '(
               :mkstr
               :ncycle
               :symb
               :void
               :with-gensyms)
  :package "AOC.QUICKUTILS")
