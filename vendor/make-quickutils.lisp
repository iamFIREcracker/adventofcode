(ql:quickload 'quickutil)

(qtlc:save-utils-as
  "quickutils.lisp"
  :utilities '(
               :digits
               :flatten
               :mkstr
               :ncycle
               :symb
               :void
               :with-gensyms)
  :package "AOC.QUICKUTILS")
