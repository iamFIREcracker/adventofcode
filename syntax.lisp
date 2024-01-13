(in-package :aoc)

;; Here we use MERGE-READTABLES-INTO to merge :MLUTILS-SYNTAX inside :CURRENT;
;; if we used IN-READTABLE instead, we would be forced to call it from each
;; and every problem package...and that would be annoying.
#+running-locally
(handler-bind ((named-readtables:reader-macro-conflict 'continue))
  (named-readtables:merge-readtables-into :current :mlutils-syntax))
#-running-locally (named-readtables:merge-readtables-into :current :mlutils-syntax)
