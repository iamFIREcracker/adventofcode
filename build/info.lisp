(format t "~a:~a on ~a~%...~%~%" (lisp-implementation-type) (lisp-implementation-version) (machine-type))
(format t " fixnum bits:~a~%" (integer-length most-positive-fixnum))
(ql:quickload 'trivial-features)
(format t "features = ~s~%" *features*)
