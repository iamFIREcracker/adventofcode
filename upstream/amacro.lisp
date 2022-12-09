(defpackage :amacro
  (:use :cl)
  (:export
   :let1
   :aif
   :awhen))
(in-package :amacro)


(defmacro let1 (binding &body body)
  (let1-expand binding body))

(eval-when (:compile-toplevel :load-toplevel :execute)
  (defun let1-expand (binding body)
    `(let (,binding)
       ,@body)))

#+#:excluded (macroexpand-1 '(let1 (foo "bar") foo))
#+#:excluded (macroexpand-1 '(let1 (foo) foo))


(defmacro aif (test then &optional else)
  (aif-expand test then else))

(eval-when (:compile-toplevel :load-toplevel :execute)
  (defun aif-expand (test then &optional else)
    (let1 (it (intern "IT"))
      `(let ((,it ,test))
         (if ,it ,then ,else)))))

#+#:excluded (aif-expand '(+ 1 2) 3)

(defmacro awhen (test &body body)
  (awhen-expand test body))

(eval-when (:compile-toplevel :load-toplevel :execute)
  (defun awhen-expand (test body)
    (let1 (it (intern "IT"))
      `(let ((,it ,test))
         (when ,it
           ,@body)))))
