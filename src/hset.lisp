(defpackage :hset
  (:use :cl)
  (:export
    :make-hset
    :hset-contains-p
    :hset-values
    :hset-size))
(in-package :hset)

(defun make-hset (values &key (test 'eql))
  "Creates a SET.

  A SET is nothing but a HASH-TABLE having `values` in keys, and T values."
  (let ((h (make-hash-table :test test)))
    (dolist (v values)
      (setf (gethash v h) t))
    h))

(defun hset-contains-p (v hset)
  "Returns T if `v` exists inside `hset`."
  (multiple-value-bind (unused exists-p)
      (gethash v hset)
    (declare (ignore unused))
    exists-p))

(defun hset-values (hset)
  "Returns a LIST containing all the values stored inside `hset`."
  (loop for v being the hash-keys of hset collect v))

(defun hset-size (hset)
  "Returns the number of values stored inside `hset`."
  (hash-table-count hset))
