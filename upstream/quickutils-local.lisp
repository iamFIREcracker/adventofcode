(defpackage "AOC.QUICKUTILS.LOCAL"
  (:use :cl)
  (:export
    :list-hash-table
    ))

(in-package "AOC.QUICKUTILS.LOCAL")

(defun list-hash-table (list key &rest hash-table-initargs)
  "Creates a hash table starting from the elements of `list`.

  The function `key` is called for each of the elements of `list` to generate
  all the key-value pairs with which to initialize the returned hash table;
  `key` is expected to return:

  - A single value, used as the key of the key-value pair
   (the original element of `list` will be used as value instead)
  - Two values, the first being the key and the second the value, of the 
    key-value pairs

  In case of duplicates (i.e. key-value pairs with equal value of key, as
  determined by the hash table's TEST function), only the last one will be
  found in the the hash table.

  Hash table is initialized using `hash-table-initargs`.

  Examples:

  - Create a hash-table mapping from integers to their squares

      (list-hash-table '(1 2 3 4 5) #'(lambda (x) (values x (* x x))))
      ;; 1 -> 1
      ;; 2 -> 4
      ;; 3 -> 9
      ;; 4 -> 16
      ;; 5 -> 25

  - Create a hash table mapping from from integers to their square roots (`key`
    returns a single value, the key of each key-value pair)

      (list-hash-table '(1 2 3 4 5) #'(lambda (x) (* x x)))
      ;; 1 -> 1
      ;; 2 -> 2
      ;; 9 -> 3
      ;; 16 -> 4
      ;; 25 -> 5
    "
  (let ((table (apply #'make-hash-table hash-table-initargs)))
    (dolist (each list table)
      (let ((k-or-kv (multiple-value-list (funcall key each))))
         (setf (gethash (first k-or-kv) table)
               (if (= (length k-or-kv) 2)
                   (second k-or-kv)
                   each))))))
