(defpackage :scratch (:use :cl :iterate))
(in-package :scratch)

(defun score (ingredients)
  (reduce #'*
          (list
           (reduce #'+
                   (mapcar #'* ingredients (list 5 -1 0 -1)))
           (reduce #'+
                   (mapcar #'* ingredients (list -1 3 -1 0)))
           (reduce #'+
                   (mapcar #'* ingredients (list 0 0 4 0)))
           (reduce #'+
                   (mapcar #'* ingredients (list 0 0 0 2))))))

(defun solve-a (&aux (iterations 0))
  (values
    (iter outer
          (for frosting from 0 to 100)
          (iter (for candy from frosting to (- 100 frosting))
                (iter (for butterscotch from candy to (- 100 frosting candy))
                      (for sugar = (- 100 frosting candy butterscotch))
                      (in outer
                          (incf iterations)
                          (maximizing (score (list frosting candy butterscotch sugar)))))))
    iterations))

(solve-a)
#; Scratch
(ql:quickload "iterate")
