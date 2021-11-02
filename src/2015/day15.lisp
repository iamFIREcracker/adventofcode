(defpackage :aoc/2015/15 #.cl-user::*aoc-use*)
(in-package :aoc/2015/15)

(defun parse-properties (string)
  (mapcar #'parse-integer (cl-ppcre:all-matches-as-strings "-?\\d+" string)))
(defun calories (props) (nth 4 props))

(defun parse-ingredient (string)
  (cl-ppcre:register-groups-bind (name (#'parse-properties properties))
      ("(\\w+): (.*)" string)
    (cons name properties)))
(defun properties (x) (cdr x))

(defun parse-ingredients (lines) (mapcar #'parse-ingredient lines))

(defun make-cookie (ingredients quantities)
  (assert (= (reduce #'+ quantities) 100))
  (apply #'mapcar
         #'+
         (loop for i in ingredients for q in quantities
               collect (mapcar (partial-1 #'* q) (properties i)))))

(defun total-score (cookie)
  (reduce #'* (remove-if (partial-1 #'< _ 0) (subseq cookie 0 4))))

(defun part1 (ingredients)
  (labels ((recur (ingredients-left spoons-left recipe)
             (cond ((= ingredients-left 1) (total-score
                                             (make-cookie
                                               ingredients
                                               (cons spoons-left recipe))))
                   (t (loop for spoons from 0 to spoons-left
                            maximize (recur
                                       (1- ingredients-left)
                                       (- spoons-left spoons)
                                       (cons spoons recipe)))))))
    (recur (length ingredients) 100 '())))

(defun part2 (ingredients)
  (labels ((recur (ingredients-left spoons-left recipe)
             (cond ((= ingredients-left 1)
                    (let ((cookie (make-cookie
                                    ingredients
                                    (cons spoons-left recipe))))
                      (if (= (calories cookie) 500)
                        (total-score cookie)
                        0)))
                   (t (loop for spoons from 0 to spoons-left
                            maximize (recur
                                       (1- ingredients-left)
                                       (- spoons-left spoons)
                                       (cons spoons recipe)))))))
    (recur (length ingredients) 100 '())))

(define-solution (2015 15) (ingredients parse-ingredients)
  (values (part1 ingredients) (part2 ingredients)))

(define-test (2015 15) (13882464 11171160))
