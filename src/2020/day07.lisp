(defpackage :aoc/2020/07 #.cl-user::*aoc-use*)
(in-package :aoc/2020/07)

(defun bag-type (string)
  (make-keyword (string-upcase (substitute #\- #\Space string))))

(defun parse-bag-content (string)
  (cl-ppcre:register-groups-bind ((#'parse-integer number)
                                  (#'bag-type type)
                                  rest)
      ("(\\d+) ([a-z ]+) bags?(.*)" string)
    (cons (cons type number) (parse-bag-content rest))))

(defun parse-rule (string)
  (cl-ppcre:register-groups-bind ((#'bag-type type) rest)
      ("([a-z ]+) bags contain (.*)" string)
    (list type (parse-bag-content rest))))

(defun parse-rules (data)
  (mapcar #'parse-rule data))

(defun contains-shiny-gold-p (rules curr)
  (or (eq curr :shiny-gold)
      (some (partial-1 'contains-shiny-gold-p rules (first _))
            (second (assoc curr rules)))))

(defun count-bags-inside (curr rules)
  (loop for (type . n) in (second (assoc curr rules))
        sum (+ (* (1+ (count-bags-inside type rules)) n))))

(define-solution (2020 7) (rules parse-rules)
  (values
    (count-if (partial-1 #'contains-shiny-gold-p rules)
              (remove :shiny-gold rules :key #'first)
              :key #'first)
    (count-bags-inside :shiny-gold rules)))

(define-test (2020 7) (148 24867))
