(defpackage :aoc/2017/04 #.cl-user::*aoc-use*)
(in-package :aoc/2017/04)

(defun read-strings-and-split (x)
  (mapcar #'(lambda (s)
              (split-sequence:split-sequence #\Space s))
          x))

(define-solution (2017 4) (data read-strings-and-split)
  (flet ((duplicate-words-p (pass-phrase)
           (loop
             :for (a . rest) :on pass-phrase
             :thereis (member a rest :test 'equal)))
         (sort-each-word (pass-phrase)
           (mapcar #'nsorted pass-phrase)))
    (values
      (count-if-not #'duplicate-words-p data)
      (count-if-not #'duplicate-words-p data :key #'sort-each-word))))

(define-test (2017 4) (451 223))
