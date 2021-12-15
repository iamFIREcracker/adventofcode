(defpackage :aoc/2015/16 #.cl-user::*aoc-use*)
(in-package :aoc/2015/16)

(defparameter *mfcsam-message* '((:children . 3)
                                 (:cats . 7)
                                 (:samoyeds . 2)
                                 (:pomeranians . 3)
                                 (:akitas . 0)
                                 (:vizslas . 0)
                                 (:goldfish . 5)
                                 (:trees . 3)
                                 (:cars . 2)
                                 (:perfumes . 1)))

(defun compound-name (string) (as-keyword string))

(defun parse-compound (string)
  (cl-ppcre:register-groups-bind ((#'compound-name name) (#'parse-integer value))
      ("(\\w+): (\\d+)" string)
    (cons name value)))

(defun parse-compounds (string)
  (mapcar #'parse-compound
          (cl-ppcre:all-matches-as-strings "\\w+: \\d+" string)))

(defun parse-aunt-memories (string)
  (cl-ppcre:register-groups-bind ((#'parse-integer n) (#'parse-compounds compounds))
      ("Sue (\\d+): (.*)" string)
    (cons n compounds)))
(defun aunt-name (aunt) (first aunt))
(defun aunt-compound (aunt name) (rest (assoc name (rest aunt))))

(defun parse-memories (lines) (mapcar #'parse-aunt-memories lines))

(defun absent-or-matches (value) (lambda (v) (or (not v) (= v value))))

(defun part1 (memoirs &aux (remaining memoirs))
  (loop for (name . value) in *mfcsam-message* do
        (setf remaining (remove-if-not
                          (absent-or-matches value)
                          remaining
                          :key (partial-1 #'aunt-compound _ name))))
  (aunt-name (first remaining)))

(defun absent-or-matches-with-ranges (name value)
  #'(lambda (v)
     (or (not v)
         (case name
           ((:cats :trees) (> v value))
           ((:pomeranians :goldfish) (< v value))
           (t (= v value))))))

(defun part2 (memoirs &aux (remaining memoirs))
  (loop for (name . value) in *mfcsam-message* do
        (setf remaining (remove-if-not
                          (absent-or-matches-with-ranges name value)
                          remaining
                          :key (partial-1 #'aunt-compound _ name))))
  (aunt-name (first remaining)))

(define-solution (2015 16) (memoirs parse-memories)
  (values (part1 memoirs) (part2 memoirs)))

(define-test (2015 16) (40 241))
