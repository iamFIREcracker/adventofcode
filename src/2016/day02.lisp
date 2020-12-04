(defpackage :aoc/2016/02 #.cl-user::*aoc-use*)
(in-package :aoc/2016/02)

(defparameter *keypad-part1* (make-array '(3 3) :initial-contents '("123"
                                                                    "456"
                                                                    "789")))

(defparameter *keypad-part2* (make-array '(5 5) :initial-contents '("--1--"
                                                                    "-234-"
                                                                    "56789"
                                                                    "-ABC-"
                                                                    "--D--")))

(defun read-single-key-instructions (string)
  (map 'list (lambda (c)
               (ecase c
                 (#\U '(-1 . 0))
                 (#\R '(0 . 1))
                 (#\D '(1 . 0))
                 (#\L '(0 . -1))))
       string))

(defun read-instructions (data)
  (mapcar #'read-single-key-instructions data))

(defun find-code (instructions keypad i j)
  (loop
    :for single-key-instructions :in instructions
    :do (loop
          :for (di . dj) :in single-key-instructions
          :for ni = (+ i di) :for nj = (+ j dj)
          :when (and (array-in-bounds-p keypad ni nj)
                     (not (eql (aref keypad ni nj) #\-)))
          :do (setf i ni
                    j nj))
    :collect (aref keypad i j) :into numbers
    :finally (return (format nil "~{~a~}" numbers))))

(define-solution (2016 2) (instructions read-instructions)
  (values (find-code instructions *keypad-part1* 1 1)
          (find-code instructions *keypad-part2* 2 0)))

(define-test (2016 2) ("48584" "563B6"))
