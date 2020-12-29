(defpackage :aoc/2018/09 #.cl-user::*aoc-use*)
(in-package :aoc/2018/09)

(defun parse-marble-setup (str)
  (let ((parts (split-sequence:split-sequence #\Space str)))
    (values
      (parse-integer (first parts))
      (parse-integer (seventh parts)))))

(defun play (players marbles)
  (loop
    :with board = (make-ring 0)
    :with scores = (make-array players :initial-element 0)
    :for marble :from 1 :to marbles
    :for player = 0 :then (mod (1+ player) players)
    :do (if (dividesp 23 marble)
          (progn
            (incf (aref scores player) marble)
            (ring-movef board -7)
            (incf (aref scores player) (ring-removef board)))
          (progn
            (ring-movef board 1)
            (ring-insertf board marble)))
    :finally (return (reduce #'max scores))))

(define-solution (2018 9) (data first)
  (multiple-value-bind (players marbles) (parse-marble-setup data)
    (values
      (play players marbles)
      (play players (* marbles 100)))))

(define-test (2018 9) (384892 3169872331))
