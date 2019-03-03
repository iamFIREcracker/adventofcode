(defpackage :aoc/2018/09 #.cl-user::*aoc-use*)
(in-package :aoc/2018/09)

(defstruct (ring (:constructor make-ring%))
  current)

(defun make-ring (content)
  "Creates a circular `DLINK` having a single element with `CONTENT`."
  (let ((element (make-dlink :content content)))
    (setf (dlink-prev element) element
          (dlink-next element) element)
    (make-ring% :current element)))

(defun ring-movef (r offset)
  (let ((move (if (> offset 0) #'dlink-next #'dlink-prev)))
    (dotimes (i (abs offset))
      (setf (ring-current r) (funcall move (ring-current r))))))

(defun ring-removef (r &aux (current (ring-current r)))
  (let* ((content (dlink-removef current)))
    (setf (ring-current r) (dlink-next current))
    content))

(defun ring-insertf (r content &aux (current (ring-current r)))
  (setf (ring-current r) (dlink-insertf current content)))

(defun parse-marble-setup (str)
  (let ((parts (split-sequence:split-sequence #\Space str)))
    (values
      (parse-integer (first parts))
      (parse-integer (seventh parts)))))

(defun play (players marbles)
  (let ((board (make-ring 0))
        (scores (make-array players :initial-element 0)))
    (loop
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
      :finally (return (maximization scores)))))

(define-problem (2018 9) (data first)
  (multiple-value-bind (players marbles) (parse-marble-setup data)
    (values
      (play players marbles)
      (play players (* marbles 100)))))

(1am:test test-2018/09
  (multiple-value-bind (part1 part2) (problem-run)
    (1am:is (= 384892 part1))
    (1am:is (= 3169872331 part2))))
