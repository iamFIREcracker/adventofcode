(defpackage :aoc/2017/10 #.cl-user::*aoc-use*)
(in-package :aoc/2017/10)

(defvar *size*)
(setf *size* 256)

(defvar *additional-lengths*)
(setf *additional-lengths* (list 17 31 73 47 23))

(defun parse-line-of-integers (s)
  (mapcar #'parse-integer (split-sequence:split-sequence #\, s)))

(defun parse-ascii-codes (s)
  (mapcar #'char-int (coerce s 'list)))

(defun scramble (hash lengths current skip)
  (flet ((twist (hash start end)
           (loop
             :for i = start :then (1+ i)
             :for j = end :then (1- j)
             :for temp = (aref hash (mod i *size*))
             :while (<= i j)
             :do (setf (aref hash (mod i *size*)) (aref hash (mod j *size*))
                       (aref hash (mod j *size*)) temp))))
    (loop
      :for len :in lengths
      :for i = current
      :for j = (+ i (1- len))
      :do (twist hash i j)
      :do (incf current (+ len skip))
      :do (incf skip)
      :finally (return (values current skip)))))

(defun dense-hash (hash)
  (loop
    :for i = 0 :then (+ i 16)
    :while (array-in-bounds-p hash i)
    :for slice = (subseq hash i (+ i 16))
    :collecting (reduce #'logxor slice)))

(define-problem (2017 10) (data first)
  (flet ((init-hash (&aux (hash (make-array *size*)))
           (dotimes (n *size* hash)
             (setf (aref hash n) n))))
    (values
      (let ((hash (init-hash))
            (lengths (append (parse-line-of-integers data) *additional-lengths*)))
        (scramble hash lengths 0 0)
        (* (aref hash 0) (aref hash 1)))
      (let ((hash (init-hash))
            (lengths (append (parse-ascii-codes data) *additional-lengths*))
            (current 0)
            (skip 0))
        (dotimes (n 64 (hexadecimal-string (dense-hash hash)))
          (multiple-value-bind (c s) (scramble hash lengths current skip)
            (setf current c
                  skip s)))))))

(1am:test test-2017/10
  (multiple-value-bind (part1 part2) (problem-run)
    (1am:is (= 23874 part1))
    (1am:is (string= "e1a65bfb5a5ce396025fab5528c25a87" part2))))
