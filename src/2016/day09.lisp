(defpackage :aoc/2016/09 #.cl-user::*aoc-use*)
(in-package :aoc/2016/09)

(defparameter *version* 1)

(defun message-length (string)
  (or (message-compressed-length string)
      (message-uncompressed-length string)
      0))

(defun message-compressed-length (string)
  (cl-ppcre:register-groups-bind ((#'parse-integer size times))
      ("^\\((\\d+)x(\\d+)\\)" string)
    (let* ((match-size (1+ (position #.(char ")" 0) string)))
           (size-uncompressed (if (= *version* 1)
                                size
                                (message-length (subseq string match-size (+ match-size size))))))
      (+ (* size-uncompressed times)
         (message-length (subseq string (+ match-size size)))))))

(defun message-uncompressed-length (string)
  (cl-ppcre:register-groups-bind (raw)
      ("^([^(]+)" string)
    (+ (length raw) (message-length (subseq string (length raw))))))

(define-problem (2016 09) (message first)
  (values (message-length message)
          (let ((*version* 2))
            (message-length message))))

(1am:test test-2016/09
  (multiple-value-bind (part1 part2) (problem-run)
    (1am:is (= 98135 part1))
    (1am:is (= 10964557606 part2))))
