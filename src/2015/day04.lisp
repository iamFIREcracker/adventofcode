(defpackage :aoc/2015/04 #.cl-user::*aoc-use*)
(in-package :aoc/2015/04)

(defun parse-secret (lines) (first lines))

(defun five-leading-zeros-p (md5-hash)
  (and (zerop (aref md5-hash 0))
       (zerop (aref md5-hash 1))
       (zerop (ldb (byte 4 4) (aref md5-hash 2)))))

(defun part1 (secret)
  (loop for n from 1 for input = (format nil "~A~D" secret n)
        for hash = (md5:md5sum-string input)
        when (five-leading-zeros-p hash) return n))

(defun six-leading-zeros-p (md5-hash)
  (and (zerop (aref md5-hash 0))
       (zerop (aref md5-hash 1))
       (zerop (aref md5-hash 2))))

(defun part2 (secret)
  (loop for n from 1 for input = (format nil "~A~D" secret n)
        for hash = (md5:md5sum-string input)
        when (six-leading-zeros-p hash) return n))

(define-solution (2015 4) (secret parse-secret)
  (values (part1 secret) (part2 secret)))

(define-test (2015 4) (254575 1038736))
