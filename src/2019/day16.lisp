(defpackage :aoc/2019/16 #.cl-user::*aoc-use*)
(in-package :aoc/2019/16)

(defun read-string (data)
  (first data))

(defun make-pattern (n)
  (let* ((first (make-list n :initial-element 0))
         (second (make-list n :initial-element 1))
         (third (make-list n :initial-element 0))
         (fourth (make-list n :initial-element -1))
         (pattern (nconc first second third fourth)))
    (ncycle pattern)
    (cdr pattern)))

(defun fft (digits)
  (loop
    :for i :from 1 :upto (length digits)
    :collecting (loop
                  :for d :in digits
                  :for p :in (make-pattern i)
                  :summing (* d p) :into sum
                  :finally (return (abs (rem sum 10))))))

(defun firt-8-digits (digits)
  (format NIL "~{~A~}" (subseq digits 0 8)))

(defun solve-part1 (message)
  (loop
    :for n = 0 :then (1+ n)
    :for digits = (reverse (digits message)) :then (fft digits)
    :when (= n 100)
    :return (firt-8-digits digits)))

(defun repeat (message times)
  (loop
    :for i :from 1 :to times
    :appending message))

(defun firt-7-digits-as-integer (digits)
  (parse-integer (format NIL "~{~a~}" (subseq digits 0 7))))

(defun fft-2 (digits)
  (loop
    :with sum = 0
    :for d :in (reverse digits)
    :do (incf sum d)
    :collecting (rem sum 10) into output
    :finally (return (nreverse output))))

(defun solve-part2 (message &aux (message-digits (repeat (reverse (digits message)) 10000)))
  (loop
    :with offset = (firt-7-digits-as-integer message-digits)
    :for n = 0 :then (1+ n)
    :for digits = (subseq message-digits offset) :then (fft-2 digits)
    :when (= n 100)
    :return (firt-8-digits digits)))

(define-solution (2019 16) (message read-integer)
  (values
    (solve-part1 message)
    (solve-part2 message)))

(define-test (2019 16) ("63483758" "96099551"))
