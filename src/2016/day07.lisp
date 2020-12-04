(defpackage :aoc/2016/07 #.cl-user::*aoc-use*)
(in-package :aoc/2016/07)

(defun parse-ip-parts (string)
  (->< string
       (split-sequence:split-sequence #\[ ><)
       (mapcan (lambda (s) (split-sequence:split-sequence #\] s)) ><)))

(defun parse-ip (string)
  (loop :for split :in (parse-ip-parts string)
        :for i :from 1
        :if (oddp i) :collect split :into normal
        :else :collect split :into hypernet
        :finally (return (list normal hypernet))))

(defun parse-ips (data)
  (mapcar #'parse-ip data))

(defun contains-abba-p (string)
  (loop :for i :from 3 :below (length string)
        :for a = (aref string (- i 3))
        :for b = (aref string (- i 2))
        :for c = (aref string (- i 1))
        :for d = (aref string (- i 0))
        :when (and (char= a d) (char= b c) (char/= a b)) :return t))

(defun supports-tls-p (normal hypernet)
  (and (some #'contains-abba-p normal)
       (not (some #'contains-abba-p hypernet))))

(defun extract-babs (string)
  (loop :for i :from 2 :below (length string)
        :for a = (aref string (- i 2))
        :for b = (aref string (- i 1))
        :for c = (aref string (- i 0))
        :when (and (char= a c) (char/= a b)) :collect (mkstr b a b)))

(defun contains-bab-p (hypernet)
  (lambda (bab)
    (some (lambda (split) (search bab split)) hypernet)))

(defun supports-ssl-p (normal hypernet)
  (loop :for each :in normal
        :for babs = (extract-babs each)
        :when (some (contains-bab-p hypernet) babs) :return t))

(define-solution (2016 7) (ips parse-ips)
  (loop :for (normal hypernet) :in ips
        :count (supports-tls-p normal hypernet) :into part1
        :count (supports-ssl-p normal hypernet) :into part2
        :finally (return (values part1 part2))))

(define-test (2016 7) (110 242))
