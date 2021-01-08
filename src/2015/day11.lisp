(defpackage :aoc/2015/11 #.cl-user::*aoc-use*)
(in-package :aoc/2015/11)

(defun next-char (ch)
  (if (char= ch #\z) #\a (code-char (1+ (char-code ch)))))

(defun next-password (password)
  (labels ((recur (password &aux
                           (ch (first password)) (ch-next (next-char ch)))
             (cond ((char/= ch-next #\a) (cons ch-next (rest password)))
                   (t (cons ch-next (recur (rest password)))))))
    (recur password)))

(defun three-decreasing-straight-p (password)
  (loop for ch1 in password for code1 = (char-code ch1)
        for ch2 in (cdr password) for code2 = (char-code ch2)
        for ch3 in (cddr password) for code3 = (char-code ch3)
        thereis (= code1 (1+ code2) (+ code3 2))))

(defun not-confusing-p (password)
  (not (some (lambda (ch) (find ch "ilo" :test #'char=)) password)))

(defun two-different-non-overlapping-pairs-p (password)
  (loop for (ch1 ch2 . rest) on password
        thereis (and (eql ch1 ch2)
                     (loop for (ch3 ch4 ch5) on rest
                           thereis (and (not (eql ch2 ch3))
                                        (eql ch3 ch4)
                                        (not (eql ch4 ch5)))))))

(defun password-valid-p (password)
  (and (three-decreasing-straight-p password)
       (not-confusing-p password)
       (two-different-non-overlapping-pairs-p password)))

(defun new-password (string)
  (flet ((string-to-password (string)
           (reverse (mapcar #'parse-char (cl-ppcre:split "" string))))
         (password-to-string (password)
           (format nil "~{~A~}" (reverse password))))
    (loop with curr = (string-to-password string)
          do (setf curr (next-password curr))
          when (password-valid-p curr) return (password-to-string curr))))

(define-solution (2015 11) (password first)
  (let ((part1 (new-password password)))
    (values part1 (new-password part1))))

(define-test (2015 11) ("vzbxxyzz" "vzcaabcc"))
