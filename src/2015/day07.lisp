(defpackage :aoc/2015/07 #.cl-user::*aoc-use*)
(in-package :aoc/2015/07)

(defun parse-provision (string)
  (cl-ppcre:register-groups-bind (in out)
                                 asdfdasf
      ("^(\\w+) -> (\\w+)" string)
    (list out #'identity in)))

(defun parse-and (string)
  (cl-ppcre:register-groups-bind (in n out)
      ("^(\\w+) AND (\\w+) -> (\\w+)" string)
    (list out #'logand in n)))

(defun parse-or (string)
  (cl-ppcre:register-groups-bind (in n out)
      ("^(\\w+) OR (\\w+) -> (\\w+)" string)
    (list out #'logior in n)))

(defun parse-lshift (string)
  (cl-ppcre:register-groups-bind (in n out)
      ("^(\\w+) LSHIFT (\\w+) -> (\\w+)" string)
    (list out #'ash in n)))

(defun parse-rshift (string)
  (cl-ppcre:register-groups-bind (in n out)
      ("^(\\w+) RSHIFT (\\w+) -> (\\w+)" string)
    (list out #'(lambda (in n) (ash in (- n))) in n)))

(defun parse-not (string)
  (cl-ppcre:register-groups-bind (in out)
      ("^NOT (\\w+) -> (\\w+)" string)
    (list out #'lognot in)))

(defun parse-instruction (string)
  (or (parse-provision string)
      (parse-and string)
      (parse-or string)
      (parse-lshift string)
      (parse-rshift string)
      (parse-not string)))

(defun parse-instructions (lines)
  (mapcar #'parse-instruction lines))

(defun signal-at (wire instructions &aux (memo (make-hash-table :test 'eql)))
  (labels ((recur (wire &aux (cached (gethash wire memo)))
             (if cached
               cached
               (setf (gethash wire memo)
                     (cond ((digit-char-p (char wire 0)) (parse-integer wire))
                           (t
                             (destructuring-bind (fun . args)
                                 (rest (assoc wire instructions :test #'string=))
                               (apply fun (mapcar #'recur args)))))))))
    (recur wire)))

(defun prepare-part2 (instructions part1)
  (cons
    (list "b" #'identity (format nil "~A" part1))
    instructions))

(define-solution (2015 7) (instructions parse-instructions)
  (let ((part1 (signal-at "a" instructions)))
    (values part1
            (signal-at "a" (prepare-part2 instructions part1)))))

(define-test (2015 7) (46065 14134))
