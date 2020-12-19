(defpackage :aoc/2020/19 #.cl-user::*aoc-use*)
(in-package :aoc/2020/19)

(defun parse-sub-rule (string)
  (if (find #\" string)
    (list (string (char string 1)))
    (mapcar #'parse-integer (cl-ppcre:split " " string))))

(defun parse-sub-rules (string)
  (let ((sub-rules (cl-ppcre:split " \\| " string)))
    (mapcar #'parse-sub-rule sub-rules)))

(defun parse-rule (string)
  (cl-ppcre:register-groups-bind ((#'parse-integer id) rest)
      ("(\\d+): (.*)" string)
    (list id (parse-sub-rules rest))))

(defun id (rule) (car rule))
(defun sub-rules (rule) (cadr rule))

(defun parse-input (data)
  (let (groups current)
    (dolist (string (append data '("")))
      (if (string= string "")
        (setf groups (cons (reverse current) groups) current nil)
        (setf current (cons string current))))
    (list
      (mapcar #'parse-rule (second groups))
      (first groups))))

(defun rules (input) (car input))
(defun messages (input) (cadr input))

(defparameter *max-recursion* 20)

(defun make-regexp (rules)
  (let ((sub-rules-map (make-hash-table)))
    (dolist (rule rules)
      (setf (gethash (id rule) sub-rules-map) (sub-rules rule)))
    (labels ((recur (id depth &aux (sub-rules (gethash id sub-rules-map)))
               (cond
                 ((= depth *max-recursion*) "") ; too much recursion, give up!
                 ((not (numberp id)) id)        ; termination rule, return it.
                 (t
                   (format nil "(~{~A~^|~})"
                           (loop for sub-rule in sub-rules collect
                                 (format nil "~{~A~}"
                                         (loop for id in sub-rule
                                               collect (recur id (1+ depth))))))))))
      (format nil "^~A$" (recur 0 0)))))

(defun count-matches (data &aux (input (parse-input data)))
  (let* ((regexp (make-regexp (rules input)))
         (scanner (cl-ppcre:create-scanner regexp)))
    (count-if (lambda (s) (cl-ppcre:all-matches scanner s)) (messages input))))

(defun prepare-part2 (data)
  (loop for string in data
        if (string= string "8: 42") collect "8: 42 | 42 8"
        else if (string= string "11: 42 31") collect "11: 42 31 | 42 11 31"
        else collect string))

(define-solution (2020 19) (data)
  (values (count-matches data)
          (count-matches (prepare-part2 data))))

(define-test (2020 19) (248 381))
