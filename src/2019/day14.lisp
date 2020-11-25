(defpackage :aoc/2019/14 #.cl-user::*aoc-use*)
(in-package :aoc/2019/14)

(defstruct (chemical (:conc-name c-))
  type
  quantity)

(defun parse-chemical (str)
  (let* ((parts (split-sequence:split-sequence #\Space str))
         (quantity (parse-integer (if (string= (first parts) "")
                                    (second parts)
                                    (first parts))))
         (type (if (string= (first parts) "")
                 (third parts)
                 (second parts))))
    (make-chemical :type (remove #\, type)
                   :quantity quantity)))

(defstruct (reaction (:conc-name r-))
  from
  to)

(defun parse-reaction (str &aux (parts (split-sequence:split-sequence #\= str)))
  (let ((parts-pre (split-sequence:split-sequence #\, (first parts)))
        (parts-post (remove #\> (second parts))))
    (make-reaction :from (mapcar #'parse-chemical parts-pre)
                   :to (parse-chemical parts-post))))

(defstruct (reactions-list (:constructor make-reactions-list%)
                           (:copier NIL)
                           (:conc-name rl-))
  list)

(defun make-reactions-list (reactions &aux (list (make-hash-table :test 'equal)))
  (prog1 (make-reactions-list% :list list)
    (dolist (r reactions)
      (multiple-value-bind (existing existingp)
          (gethash (c-type (r-to r)) list)
        (when existingp
          (error (format NIL "Multiple rules for the same chemical-type: ~a and ~a" r existing)))
        (setf (gethash (c-type (r-to r)) list) r)))))

(defun read-reactions-list (data)
  (make-reactions-list (mapcar #'parse-reaction data)))

(defun rl-reaction-for-type (rl type)
  (gethash type (rl-list rl)))

(defun rl-generate-fuel (rl &optional (quantity 1))
  (let ((stock (make-hash-table :test 'equal))
        (consumed-ore 0))
    (recursively ((type "FUEL")
                  (quantity quantity))
      (let* ((existing (gethash type stock 0)))
        (cond ((>= existing quantity) (decf (gethash type stock) quantity))
              ((string= type "ORE") (incf consumed-ore (- quantity existing)))
              (T (let* ((rl (rl-reaction-for-type rl type))
                        (reactions (ceiling (- quantity existing) (c-quantity (r-to rl))))
                        (waste (- (* reactions (c-quantity (r-to rl))) quantity)))
                   (dolist (d (r-from rl))
                     (recur (c-type d) (* (c-quantity d) reactions)))
                   (incf (gethash type stock 0) waste))))))
    consumed-ore))

(define-problem (2019 14) (rl read-reactions-list)
  (let* ((cargo-limit 1000000000000)
         (ore-per-fuel (rl-generate-fuel rl 1))
         (min-fuel (floor cargo-limit ore-per-fuel)))
    (values
      ore-per-fuel
      (binary-search min-fuel cargo-limit
                     (lambda (fuel)
                       (let ((ore (rl-generate-fuel rl fuel)))
                         (<=> ore cargo-limit)))))))

(1am:test test-2019/14
  (multiple-value-bind (part1 part2) (problem-run)
    (1am:is (= 443537 part1))
    (1am:is (= 2910558 part2))))
