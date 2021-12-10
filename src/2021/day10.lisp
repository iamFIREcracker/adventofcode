(defpackage :aoc/2021/10 #.cl-user::*aoc-use*)
(in-package :aoc/2021/10)


(defparameter *closing*
  '((#\( . #\))
    (#\[ . #\])
    (#\{ . #\})
    (#\< . #\>)))

(defun syntax-scoring (file &aux (error-score 0) completion-scores)
  (dolist (line file)
    (let (stack)
      (loop for ch across line do
            (cond ((find ch "([{<") (push (cdr (assoc ch *closing*)) stack))
                  ((eq ch (car stack)) (pop stack))
                  (t (return (incf error-score (syntax-error-score ch)))))
            finally (when stack
                      (push (completion-score stack) completion-scores)))))
  (values
    error-score
    (let* ((completion-scores (sort completion-scores #'<))
           (n (floor (length completion-scores) 2)))
      (nth n completion-scores))))


(defparameter *syntax-error-table*
  '((#\) . 3)
    (#\] . 57)
    (#\} . 1197)
    (#\> . 25137)))

(defun syntax-error-score (ch) (cdr (assoc ch *syntax-error-table*)))


(defparameter *point-value-table*
  '((#\) . 1)
    (#\] . 2)
    (#\} . 3)
    (#\> . 4)))

(defun completion-score (stack &aux (score 0))
  (dolist (ch stack score)
    (setf score (+ (* score 5) (cdr (assoc ch *point-value-table*))))))


(define-solution (2021 10) (data) (syntax-scoring data))

(define-test (2021 10) (323613 3103006161))
