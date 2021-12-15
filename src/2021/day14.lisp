(defpackage :aoc/2021/14 #.cl-user::*aoc-use*)
(in-package :aoc/2021/14)


(defun parse-input (data)
  (list (coerce (first data) 'list)
        (parse-insertion-rules (cddr data))))

(defun parse-insertion-rules (data &aux (rules (make-hash-table :test 'equal)))
  (flet ((rule (string)
           (cl-ppcre:register-groups-bind (from to)
               ("(\\w+) -> (\\w)" string)
             (setf (gethash (coerce from 'list) rules) (coerce to 'list)))))
    (dolist (string data rules)
      (rule string))))


(defun massage (input &aux (freqs (make-hash-table :test 'equal)))
  (destructuring-bind (template rules) input
    (loop for (a b) on template do (incf (gethash (list a b) freqs 0)))
    (list freqs rules)))


(defun evolve (times input &aux (polymer (first input)) (rules (second input)))
  (dotimes (_ times) (setf polymer (tick rules polymer)))
  (result polymer))

(defun tick (rules freqs &aux (next (make-hash-table :test 'equal)))
  (loop for (a b) being the hash-keys of freqs using (hash-value n)
        for (z) = (gethash (list a b) rules)
        if z do (incf (gethash (list a z) next 0) n) and do (incf (gethash (list z b) next 0) n)
        else do (incf (gethash (list a b) next 0) n))
  next)


(defun result (freqs &aux (ch-freqs (make-hash-table)))
  (loop for (a) being the hash-keys of freqs using (hash-value n) do
        (incf (gethash a ch-freqs 0) n))
  (- (loop for v being the hash-values of ch-freqs maximize v)
     (loop for v being the hash-values of ch-freqs minimize v)))


(define-solution (2021 14) (input parse-input)
  (let ((input (massage input)))
    (values (evolve 10 input) (evolve 40 input))))

(define-test (2021 14) (5656 12271437788530))
