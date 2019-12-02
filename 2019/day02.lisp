(defpackage :aoc/2019/02 #.cl-user::*aoc-use*)
(in-package :aoc/2019/02)

(defun read-program (data &aux (str (first data)))
  (let* ((list (split-sequence:split-sequence #\, str))
         (list (mapcar #'parse-integer list)))
    (make-array (length list) :initial-contents list)))

(defun copy-program (program noun verb &aux (program (copy-seq program)))
  (setf (aref program 1) noun
        (aref program 2) verb)
  program)

(defun program-output (program)
  (flet ((read-from (loc)
           (aref program loc))
         (write-at (loc value)
           (setf (aref program loc) value)))
    (loop
      :with ip = 0
      :for curr = (aref program ip)
      :when (= curr 99) :return (aref program 0)
      :when (= curr 1) :do (let ((v1 (read-from (read-from (+ ip 1))))
                                 (v2 (read-from (read-from (+ ip 2)))))
                             (write-at (read-from (+ ip 3)) (+ v1 v2)))
      :when (= curr 2) :do (let ((v1 (read-from (read-from (+ ip 1))))
                                 (v2 (read-from (read-from (+ ip 2)))))
                             (write-at (read-from (+ ip 3)) (* v1 v2)))
      :do (setf ip (+ ip 4)))))

(define-problem (2019 2) (program read-program)
  (values
    (program-output (copy-program program 12 2))
    (block part-2
      (dorange (noun 0 100)
        (dorange (verb 0 100)
          (let* ((program (copy-program program noun verb))
                 (result (program-output program)))
            (when (= 19690720 result)
              (return-from part-2 (+ (* 100 noun) verb)))))))))

(1am:test test-2019/02
  (multiple-value-bind (part1 part2) (problem-run)
    (1am:is (= 5866714 part1))
    (1am:is (= 5208 part2))))
