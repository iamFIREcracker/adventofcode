(defpackage :aoc/2016/06 #.cl-user::*aoc-use*)
(in-package :aoc/2016/06)

(defun parse-recording (data)
  (make-array (list (length data) (length (first data)))
              :initial-contents data))

(defun array-column (array j)
  (loop :for i :below (array-dimension array 0)
        :collect (aref array i j) :into values
        :finally (return (coerce values 'simple-vector))))

(define-problem (2016 06) (recording parse-recording)
  (loop :with columns = (array-dimension recording 1)
        :for j :below columns
        :for column = (array-column recording j)
        :for freqs = (sort (hash-table-alist (frequencies column)) #'> :key #'cdr)
        :collect (car (first freqs)) :into part1
        :collect (caar (last freqs)) :into part2
        :finally (return (values (coerce part1 'string)
                                 (coerce part2 'string)))))

(1am:test test-2016/06
  (multiple-value-bind (part1 part2) (problem-run)
    (1am:is (string= "umcvzsmw" part1))
    (1am:is (string= "rwqoacfz" part2))))
