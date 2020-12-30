(defpackage :aoc/2016/06 #.cl-user::*aoc-use*)
(in-package :aoc/2016/06)

(defun parse-recording (data)
  (make-array (list (length data) (length (first data)))
              :initial-contents data))

(defun array-column (array j)
  (loop :for i :below (array-dimension array 0)
        :collect (aref array i j) :into values
        :finally (return (coerce values 'simple-vector))))

(define-solution (2016 6) (recording parse-recording)
  (loop :with columns = (array-dimension recording 1)
        :for j :below columns
        :for column = (array-column recording j)
        :for freqs = (sort (frequencies column) #'> :key #'cdr)
        :collect (car (first freqs)) :into part1
        :collect (caar (last freqs)) :into part2
        :finally (return (values (coerce part1 'string)
                                 (coerce part2 'string)))))

(define-test (2016 6) ("umcvzsmw" "rwqoacfz"))
