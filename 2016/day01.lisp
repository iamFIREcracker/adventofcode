(defpackage :aoc/2016/01 #.cl-user::*aoc-use*)
(in-package :aoc/2016/01)

(defun read-instructions (data)
  (mapcar (lambda (step)
            (let ((rotation (if (eql (aref step 0) #\L) #c(0 1) #c(0 -1)))
                  (walk-for (parse-integer (subseq step 1) :junk-allowed t)))
              (cons rotation walk-for)))
          (split-sequence:split-sequence #\Space (first data))))

(defun problem-run ()
  (let* ((data (uiop:read-file-lines #p"./2016/day01.txt"))
         (instructions (read-instructions data)))
    (loop
      :with pos = #c(0 0) :with part2 :with seen
      :for (change-dir . walk-for) :in instructions
      :for heading = (* #c(0 1) change-dir) :then (* heading change-dir)
      :do (loop
            :repeat walk-for
            :do (progn
                  (incf pos heading)
                  (when (and (not part2) (member pos seen))
                    (setf part2 pos))
                  (push pos seen)))
      :finally (return (values (manhattan-distance pos 0)
                               (manhattan-distance part2 0))))))

(1am:test test-2016/01
  (multiple-value-bind (part1 part2) (problem-run)
    (1am:is (= 278 part1))
    (1am:is (= 161 part2))))
