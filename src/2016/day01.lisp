(defpackage :aoc/2016/01 #.cl-user::*aoc-use*)
(in-package :aoc/2016/01)

(defun read-instructions (data)
  (mapcar (lambda (step)
            (let ((rotation (if (eql (aref step 0) #\L) #c(0 1) #c(0 -1)))
                  (walk-for (parse-integer (subseq step 1) :junk-allowed t)))
              (cons rotation walk-for)))
          (split-sequence:split-sequence #\Space (first data))))

(define-solution (2016 1) (instructions read-instructions)
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
                             (manhattan-distance part2 0)))))

(define-test (2016 1) (278 161))
