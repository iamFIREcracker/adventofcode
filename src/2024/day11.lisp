(defpackage :aoc/2024/11 #.cl-user::*aoc-use*)
(in-package :aoc/2024/11)


(defun parse-input (&optional (strings (uiop:read-file-lines #P"src/2024/day11.txt")))
  (extract-positive-integers (first strings)))
#+#:excluded (parse-input)


(defvar *memo* (make-hash-table :test 'equal))

(defun count-stones (stone blinks)
  (memoizing (*memo* stone blinks)
    (cond ((= blinks 0) 1)
          ((= stone 0) (count-stones 1 (1- blinks)))
          ((evenp (length (spr stone))) (let* ((s (spr stone))
                                               (left (parse-integer (subseq s 0 (/ (length s) 2))))
                                               (right (parse-integer (subseq s (/ (length s) 2)))))
                                          (+ (count-stones left (1- blinks))
                                             (count-stones right (1- blinks)))))
          (t (count-stones (* stone 2024) (1- blinks))))))



(define-solution (2024 11) (stones parse-input)
  (values
    (reduce '+ stones :key [count-stones _ 25])
    (reduce '+ stones :key [count-stones _ 75])))

(define-test (2024 11) (231278 274229228071551))
