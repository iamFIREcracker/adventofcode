(defpackage :aoc/2017/07 #.cl-user::*aoc-use*)
(in-package :aoc/2017/07)

(defstruct (program (:conc-name NIL))
  name
  weight
  programs-above)

(defun parse-programs (x)
  (labels ((parse-programs-above (x)
             (mapcar #'(lambda (s)
                         (if (find #\, s)
                           (subseq s 0 (1- (length s)))
                           s))
                     x))
           (parse-program (s)
             (let* ((splits (split-sequence:split-sequence #\Space s))
                    (name (first splits))
                    (weight (parse-integer (subseq (second splits) 1) :junk-allowed T))
                    (programs-above (and
                                      (>= (length splits) 3)
                                      (parse-programs-above (subseq splits 3)))))
               (make-program :name name
                             :weight weight
                             :programs-above programs-above))))
    (mapcar #'parse-program x)))

(defun find-bottom-program (programs)
  (loop
    :with above = (reduce #'append programs :key #'programs-above)
    :for p :in programs
    :for name = (name p)
    :unless (member name above :test 'equal)
    :do (return name)))

(defun find-corrected-weight (index bottom-program)
  (flet ((program-by-name (name)
           (gethash name index)))
    (recursively ((p (program-by-name bottom-program)))
      (let* ((programs-above (mapcar #'program-by-name (programs-above p)))
             (tower-weights (mapcar #'recur programs-above)))
        (cond
          ((not programs-above) (return-from recur (weight p)))
          ((apply #'= tower-weights) (+ (weight p) (summation tower-weights)))
          (T (let* ((frequencies (mapcar (partial-1 #'count _ tower-weights) tower-weights))
                    (i (position 1 frequencies))
                    (j (position 1 frequencies :test-not #'=))
                    (delta (- (nth i tower-weights) (nth j tower-weights))))
               (return-from find-corrected-weight (- (weight (nth i programs-above)) delta)))))))))

(define-solution (2017 7) (data parse-programs)
  (flet ((index-by-name (programs &aux (index (make-hash-table :test 'equal)))
           (dolist (p programs index)
             (hash-table-insert index (name p) p))))
    (let ((bottom-program (find-bottom-program data)))
      (values
        bottom-program
        (find-corrected-weight (index-by-name data) bottom-program)))))

(define-test (2017 7) ("cyrupz" 193))
