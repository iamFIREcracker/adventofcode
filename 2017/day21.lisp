(defpackage :aoc/2017/21 #.cl-user::*aoc-use*)
(in-package :aoc/2017/21)

(defun size (source)
  (multiple-value-bind (d r)
      (truncate (sqrt (length source)))
    (unless (zerop r)
      (error "~s is not square" source))
    d))

(defun indices-flip-v (size)
  (if (= size 2)
    `((1 0) (1 1)
      (0 0) (0 1))
    `((2 0) (2 1) (2 2)
      (1 0) (1 1) (1 2)
      (0 0) (0 1) (0 2))))

(defun indices-rotate-cw (size)
  (if (= size 2)
    `((1 0) (0 0)
      (1 1) (0 1))
    `((2 0) (1 0) (0 0)
      (2 1) (1 1) (0 1)
      (2 2) (1 2) (0 2))))

(defun indices-square (top left size)
  (gathering
    (dorange (i top (+ top size))
      (dorange (j left (+ left size))
        (gather (list i j))))))

(defun select-pixels (source indices &aux (size (size source)))
  (loop
    :for (i j) :in indices
    :for flattened = (+ (* i size) j)
    :collect (aref source flattened) :into pixels
    :finally (return (apply #'mkstr pixels))))

(defun generate-patterns (original &aux (size (size original)))
  (let* ((rotated-cw1 (select-pixels original (indices-rotate-cw size)))
         (rotated-cw2 (select-pixels rotated-cw1 (indices-rotate-cw size)))
         (rotated-cw3 (select-pixels rotated-cw2 (indices-rotate-cw size))))
    (list original
          (select-pixels original (indices-flip-v size))
          rotated-cw1
          (select-pixels rotated-cw1 (indices-flip-v size))
          rotated-cw2
          (select-pixels rotated-cw2 (indices-flip-v size))
          rotated-cw3
          (select-pixels rotated-cw3 (indices-flip-v size)))))

(defun parse-rules (x &aux (rules (make-hash-table :test 'equal)))
  (flet ((parse-rule (s &aux (splits (split-sequence:split-sequence #\Space s)))
           (let ((from (remove #\/ (first splits)))
                 (to (split-sequence:split-sequence #\/ (third splits))))
             (list from to))))
    (loop
      :for (from to) :in (mapcar #'parse-rule x)
      :do (loop
            :for p :in (generate-patterns from)
            :do (setf (gethash p rules) to)))
    rules))

(defun break-up (pixels &aux (size (size pixels)))
  (let ((sub-size (if (dividesp 2 size) 2 3)))
    (gathering
      (dorange (i 0 size sub-size)
        (dorange (j 0 size sub-size)
          (gather (select-pixels pixels (indices-square i j sub-size))))))))

(defun combine (squares &aux (size (size squares)))
  ;; XXX review this?!
  (loop
    :for start = 0 :then end
    :for end = (+ start size)
    :while (< start (length squares))
    :for foo = (subseq squares start end)
    :collect (apply #'mkstr (apply #'mapcar #'mkstr foo)) :into lines
    :finally (return (apply #'mkstr lines))))

(define-problem (2017 21) (data parse-rules)
  (flet ((pixels-on (pixels)
           (count #\# pixels)))
    (loop
      :with pixels = ".#...####"
      :with part1
      :for iteration :from 1 :upto 18
      :for grids = (break-up pixels)
      :for expansions = (mapcar (curry #'gethash _ data) grids)
      :do (setf pixels (combine expansions))
      :when (= iteration 5) :do (setf part1 (pixels-on pixels))
      :finally (return (values part1 (pixels-on pixels))))))

(1am:test test-2017/21
  (multiple-value-bind (part1 part2) (problem-run)
    (1am:is (= 188 part1))
    (1am:is (= 2758764 part2))))
