(defpackage :aoc/2016/15 #.cl-user::*aoc-use*)
(in-package :aoc/2016/15)

(defun parse-disk (string)
  (cl-ppcre:register-groups-bind ((#'parse-integer disk size time position))
      ("Disc #(\\d+) has (\\d+) positions; at time=(\\d+), it is at position (\\d+)." string)
    (list disk size time position)))

(defun position-generator (disk size time position)
  (let* ((at-pos-0 (- time position))
         (current (- at-pos-0 disk)))
    (labels ((next ()
               (incf current size)
               (cons current #'next)))
      (cons current #'next))))

(defun make-generators (data)
  (let* ((size (length data))
         (disks (mapcar #'parse-disk data))
         (generators (mapcar (partial-1 #'apply #'position-generator) disks)))
    (make-array size :initial-contents generators)))

(defun minmax (gens)
  (loop :with min :with min-pos :with max :with max-pos
        :for (value . ignored) :across gens
        :for i :from 0
        :when (or (not min) (< value min)) :do (setf min value min-pos i)
        :when (or (not max) (> value max)) :do (setf max value max-pos i)
        :finally (return (cons min-pos max-pos))))

(defun solve (data)
  (loop :with gens = (make-generators data)
        :for (min . max) = (minmax gens)
        :if (= min max) :return (car (aref gens min))
        :else :do (setf (aref gens min) (funcall (cdr (aref gens min))))))

(defun prepare-part2 (data)
  (let* ((new-disk-id (1+ (length data)))
         (new-disk (format nil "Disc #~D has 11 positions; at time=0, it is at position 0." new-disk-id)))
    (append data (list new-disk))))

(define-problem (2016 15) (data)
  (values
    (solve data)
    (solve (prepare-part2 data))))

(1am:test test-2016/15
  (multiple-value-bind (part1 part2) (problem-run)
    (1am:is (= 317371 part1))
    (1am:is (= 2080951 part2))))
