(defpackage :aoc/2023/05 #.cl-user::*aoc-use*)
(in-package :aoc/2023/05)


(defun parse-map (strings)
  (bnd1 (ranges (looping
                  (dolist (range (rest strings))
                    (destructuring-bind (dest source size)
                        (extract-positive-integers range)
                      (collect! (list source dest size))))))
    (setf ranges (sort ranges #'< :key #'first))
    (cons
      (list 0 0 (first (first ranges)))
      ranges)))

(defun parse-input (&optional (strings (uiop:read-file-lines #P"src/2023/day05.txt")))
  (destructuring-bind (seeds &rest maps)
      (split-sequence:split-sequence "" strings :test #'string=)
    (cons
      (extract-positive-integers (first seeds))
      (mapcar #'parse-map maps))))


(defun find-range (seed ranges)
  (find-if (lambda (range)
             (bnd* ((start (first range))
                    (end (+ start (third range) -1)))
               (and (<= start seed end))))
           ranges))

(defun unmapped-range (start &optional (size 1)) (list start start size))

(defun part1 (&optional (input (parse-input))
                        &aux (seeds (car input)) (maps (cdr input)))
  (looping
    (dolist (seed seeds)
      (dolist (map maps)
        (destructuring-bind (source dest _) (or (find-range seed map)
                                                (unmapped-range seed))
          (declare (ignore _))
          (setf seed (+ (- seed source) dest))))
      (minimize! seed))))


(defun part2 (&optional (input (parse-input))
                        &aux (ranges (subdivide (car input) 2)) (maps (cdr input)))
  (dolist (map maps)
    (bnd1 (ranges-next)
      (doseq ((start length) ranges)
        (while (> length 0)
          (bnd* (((source dest size) (or (find-range start map)
                                         (unmapped-range start length)))
                 (available (- size (- start source)))
                 (mapped (if (< available length) available length)))
            (push (list (+ (- start source) dest)
                        mapped)
                  ranges-next)
            (setf start (+ start mapped) ; was doing (+ start mapped 1)
                  length (- length mapped)))))
      (setf ranges ranges-next)))
  (reduce #'min ranges :key #'first))


(define-solution (2023 05) (input parse-input)
  (values (part1 input) (part2 input)))

(define-test (2023 05) (600279879 20191102))
