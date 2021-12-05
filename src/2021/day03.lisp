(defpackage :aoc/2021/03 #.cl-user::*aoc-use*)
(in-package :aoc/2021/03)


(defun part1 (strings)
  (let* ((gamma (gamma-rate strings))
         (epsilon (map 'string #'ch-complement gamma)))
    (* (parse-binary gamma) (parse-binary epsilon))))

(defun ch-complement (ch) (if (eq ch #\1) #\0 #\1))

(defun parse-binary (s) (parse-integer s :radix 2))


(defun gamma-rate (strings &aux (n (length (first strings))))
  (coerce
    (uiop:while-collecting (bit!)
      (dotimes (i n)
        (bit! (most-common-ch-at strings i))))
    'string))

(defun most-common-ch-at (strings i)
  (loop for s in strings for ch = (aref s i)
        count (eq ch #\1) into ones
        finally (return (if (>= ones (/ (length strings) 2)) #\1 #\0))))


(defun part2 (strings)
  (* (oxygen-generator-rating strings) (co2-scrubber-rating strings)))

(defun oxygen-generator-rating (strings &optional (n (length (first strings))))
  (loop until (= (length strings) 1) for i below n
        for ch = (most-common-ch-at strings i) do
        (setf strings (remove ch strings :key (partial-1 #'aref _ i) :test-not 'eq)))
  (parse-binary (first strings)))

(defun co2-scrubber-rating (strings &optional (n (length (first strings))))
  (loop until (= (length strings) 1) for i below n
        for ch = (ch-complement (most-common-ch-at strings i)) do
        (setf strings (remove ch strings :key (partial-1 #'aref _ i) :test-not 'eq)))
  (parse-binary (first strings)))


(define-solution (2021 03) (data)
  (values (part1 data) (part2 data)))

(define-test (2021 03) (3985686 2555739))
