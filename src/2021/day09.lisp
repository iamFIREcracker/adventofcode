(defpackage :aoc/2021/09 #.cl-user::*aoc-use*)
(in-package :aoc/2021/09)


(defun parse-heights (data &aux (heights (make-hash-table :test 'equal)))
  (loop for r below (length data)
        for string in data do
        (loop for c below (length string)
              for ch across string do
              (setf (gethash (list r c) heights)
                    (- (char-code ch) (char-code #\0)))))
  heights)


(defun part1 (heights)
  (loop for p in (low-points heights) sum (1+ (gethash p heights))))


(defun low-points (heights)
  (loop for p being the hash-keys of heights using (hash-value h)
        when (every (lambda (n) (< h (gethash n heights))) (neighbors heights p))
        collect p))


(defun neighbors (heights p)
  (loop for n in (adjacents p)
        when (gethash n heights) collect n))


(defun part2 (heights) (reduce #'* (subseq (sort (basins heights) #'>) 0 3)))


(defun basins (heights)
  (looping
    (dolist (lp (low-points heights))
      (let ((size 0))
        (bfs lp
             :test 'equal
             :neighbors (lambda (p)
                          (incf size)
                          (remove-if-not (lambda (n)
                                           (< (gethash n heights) 9))
                                         (neighbors heights p))))
        (collect! size)))))


#+#:alternate-solution-with-union-find (defun basins (heights &aux
                       (basins (make-hash-table :test 'equal))
                       (sizes (make-hash-table :test 'eq)))
  (loop for p being the hash-keys of heights using (hash-value h)
        unless (= h 9) do (setf (gethash p basins) (make-dset h)))
  (loop for p being the hash-keys of basins using (hash-value ds) do
        (loop for np in (neighbors heights p)
              for nds = (gethash np basins)
              when nds do (dset-union ds nds)))
  (loop for ds being the hash-values of basins do (incf (gethash (dset:dset-find ds) sizes 0)))
  (hash-table-values sizes))


(define-solution (2021 09) (heights parse-heights)
  (values (part1 heights) (part2 heights)))

(define-test (2021 09) (502 1330560))
