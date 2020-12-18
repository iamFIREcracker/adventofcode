(defpackage :aoc/2020/09 #.cl-user::*aoc-use*)
(in-package :aoc/2020/09)

(defun validp (validator number)
  (loop for n being the hash-keys of validator
        thereis (gethash (- number n) validator)))

(defun find-first-invalid (numbers)
  (loop with s = (make-hset (subseq numbers 0 25))
        for oldest in numbers for newest in (nthcdr 25 numbers)
        if (not (validp s newest)) return newest
        else do (hset-add oldest s) (hset-add newest s)))

(defun subseq-that-adds-up-to (numbers target
                                       &aux (numbers (coerce numbers 'vector)))
  (loop
    with start = 0 with end = 0 with sum = (aref numbers start)
    if (= sum target) return (coerce (subseq numbers start (1+ end)) 'list)
    else if (> sum target) do (setf sum (- sum (aref numbers start))
                                    start (1+ start))
    else do (setf end (1+ end) sum (+ sum (aref numbers end)))))

(define-solution (2020 9) (numbers parse-integers)
  (let* ((invalid (find-first-invalid numbers))
         (subseq (subseq-that-adds-up-to numbers invalid)))
    (values
      invalid
      (+ (apply 'min subseq) (apply 'max subseq)))))

(define-test (2020 9) (177777905 23463012))
