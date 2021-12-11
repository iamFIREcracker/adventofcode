(defpackage :aoc/2021/11 #.cl-user::*aoc-use*)
(in-package :aoc/2021/11)


(defun parse-octopuses (data &aux (octopuses (make-hash-table :test 'equal)))
  (loop for r below (length data)
        for string in data do
        (loop for c below (length string)
              for ch across string do
              (setf (gethash (list r c) octopuses)
                    (- (char-code ch) (char-code #\0)))))
  octopuses)


(defun flash-dance (octopuses &aux
                              (octopuses (copy-hash-table octopuses))
                              (part1 0))
  (loop for step from 1 for flashes = (dance octopuses)
        sum flashes into flash-count
        when (= step 100) do (setf part1 flash-count)
        when (= flashes 100) return (values part1 step)))


(defun dance (curr &aux (flashed (make-hset nil :test 'equal)))
  (loop for p being the hash-keys of curr
        unless (hset-contains-p p flashed) do
        (incf (gethash p curr))
        (when (> (gethash p curr) 9)
          (hset-add p flashed)
          (loop with remaining = (neighbors curr p)
                while remaining
                for n = (pop remaining)
                unless (hset-contains-p n flashed) do
                (incf (gethash n curr))
                (when (> (gethash n curr) 9)
                  (hset-add n flashed)
                  (setf remaining (append remaining (neighbors curr n)))))))
  (loop for p being the hash-keys of curr using (hash-value e)
        count (when (> e 9) (setf (gethash p curr) 0))))


(defparameter *nhood* '((-1 0) (-1 1) (0 1) (1 1) (1 0) (1 -1) (0 -1) (-1 -1)))

(defun neighbors (energies p)
  (loop for d in *nhood* for n = (mapcar #'+ p d)
        when (gethash n energies) collect n))


(define-solution (2021 11) (octopuses parse-octopuses) (flash-dance octopuses))

(define-test (2021 10) (1665 235))
