(defpackage :aoc/2025/08 #.cl-user::*aoc-use*)
(in-package :aoc/2025/08)

(defun read-junction-boxes (&optional (strings (uiop:read-file-lines #P"src/2025/day08.txt")))
  (mapcar #'extract-integers strings))


(defun euclidean-distance (p1 p2)
  (sqrt (reduce #'+ (mapcar #'* (mapcar #'- p1 p2) (mapcar #'- p1 p2)))))

(defun all-connections (boxes)
  (let1 all (looping
              (dosublists ((b1 . boxes2) boxes)
                (dolist (b2 boxes2)
                  (let1 d (euclidean-distance (dset-value b1) (dset-value b2))
                    (collect! (list b1 b2 d))))))
    (sort all '< :key 'third)))


(defun part1 (&optional (boxes (read-junction-boxes)) (n 1000))
  (setf boxes (mapcar #'make-dset boxes))
  (let1 conn (all-connections boxes)
    (repeat n
      (destructuring-bind (b1 b2) (butlast (pop conn))
        (unless (eq (dset-find b1) (dset-find b2))
          (dset-union b1 b2)))))
  (~> (mapcar [dset-value (dset-find _)] boxes)
    frequencies
    (sort ~ '> :key 'cdr)
    (subseq ~ 0 3)
    (reduce #'* ~ :key 'cdr)))


(defun part2 (&optional (boxes (read-junction-boxes)))
  (setf boxes (mapcar #'make-dset boxes))
  (let1 conn (all-connections boxes)
    (prog1-let last nil
      (while conn
        (destructuring-bind (b1 b2) (butlast (pop conn))
          (unless (eq (dset-find b1) (dset-find b2))
            (dset-union b1 b2)
            (setf last (* (car (dset-value b1)) (car (dset-value b2))))))))))


(define-solution (2025 08) (boxes read-junction-boxes)
  (values (part1 boxes 1000)
          (part2 boxes)))

(define-test (2025 06) (62186 8420405530))
