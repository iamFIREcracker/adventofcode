(defpackage :aoc/2024/22 #.cl-user::*aoc-use*)
(in-package :aoc/2024/22)

(defun parse-input (&optional (strings (uiop:read-file-lines #P"src/2024/day22.txt")))
  (mapcar 'parse-integer strings))
#+#:excluded (parse-input)

(defun mix (n v) (logxor n v))
(defun prune (n) (mod n 16777216))

(defun evolve (x)
  (setf x (~> x (* 64) (mix x) prune))
  (setf x (~> x (floor ~ 32) (mix x) prune))
  (setf x (~> x (* 2048) (mix x) prune)))


(defun unit-digit (x) (mod x 10))

(defun prices (x)
  (looping
    (collect! (unit-digit x))
    (repeat 2000
      (zapf x #'evolve)
      (collect! (unit-digit x)))))
#+#:excluded (prices 123)

(defun winning-by-sequence (prices)
  (prog1-let map (make-hash-table :test 'equal)
    (dosublists ((p1 p2 p3 p4 p5) prices)
      (when (and p1 p2 p3 p4 p5)
        (let1 sequence (list (- p2 p1) (- p3 p2) (- p4 p3) (- p5 p4))
          (unless (hash-table-key-exists-p map sequence)
            (setf (gethash sequence map) p5)))))))
#+#:excluded (winning-by-sequence (prices 123))


(define-solution (2024 22) (secrets parse-input)
  (values (looping
            (dolist (x secrets)
              (repeat 2000 (zapf x #'evolve))
              (sum! x)))
          (let1 bananas-by-sequence (make-hash-table :test 'equal)
            (dolist (x secrets)
              (dohash (sequence price ~x.prices.winning-by-sequence)
                (incf (gethash sequence bananas-by-sequence 0) price)))
            (looping
              (dohashv (bananas bananas-by-sequence)
                (maximize! bananas))))))

(define-test (2024 22) (17163502021 1938))
