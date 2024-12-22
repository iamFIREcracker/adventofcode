(defpackage :aoc/2024/22 #.cl-user::*aoc-use*)
(in-package :aoc/2024/22)

#;


(defun parse-input (&optional (strings (uiop:read-file-lines #P"src/2024/day22.txt")))
  (mapcar 'parse-integer strings))
#+#:excluded (parse-input)

(defun mix (n v) (logxor n v))
#+#:excluded (mix 42 15)

(defun prune (n) (mod n 16777216))
#+#:excluded (prune 100000000)

(defun evolve (x n)
  (repeat n
    (setf x (~> x (* 64) (mix x) prune))
    (setf x (~> x (floor ~ 32) (mix x) prune))
    (setf x (~> x (* 2048) (mix x) prune)))
  x)
#+#:excluded (evolve 123 2000)
#+#:excluded (evolve * 1)

#+#:excluded (reduce #'+ (parse-input) :key [evolve _ 2000])
; 17163502021

(defun unit-digit (x) (mod x 10))

(defun prices (x &optional (n 2000))
  (looping
    (collect! (unit-digit x))
    (repeat n
      (setf x (~> x (* 64) (mix x) prune))
      (setf x (~> x (floor ~ 32) (mix x) prune))
      (setf x (~> x (* 2048) (mix x) prune))
      (collect! (unit-digit x)))))
#+#:excluded (prices 123)

(defun collectables (x &optional (n 2000))
  (prog1-let (map (make-hash-table :test 'equal))
    (let1 prices (prices x n)
      (doseqs ((p1 prices)
               (p2 (cdr prices))
               (p3 (cddr prices))
               (p4 (cdddr prices))
               (p5 (cddddr prices)))
        (let1 key (list (- p2 p1) (- p3 p2) (- p4 p3) (- p5 p4))
          (unless (hash-table-key-exists-p map key)
            (setf (gethash key map) p5)))))))
#+#:excluded (collectables 123)

(let1 all-collectables (mapcar 'collectables (parse-input))
  (let1 seen (make-hash-table :test 'equal)
    (looping
      (dolist (coll all-collectables)
        (dohashk (key coll)
          (maximize!
            (looping
              (unless-already-seen (seen key)
                (dolist (coll all-collectables)
                  (awhen (gethash key coll)
                    (sum! it)))))))))))

; 17557521 too high
1938
