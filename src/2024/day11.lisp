(defpackage :aoc/2024/11 #.cl-user::*aoc-use*)
(in-package :aoc/2024/11)


(defun parse-input (&optional (strings (uiop:read-file-lines #P"src/2024/day11.txt")))
  (extract-positive-integers (first strings)))
#+#:excluded (parse-input)

(defun change (stone)
  (cond ((= stone 0) (list 1))
        ((evenp (length (spr stone)))
         (let* ((s (spr stone))
                (left (parse-integer (subseq s 0 (/ (length s) 2))))
                (right (parse-integer (subseq s (/ (length s) 2)))))
           (list left right)))
        (t (list (* stone 2024)))))


(defun blink (times stones)
  (let1 curr (make-counter stones)
    (repeat times
      (setf curr
            (prog1-let next (make-counter nil)
              (dohash (stone n curr)
                (dolist (stone1 (change stone))
                  (incf (gethash stone1 next 0) n))))))
    (looping (dohashv (n curr) (sum! n)))))


(define-solution (2024 11) (stones parse-input)
  (values
    (blink 25 stones)
    (blink 75 stones)))

(define-test (2024 11) (231278 274229228071551))
