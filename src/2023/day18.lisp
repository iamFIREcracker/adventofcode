(defpackage :aoc/2023/18 #.cl-user::*aoc-use*)
(in-package :aoc/2023/18)


(defparameter *north* '(-1 0))
(defparameter *east*  '(0 1))
(defparameter *south* '(1 0))
(defparameter *west*  '(0 -1))

(defun move-straight (pos dir &optional (times 1))
  (mapcar #'+ pos (mapcar #'* dir (list times times))))


(defun dig-plan (&optional (strings (uiop:read-file-lines #P"src/2023/day18.txt")))
  (looping
    (dolist (s strings)
      (destructuring-bind (dir length color) (split-sequence:split-sequence #\Space s)
        (collect!
          (list
            (ecase (char dir 0)
              (#\R *east*)
              (#\U *north*)
              (#\D *south*)
              (#\L *west*))
            (parse-integer length)
            color))))))


;; In geometry, Pick's theorem provides a formula for the area of a simple
;; polygon with integer vertex coordinates, in terms of the number of integer
;; points within it and on its boundary:
;;
;;   A = i + b/2 - 1
;;
;; Turns out, i + b is the answer to our problem, i.e., the area of the pit is
;; equal to the number of integer points (lattice) on the edge plus the number
;; of points within the curve.
;;
;; Let's rewrite the above and isolate i + b on one side of the equation:
;;
;; A + b/2     = i + b - 1
;; A + b/2 + 1 = i + b
;;
;; And that's it: A can be calculated using the Shoelace formula, while b
;; is known, i.e., it's the perimeter of the pit.
;;
;; Read: https://www.reddit.com/r/adventofcode/comments/18l0qtr/2023_day_18_solutions/kdvrqv8/
(defun pit-area (&optional (plan (dig-plan)))
  ""
  (bnd1 points (list `(0 0))
    (doseq ((dir length) plan)
      (bnd* ((npos (move-straight (first points) dir length)))
        (push npos points)))
    ;; Close the polygon
    (setf points (cons (last-elt points) points))
    (+ (area points) (/ (perimeter points) 2) 1)))

(defun area (polygon)
  "Calculates the area of `polygon` using Shoelace's traingles formula."
  (flet ((shoelace (p1 p2)
           (destructuring-bind (y1 x1) p1
             (destructuring-bind (y2 x2) p2
               (- (* x1 y2) (* x2 y1))))))
    (/ (abs
         (looping
           (dolists ((p1 polygon)
                     (p2 (cdr polygon)))
             (sum! (shoelace p1 p2)))))
       2)))

(defun perimeter (polygon)
  (looping
    (dolists ((p1 polygon)
              (p2 (cdr polygon)))
      (sum! (manhattan-distance p1 p2)))))


(defun revised-dig-plan (&optional (strings (uiop:read-file-lines #P"src/2023/day18.txt")))
  (looping
    (dolist (s strings)
      (bnd1 color (third (split-sequence:split-sequence #\Space s))
        (setf color (subseq color 2 (1- (length color))))
        (collect!
          (list
            (ecase (char color 5)
              (#\0 *east*)
              (#\3 *north*)
              (#\1 *south*)
              (#\2 *west*))
            (parse-integer (subseq color 0 5) :radix 16)
            "ignored"))))))


(define-solution (2023 18) (strings)
  (values (pit-area (dig-plan strings))
          (pit-area (revised-dig-plan strings))))

(define-test (2023 18) (50603 96556251590677))
