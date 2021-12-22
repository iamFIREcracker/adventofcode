(defpackage :aoc/2021/19 #.cl-user::*aoc-use*)
(in-package :aoc/2021/19)


;; Input
(defun parse-scanner (paragraph &aux (paragraph (cl-ppcre:split "\\n" paragraph)))
  (flet ((numbers (string)
           (mapcar #'parse-integer (cl-ppcre:all-matches-as-strings "-?\\d+" string))))
    (cons
      (first (numbers (first paragraph)))
      (mapcar #'numbers (rest paragraph)))))
(defun id (scn) (car scn))
(defun beacons (scn) (cdr scn))

;;; Matrices
(defun row (i m) (nth i m))
(defun col (j m) (loop for row in m collect (nth j row)))
(defun dot-product (v1 v2) (apply #'+ (mapcar #'* v1 v2)))
(defun invert (m) (transpose m))
(defun transpose (m) (loop for i below (length m) collect (col i m)))

; Coutertesy of: https://www.euclideanspace.com/maths/algebra/matrix/orthogonal/rotation/index.htm
(defparameter *rotate-90-x*
  '((1 0  0)
    (0 0 -1)
    (0 1  0)))
(defparameter *rotate-90-y*
  '(( 0 0 1)
    ( 0 1 0)
    (-1 0 0)))
(defparameter *rotate-90-z*
  '((0 -1 0)
    (1  0 0)
    (0  0 1)))

(defun m* (m1 m2)
  (loop for i below (length m1) collect
        (loop for j below (length (first m1)) collect
              (dot-product (row i m1) (col j m2)))))

(defun compute-all-rotation-matrices (&aux
                                       (midentity '((1 0 0) (0 1 0) (0 0 1)))
                                       rez)
  (loop repeat 4
        for rz = midentity then (m* rz *rotate-90-z*) do
        (loop repeat 4
              for ry = (m* rz midentity) then (m* ry *rotate-90-y*) do
              (loop repeat 4
                    for rx = (m* ry midentity) then (m* rx *rotate-90-x*) do
                    (push rx rez))))
  (reverse (remove-duplicates rez :test 'equal)))

(defparameter *all-rotation-natrices* (compute-all-rotation-matrices))

;; Part 1
(defun find-beacons (scanners &aux
                       (queue (list (list (first scanners) '(0 0 0) '((1 0 0) (0 1 0) (0 0 1)))))
                       (rez (beacons (first scanners)))
                       (scanners (rest scanners)))
  (loop while queue
        for (scn1 d10 r10) = (pop queue) do
        (loop for scn2 in scanners do
              (multiple-value-bind (p1 r21) (locate-scanner scn1 scn2)
                (when p1
                  (flet ((transform21 (p2) (v+ (rotate p2 r21) p1))
                         (transform10 (p1) (v+ (rotate p1 r10) d10)))
                    (let ((d20 (transform10 p1))
                          (r20 (m* r10 r21)))
                      (prl (id scn1) d10 r10 (id scn2) p1 r21 d20 r20)
                      (setf rez (append rez (mapcar (lambda (p2)
                                                      (transform10 (transform21 p2)))
                                                    (beacons scn2)))
                        scanners (remove scn2 scanners :test 'equal)
                        queue (cons (list scn2 d20 r20) queue))))))))
  (remove-duplicates rez :test 'equal))


(defun locate-scanner (scn1 scn2 &aux (bb1 (beacons scn1)))
  (dolist (b1 bb1)
    (let ((dd1 (relative-to b1 bb1)))
      (dolist (m *all-rotation-natrices*)
        (let ((bb2 (rotate-all (beacons scn2) m)))
          (dolist (b2 bb2)
            (let ((dd2 (relative-to b2 bb2)))
              (let ((common (intersection dd1 dd2 :test 'equal)))
                (when (>= (length common) 12)
                  (let ((b2-orig (rotate b2 (invert m))))
                    (return-from locate-scanner
                                 (values (locate b1 b2-orig m) m))))))))))))

(defun relative-to (b bb) (mapcar (lambda (o) (mapcar #'- o b)) bb))
(defun rotate-all (points matrix) (mapcar (partial-1 #'rotate _ matrix) points))
(defun rotate (point matrix)
  (loop for row in matrix collect (dot-product row point)))
(defun locate (b1 b2 m) (v- b1 (rotate b2 m)))


#+#:excluded (find-beacons (mapcar #'parse-scanner (cl-ppcre:split "\\n\\n" (uiop:read-file-string "src/2021/day19.txt"))))
#+#:excluded (setq beacons *)
#+#:excluded (length *)


;; Part 2
(defun find-scanners (scanners &aux
                       (queue (list (list (first scanners) '(0 0 0) '((1 0 0) (0 1 0) (0 0 1)))))
                       rez
                       (scanners (rest scanners)))
  (loop while queue
        for (scn1 d10 r10) = (pop queue) do
        (loop for scn2 in scanners do
              (multiple-value-bind (p1 r21) (locate-scanner scn1 scn2)
                (when p1
                  (flet ((transform21 (p2) (v+ (rotate p2 r21) p1))
                         (transform10 (p1) (v+ (rotate p1 r10) d10)))
                    (let ((d20 (transform10 p1))
                          (r20 (m* r10 r21)))
                      (prl (id scn1) d10 r10 (id scn2) p1 r21 d20 r20)
                      (setf rez (cons d20 rez)
                        scanners (remove scn2 scanners :test 'equal)
                        queue (cons (list scn2 d20 r20) queue))))))))
  rez)

#+#:excluded (find-scanners (mapcar #'parse-scanner (cl-ppcre:split "\\n\\n" (uiop:read-file-string "src/2021/day19.txt"))))
#+#:excluded (setq scanners *)
#+#:excluded (loop for (s1 . remaining) on scanners maximize
      (loop for s2 in remaining maximize (manhattan-distance s1 s2)))

