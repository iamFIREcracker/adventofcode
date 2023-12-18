(defpackage :aoc/2023/18 #.cl-user::*aoc-use*)
(in-package :aoc/2023/18)


(defparameter *north* '(-1 0))
(defparameter *east*  '(0 1))
(defparameter *south* '(1 0))
(defparameter *west*  '(0 -1))

(defun move-straight (pos dir) (mapcar #'+ pos dir))
(defun rotate-cw (dir) (list (second dir) (- (first dir))))
(defun rotate-ccw (dir) (list (- (second dir)) (first dir)))


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

(defun dig-inside (map)
  (bnd* ((min-row (reduce #'min (hash-table-keys map) :key #'car)))
    (labels ((dfs (pos)
               ; (break)
               ; (pr pos)
               )
             (find-one-inside ()
               (bnd* ((i (1+ min-row))
                      (edge? nil) (inside? nil)
                      (min-col (reduce #'min (hash-table-keys map) :key #'cadr))
                      (max-col (reduce #'max (hash-table-keys map) :key #'cadr)))
                 (dorangei (j min-col max-col)
                   #+#:excluded (pr i j edge? inside?)
                   (cond ((and (not edge?) (not inside?) (gethash (list i j) map))
                          (setf edge? t))
                         ((and edge? (not (gethash (list i j) map)))
                          (return-from find-one-inside (list i j)))))
                 )))
      (bnd1 (frontier (list (find-one-inside)))
        (while frontier
          (bnd1 (pos (pop frontier))
            (unless (gethash pos map)
              (setf (gethash pos map) #\# )
              (dolist (dir (list *north* *east* *south* *west*))
                (push (move-straight pos dir) frontier)))))))

    #+#:excluded (dorangei (i min-row max-row)
                   (bnd* ((edge? nil) (inside? nil))
                     (dorangei (j min-col max-col)
                       (cond ((and (not edge?) (not inside?) (gethash (list i j) map))
                              (setf edge? t))
                             ((and edge? (not (gethash (list i j) map)))
                              (setf edge? nil inside? t (gethash (list i j) map) #\# ))
                             ((and inside? (not (gethash (list i j) map)))
                              (setf (gethash (list i j) map) #\# ))
                             ((and inside? (gethash (list i j) map))
                              (return))))
                     ))))
(defun dig (&optional (plan (dig-plan)))
  (bnd* ((pos (list 0 0))
         (map (make-hash-table :test 'equal)))
    (setf (gethash pos map) #\# )
    (dolist+ ((dir length _) plan)
      (repeat length
        (setf pos (move-straight pos dir))
        (setf (gethash pos map) #\# )))
    (print-map map)
    (dig-inside map)
    (format t "----------------------~%")
    (print-map map)
    map))

(defun print-map (map)
  (bnd* ((min-row (reduce #'min (hash-table-keys map) :key #'car))
         (max-row (reduce #'max (hash-table-keys map) :key #'car))
         (min-col (reduce #'min (hash-table-keys map) :key #'cadr))
         (max-col (reduce #'max (hash-table-keys map) :key #'cadr))
         )
    (dorangei (i min-row max-row)
      (dorangei (j min-col max-col)
        (princ (or (gethash (list i j) map) #\.)))
      (terpri))))
(dig)
(hash-table-keys *)
(length *)

; 53931 nope
; 60566 nope
; 51280 nope
; 24660 nope
; 50603 !!!

#x70c71
(defun revised-dig-plan (&optional (strings (uiop:read-file-lines #P"src/2023/day18.txt")))
  (looping
    (dolist (s strings)
      (bnd1 (color (third (split-sequence:split-sequence #\Space s)))
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
#+#:excluded (revised-dig-plan)
(defun fake-dig-plan ()
  `((,*east* 10)
     (,*south* 10)
     (,*west* 10)
     (,*north* 10)))
#+#:excluded (fake-dig-plan)
(defun fake-dig-plan2 ()
  `((,*east* 5)
     (,*north* 1)
     (,*east* 2)
     (,*south* 1)
     (,*east* 3)
     (,*south* 10)
     (,*west* 10)
     (,*north* 10)))
#+#:excluded (fake-dig-plan2)
#|
>>> def area_by_shoelace(x, y):
    "Assumes x,y points go around the polygon in one direction"
    return abs( sum(i * j for i, j in zip(x,             y[1:] + y[:1]))
               -sum(i * j for i, j in zip(x[1:] + x[:1], y            ))) / 2
|#

(defun rotate-one-left (x)
  (concatenate 'list (subseq x 1) (subseq x 0 1)))
; (rotate-one-left (list 1 2 3))

(defun area (points)
  (bnd* ((xs (mapcar #'cadr points))
         (ys (mapcar #'car points)))
    (/ (abs
         (looping
           (dolists ((i xs)
                     (j (rotate-one-left ys)))
             (sum! (* i j)))
           (dolists ((i (rotate-one-left xs))
                     (j ys))
             (sum! (- (* i j))))))
       2)))

;; Shoelace triangle formula
(defun area (points)
  (flet ((shoelace (p1 p2)
           (destructuring-bind (y1 x1) p1
             (destructuring-bind (y2 x2) p2
               (- (* x1 y2) (* x2 y1))))))
    (bnd1 (points (concatenate 'list points (list (first points))))
      (/ (abs
           (looping
             (dosublists ((p1 p2) points)
               (when p2
                 (sum! (shoelace p1 p2))))))
         2))))
#+#:excluded (area `((4 3) (11 5) (8 12) (5 9) (6 5)))
#+#:excluded (area `((4 3) (11 5) (8 12) (5 9) (6 5)))

(defun dig2 (&optional (plan (revised-dig-plan)))
  (bnd* ((pos (list 0 0))
         points
         directions)
    (dolist+ ((dir length _) plan)
      #+#:excluded (prl 'start dir length pos)
      (when directions
        (cond ((equal (rotate-ccw (car directions)) dir)
               (setf pos (move-straight pos (rotate-cw (rotate-cw (car directions))))
                     (first points) pos
                     length (1- length)))))
      #+#:excluded (prl 'post-adj dir length pos)
      (bnd* ((next (move-straight pos (mapcar #'* dir (list (1+ length) (1+ length))))))
        (push next points)
        (push dir directions)
        (setf pos next)
        #+#:excluded (prl 'end dir length next))
      #+#:excluded (break))
    (bnd1 (dir (first (first plan)))
      (cond ((equal (rotate-ccw (car directions)) dir)
             (setf pos (move-straight pos (rotate-cw (rotate-cw (car directions))))
                   (first points) pos))))
    (pr (reverse points))
    (values (area points)
            (area (reverse points)))))



; (dig2 (fake-dig-plan))
; (dig (fake-dig-plan2))
#+#:excluded (dig2 (dig-plan))
#+#:excluded (dig2)
