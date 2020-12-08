(defpackage :aoc/2018/17 #.cl-user::*aoc-use*)
(in-package :aoc/2018/17)

(defun parse-clay-vein (string)
  (cl-ppcre:register-groups-bind (x-or-y (#'parse-integer n m o))
      ("(x|y)=(\\d+).*=(\\d+)..(\\d+)" string)
    (if (string= x-or-y "x")
        (loop with col = n for row from m upto o collect (complex n row))
        (loop with row = n for col from m upto o collect (complex col n)))))

(defun parse-map (data)
  (let ((map (make-hash-table)))
    (dolist (vein (mapcar #'parse-clay-vein data) map)
      (dolist (pos vein)
        (setf (gethash pos map) #\#)))))

(defun row-min-max (map)
  (loop for pos being the hash-keys of map
        minimize (imagpart pos) into min
        maximize (imagpart pos) into max
        finally (return (cons min max))))

(defun left (pos) (+ #c(-1 0) pos))
(defun right (pos) (+ #c(1 0) pos))
(defun down (pos) (+ #c(0 1) pos))

(defun should-be-still-water-p (map pos)
  (labels ((recur (next pos &aux (ch (gethash pos map)))
             (when ch
               (or (find ch "#~")
                   (and (char= ch #\|) (recur next (funcall next pos)))))))
    (and (find (gethash (down pos) map) "#~")
         (recur #'left pos)
         (recur #'right pos))))

(defun mark-row-as-still-water (map pos)
  (loop for cur = pos then (left cur) for ch = (gethash cur map)
        until (find ch "#~") do (setf (gethash cur map) #\~))
  (loop for cur = (right pos) then (right cur) for ch = (gethash cur map)
        until (find ch "#~") do (setf (gethash cur map) #\~)))

(defun spill-water (map)
  (destructuring-bind (row-min . row-max) (row-min-max map)
    (labels ((dfs (pos)
               (cond ((gethash pos map))
                     ((> (imagpart pos) row-max))
                     (t (setf (gethash pos map) #\|)
                        (unless (gethash (down pos) map)
                          (dfs (down pos))
                          (when (should-be-still-water-p map (down pos))
                            (mark-row-as-still-water map (down pos))))
                        (when (find (gethash (down pos) map) "#~")
                          (dfs (left pos))
                          (dfs (right pos)))))))
      (dfs (complex 500 row-min)))))

(define-solution (2018 17) (map parse-map)
  (spill-water map)
  (loop for ch being the hash-values of map
        count (char= ch #\|) into dripping
        count (char= ch #\~) into still
        finally (return (values (+ dripping still) still))))

(define-test (2018 17) (33052 27068))
