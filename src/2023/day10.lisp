(defpackage :aoc/2023/10 #.cl-user::*aoc-use*)
(in-package :aoc/2023/10)


(defun move-straight (pos dir) (mapcar #'+ pos dir))

(defun parse-loop (&optional (strings (uiop:read-file-lines #P"src/2023/day10.txt")))
  (bnd* ((map (make-hash-table :test 'equal))
         start
         dirs)
    (dolist+ ((i s) (enumerate strings))
      (dolist+ ((j ch) (enumerate s))
        (setf (gethash (list i j) map) ch)
        (if (char= ch #\S)
          (setf start (list i j)))))
    (dolist+ ((dir s) '(((0 1) "-7J") ((1 0) "|LJ") ((0 -1) "-LF") ((-1 0) "|7F")))
      (if (find (gethash (move-straight start dir) map) s)
        (push dir dirs)))
    (destructuring-bind (dir1 dir2) dirs
      (setf (gethash start map)
            (cond ((equal dir1 '(0 1)) (cond ((equal dir2 '(1 0)) #\F)
                                             ((equal dir2 '(0 -1)) #\-)
                                             ((equal dir2 '(-1 0)) #\L)))
                  ((equal dir1 '(1 0)) (cond ((equal dir2 '(0 -1)) #\7)
                                             ((equal dir2 '(-1 0)) #\|)
                                             ((equal dir2 '(0 1)) #\F)))
                  ((equal dir1 '(0 -1)) (cond ((equal dir2 '(-1 0)) #\J)
                                              ((equal dir2 '(0 1)) #\-)
                                              ((equal dir2 '(1 0)) #\7)))
                  ((equal dir1 '(-1 0)) (cond ((equal dir2 '(0 1)) #\L)
                                              ((equal dir2 '(1 0)) #\|)
                                              ((equal dir2 '(0 -1)) #\J))))))
    (list start dirs map)))


(defun loop-steps (map start dir &aux (pos start) (steps 0))
  (declare (optimize (debug 3)))
  (labels ((walk-straight () (mapcar #'+ pos dir))
           (rotate-cw ()
             (list (second dir) (- (first dir))))
           (rotate-ccw ()
             (list (- (second dir)) (first dir)))
           (move (&aux (ch (gethash pos map)))
             (cond ((equal pos start) (setf pos (walk-straight)))
                   ((find  ch "-|")
                    ;; JFC...kept on using `pos` instead of `start`...lost
                    ;; too much time debugging this Note: not just here...
                    (setf pos (walk-straight)))
                   ((char= ch #\L) (setf dir (if (zerop (first dir))
                                               (rotate-cw)
                                               (rotate-ccw))
                                         pos (walk-straight)))
                   ((char= ch #\J) (setf dir (if (zerop (first dir))
                                               (rotate-ccw)
                                               (rotate-cw))
                                         pos (walk-straight)))
                   ((char= ch #\7) (setf dir (if (zerop (first dir))
                                               (rotate-cw)
                                               (rotate-ccw))
                                         pos (walk-straight)))
                   ((char= ch #\F) (setf dir (if (zerop (first dir))
                                               (rotate-ccw)
                                               (rotate-cw))
                                         pos (walk-straight)))
                   (t (error 'wtf)))))
    (looping
      (collect! (list (incf steps) (move)))
      (while (not (equal pos start))
        (collect! (list (incf steps) (move)))))))


(defun furthest (&optional (input (parse-loop))
                           &aux
                           (start (first input))
                           (dirs (second input))
                           (map (third input)))
  (loop for (steps1 pos1) in (sort (loop-steps map start (first dirs)) #'< :key #'first)
        for (steps2 pos2) in (sort (loop-steps map start (second dirs)) #'< :key #'first)
        when (equal pos1 pos2) return steps1))
#+#:excluded (furthest)
6882


(defun part2 (&optional (input (parse-loop))
                        &aux
                        (start (first input))
                        (dir (first (second input)))
                        (map (third input)))
  (bnd* ((loop-steps (mapcar #'second (loop-steps map start dir)))
         (rows (reduce #'max (hash-table-keys map) :key #'first))
         (cols (reduce #'max (hash-table-keys map) :key #'second))
         (on-the-left))
    (looping
      (dorange (i 0 rows)
        (setf on-the-left 0)
        (dorange (j 0 cols)
          (bnd* ((ch (gethash (list i j) map))
                 (on-loop? (find (list i j) loop-steps :test 'equal)))
            #+#:excluded (when (and (not on-loop?) (oddp on-the-left))
                           (prl (list i j) on-the-left)
                           (break))

            (count! (and (not on-loop?) (oddp on-the-left)))
            (when on-loop?
              (incf on-the-left (ecase ch
                                  (#\| 1)
                                  (#\F -1/2)
                                  (#\L 1/2)
                                  (#\J -1/2)
                                  (#\7 1/2)
                                  (#\- 0))))))))))
#+#:excluded (part2)

8235 ;too high
1323 ; too high
817 ; nope
849 ; nope
