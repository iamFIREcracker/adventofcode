(defpackage :aoc/2023/10 #.cl-user::*aoc-use*)
(in-package :aoc/2023/10)


(defparameter *north* '(-1 0))
(defparameter *east*  '(0 1))
(defparameter *south* '(1 0))
(defparameter *west*  '(0 -1))

(defun move-straight (pos dir) (mapcar #'+ pos dir))
(defun rotate-cw (dir) (list (second dir) (- (first dir))))
(defun rotate-ccw (dir) (list (- (second dir)) (first dir)))

(defun parse-loop (&optional (strings (uiop:read-file-lines #P"src/2023/day10.txt")))
  (bnd* ((map (make-hash-table :test 'equal))
         start
         dirs)
    ;; Load input into a hash-table, and record where the start is
    (dolist+ ((i s) (enumerate strings))
      (dolist+ ((j ch) (enumerate s))
        (setf (gethash (list i j) map) ch)
        (if (char= ch #\S)
          (setf start (list i j)))))
    ;; Figure out what the possible starting directions might be
    (dolist+ ((dir next) `((,*east* "-7J") (,*south* "|LJ") (,*west* "-LF") (,*north* "|7F")))
      (if (find (gethash (move-straight start dir) map) next)
        (push dir dirs)))
    ;; Replace S with the right pipe connector
    (destructuring-bind (dir1 dir2) dirs
      (setf (gethash start map)
            (cond ((equal dir1 *north*) (cond ((equal dir2 *east*) #\L)
                                              ((equal dir2 *south*) #\|)
                                              ((equal dir2 *west*) #\J)))
                  ((equal dir1 *east*)  (cond ((equal dir2 *south*) #\F)
                                              ((equal dir2 *west*) #\-)
                                              ((equal dir2 *north*) #\L)))
                  ((equal dir1 *south*) (cond ((equal dir2 *west*) #\7)
                                              ((equal dir2 *north*) #\|)
                                              ((equal dir2 *east*) #\F)))
                  ((equal dir1 *west*)  (cond ((equal dir2 *north*) #\J)
                                              ((equal dir2 *east*) #\-)
                                              ((equal dir2 *south*) #\7))))))
    (list start dirs map)))


(defun loop-steps (&optional (input (parse-loop)) (dir (first (second input)))
                             &aux
                             (start (first input))
                             (map (third input))
                             (pos start))
  (labels ((move (&aux (ch (gethash pos map)))
             (cond ((equal pos start) (setf pos (move-straight dir pos)))
                   ((find  ch "-|") (setf pos (move-straight dir pos)))
                   ((char= ch #\L) (setf dir (if (zerop (first dir))
                                               (rotate-cw dir)
                                               (rotate-ccw dir))
                                         pos (move-straight dir pos)))
                   ((char= ch #\J) (setf dir (if (zerop (first dir))
                                               (rotate-ccw dir)
                                               (rotate-cw dir))
                                         pos (move-straight dir pos)))
                   ((char= ch #\7) (setf dir (if (zerop (first dir))
                                               (rotate-cw dir)
                                               (rotate-ccw dir))
                                         pos (move-straight dir pos)))
                   ((char= ch #\F) (setf dir (if (zerop (first dir))
                                               (rotate-ccw dir)
                                               (rotate-cw dir))
                                         pos (move-straight dir pos)))
                   (t (error 'wtf)))))
    (looping
      (collect! (move))
      (while (not (equal pos start))
        (collect! (move))))))


(defun furthest-loop-step (&optional (input (parse-loop)))
  (bnd1 (steps (loop-steps input))
    (/ (length steps) 2)))


(defun enclosed-inside-loop? (loop i j)
  "The position `(list i j)` is enclosed inside the loop `loop`, if there
are a odd number of pipes separating the position and the edge of the map.

It does not matter which direction we choose (north, east, south, west);
this function will count number of crossing pipes west of the current position.

What makes a crossing pipe?

- A straight vertical pipe: |
- L / 7 pairs, and J / F pairs

What does not constitue a crossing pipe?

- L / J pairs, and 7 / F pairs

Horintal pipes do not move the needle, so they are skipped.
"
  (oddp
    (looping
      (dorange (jj 0 j)
        (awhen (gethash (list i jj) loop)
          (sum! (ecase it
                  (#\| 1)
                  (#\L 1/2)
                  (#\J -1/2)
                  (#\7 1/2)
                  (#\F -1/2)
                  (#\- 0))))))))

(defun count-enclosed-inside-loop (&optional (input (parse-loop))
                                             &aux
                                             (map (third input)))
  (bnd* ((loop (make-hash-table :test 'equal))
         (rows (reduce #'max (hash-table-keys map) :key #'first))
         (cols (reduce #'max (hash-table-keys map) :key #'second)))
    (dolist (pos (loop-steps input))
      (setf (gethash pos loop) (gethash pos map)))
    (looping
      (dorange (i 0 rows)
        (dorange (j 0 cols)
          (bnd1 (on-loop? (gethash (list i j) loop))
            (count! (and (not on-loop?) (enclosed-inside-loop? loop i j)))))))))


(define-solution (2023 10) (input parse-loop)
  (values (furthest-loop-step input)
          (count-enclosed-inside-loop input)))

(define-test (2023 10) (6882 491))
