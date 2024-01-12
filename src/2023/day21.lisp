(defpackage :aoc/2023/21 #.cl-user::*aoc-use*)
(in-package :aoc/2023/21)


(defparameter *north* '(-1 0))
(defparameter *east*  '(0 1))
(defparameter *south* '(1 0))
(defparameter *west*  '(0 -1))

(defun move-straight (pos dir &optional (times 1))
  (mapcar #'+ pos (mapcar [* times _] dir)))


(defun parse-map (&optional (strings (uiop:read-file-lines #P"src/2023/day21.txt")))
  (bnd* ((rows (length strings))
         (cols (length (first strings)))
         (map (make-hash-table :test 'equal))
         (start))
    (doseq ((i s) (enumerate strings))
      (doseq ((j ch) (enumerate s))
        (when (find ch ".S")
          (setf (gethash (list i j) map) ch))
        (when (char= ch #\S)
          (setf start (list i j)))))
    (list map start rows cols)))
#+#:excluded (parse-map)

(defun part1 (&optional (input (parse-map)))
  (destructuring-bind (map start rows cols) input
    (bnd* ((q (list start)))
      (repeat 64
        (setf q (remove-duplicates
                  (looping
                    (dolist (pos q)
                      (dolist (dir (list *north* *east* *south* *west*))
                        (bnd* ((npos (move-straight pos dir)))
                          (when (gethash npos map)
                            (collect! npos))))))
                  :test #'equal)))
      (length q))))
#+#:excluded (part1)
; 3746

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun parse-map (&optional (strings (uiop:read-file-lines #P"src/2023/day21.txt")))
  (bnd* ((rows (length strings))
         (cols (length (first strings)))
         (map (make-hash-table :test 'equal))
         (start))
    (doseq ((i s) (enumerate strings))
      (doseq ((j ch) (enumerate s))
        (setf (gethash (list i j) map) ch)
        (when (char= ch #\S)
          (setf start (list i j)))))
    (list map start rows cols)))
#+#:excluded (parse-map)

(defun recenter (pos rows cols)
  (destructuring-bind (r c) pos
    (multiple-value-bind (tr r) (floor r rows)
      (multiple-value-bind (tc c) (floor c cols)
        (list (list r c) (list tr tc))))))
#+#:excluded (recenter '(-1 65) 131 131)
#+#:excluded (recenter '(-132 65) 131 131)
#+#:excluded (recenter '(-263 65) 131 131)
#+#:excluded (recenter '(-263 131) 131 131)

(defun distances (max-tiles-away &optional (input (parse-map)))
  (destructuring-bind (map (sr sc) rows cols) input
    (bnd* ((q (make-queue))
           (seen-at (make-hash-table :test 'equal)))
      (enqueue (list sr sc 0 0 0) q)
      (while (not (queue-empty-p q))
        (bnd* (((r c tr tc step) (dequeue q))
               (ch (gethash (list r c) map)))
          (cond ((gethash (list r c tr tc) seen-at) nil)
                ((or (> (abs tr) max-tiles-away)
                     (> (abs tc) max-tiles-away))
                 nil)
                ((char= ch #\#) nil)
                (t (setf (gethash (list r c tr tc) seen-at) step)
                   (doseq ((dr dc) (list *north* *east* *south* *west*))
                     (bnd* ((nr (+ r dr)) (nc (+ c dc))
                            (ntr tr) (ntc tc))
                       (cond ((< nr 0) (setf nr (+ nr rows)
                                             ntr (- ntr 1)))
                             ((>= nr rows) (setf nr (- nr rows)
                                                 ntr (+ ntr 1)))
                             ((< nc 0) (setf nc (+ nc cols)
                                             ntc (- ntc 1)))
                             ((>= nc cols) (setf nc (- nc cols)
                                                 ntc (+ ntc 1))))
                       (enqueue (list nr nc ntr ntc (1+ step)) q)))))))
      seen-at)))
#+#:excluded (time (distances 0))

(defun reachable? (step &optional (max-steps 26501365))
  (evenp (- max-steps step)))

#+#:excluded (defun more-experiments (max-steps &optional (input (parse-map)))
               (bnd1 (ff (flood-fill 500 input)) ; TODO arbitrary number
                 (looping
                   (destructuring-bind (map start rows cols) input
                     (dohash (pos ch map)
                       (when (and (char/= ch #\#)
                                  ; 4 108 is surrounded by walls...
                                  (gethash pos ff))
                         (bnd* ((start-to-pos (- (gethash pos ff) (gethash start ff))))
                           (bnd* ((start-to-north (- (gethash (move-straight pos *north* rows) ff)
                                                     (gethash start ff)))
                                  (pos-to-north (- (gethash (move-straight pos *north* (* 2 rows)) ff)
                                                   (gethash (move-straight pos *north* rows) ff)))
                                  (start-to-east (- (gethash (move-straight pos *east* cols) ff)
                                                    (gethash start ff)))
                                  (pos-to-east (- (gethash (move-straight pos *east* (* 2 cols)) ff)
                                                  (gethash (move-straight pos *east* cols) ff)))
                                  (start-to-south (- (gethash (move-straight pos *south* rows) ff)
                                                     (gethash start ff)))
                                  (pos-to-south (- (gethash (move-straight pos *south* (* 2 rows)) ff)
                                                   (gethash (move-straight pos *south* rows) ff)))
                                  (start-to-west (- (gethash (move-straight pos *west* cols) ff)
                                                    (gethash start ff)))
                                  (pos-to-west (- (gethash (move-straight pos *west* (* 2 cols)) ff)
                                                  (gethash (move-straight pos *west* cols) ff))))
                             ;; middle tile
                             (count! (reachable? start-to-pos max-steps))
                             ; (break)

                             ;; go up
                             (bnd* ((step start-to-north)
                                    (tiles 0))
                               (while (< step max-steps)
                                 (incf tiles)
                                 (count! (reachable? step max-steps))
                                 (incf step pos-to-north)
                                 #+#:excluded (break)
                                 )
                               (pr 'up start-to-pos pos-to-north tiles)
                               (break)
                               )
                             #+#:excluded (break)

                             ;; TODO diagonal

                             ;; go right
                             (bnd* ((step start-to-east)
                                    (tiles 0))
                               (while (< step max-steps)
                                 (incf tiles)
                                 (count! (reachable? step max-steps))
                                 (incf step pos-to-east)
                                 #+#:excluded (break))
                               (pr 'right start-to-pos pos-to-east tiles)
                               (break)
                               )

                             ;; TODO diagonal

                             ;; go down
                             (bnd* ((step start-to-south)
                                    (tiles 0))
                               (while (< step max-steps)
                                 (incf tiles)
                                 (count! (reachable? step max-steps))
                                 (incf step pos-to-south)
                                 )
                               (pr 'down start-to-pos pos-to-south tiles)
                               (break)
                               )

                             ;; TOOD diagonal

                             ;; go left
                             (bnd* ((step start-to-west)
                                    (tiles 0))
                               (while (< step max-steps)
                                 (incf tiles)
                                 (count! (reachable? step max-steps))
                                 (incf step pos-to-west)
                                 #+#:excluded (break))
                               (pr 'left start-to-pos pos-to-west tiles)
                               (break)
                               )
                             #+#:excluded (break)
                             ))))))))

(defmacro memoizing ((ht &rest key-parts) &body body)
  (with-gensyms (memo key)
    `(let ((,memo ,ht)
           (,key (list ,@key-parts)))
       (multiple-value-bind (res res?) (gethash ,key ,memo)
         (if res?
           res
           (setf (gethash ,key ,memo)
                 (block memo
                        ,@body)))))))

(defun part2 (max-steps &optional (input (parse-map)))
  (destructuring-bind (rows cols) (cddr input)
    (assert (= rows cols))
    (bnd* ((size rows)
           (memo (make-hash-table :test 'equal)))
      (labels ((edge-tile? (tr tc) (xor (= (abs tr) 3) (= (abs tc) 3)))
               (corner-tile? (tr tc) (and (= (abs tr) 3) (= (abs tc) 3)))
               ;; CEEEEEC
               ;; E     E
               ;; E     E1234567
               ;; E     EXYXYXYX
               ;; E     E
               ;; E     E
               ;; CEEEEEC
               (count-copies-on-straight-line (step)
                 (memoizing (memo :straight step)
                   (bnd1 (tiles (ceiling (- max-steps step) size))
                     (looping
                       (dorangei (x 1 tiles)
                         (bnd1 (nstep (+ step (* x size)))
                           (count! (and (<= nstep max-steps)
                                        (reachable? nstep max-steps)))))))))
               ;; CEEEEEC
               ;; E     E
               ;; E     E
               ;; E     E
               ;; E     E
               ;; E     E
               ;; CEEEEECXYXYXYX
               ;;       XYXYXYX
               ;;       YXYXYX
               ;;       XYXYX
               ;;       YXYX
               ;;       XYX
               ;;       YX
               ;;       X
               (count-copies-on-quadrant (step)
                 (memoizing (memo :quadrant step)
                   (bnd1 (tiles (ceiling (- max-steps step) size))
                     (looping
                       (dorangei (x 1 tiles)
                         (bnd1 (nstep (+ step (* x size)))
                           (when (and (<= nstep max-steps)
                                      (reachable? nstep max-steps))
                             ;; CEEEEEC
                             ;; E     E
                             ;; E     E
                             ;; E     E
                             ;; E     E
                             ;; E     E
                             ;; CEEEEEC1234567
                             ;;       1234567
                             ;;       234567
                             ;;       34567
                             ;;       4567
                             ;;       567
                             ;;       67
                             ;;       7
                             (sum! (1+ x))))))))))
        (looping
          (dohash ((r c tr tc) step (distances 3 input))
            (when (reachable? step max-steps)
              (sum! 1))
            (if (edge-tile? tr tc)
              (sum! (count-copies-on-straight-line step))
              (if (corner-tile? tr tc)
                (sum! (count-copies-on-quadrant step))))))))))
#+#:excluded (time (part2 5000))
#+#:excluded (time (part2 26501365))
623540829615589
