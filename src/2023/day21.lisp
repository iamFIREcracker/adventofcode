(defpackage :aoc/2023/21 #.cl-user::*aoc-use*)
(in-package :aoc/2023/21)


(defparameter *north* '(-1 0))
(defparameter *east*  '(0 1))
(defparameter *south* '(1 0))
(defparameter *west*  '(0 -1))


(defun parse-map (&optional (strings (aoc::read-problem-input 2023 21)))
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


(defun recenter (pos rows cols)
  (destructuring-bind (r c) pos
    (multiple-value-bind (tr r) (floor r rows)
      (multiple-value-bind (tc c) (floor c cols)
        (list (list r c) (list tr tc))))))

(defun distances (max-tiles-away &optional (input (parse-map)))
  (destructuring-bind (map (sr sc) rows cols) input
    (bnd* ((q (make-queue))
           (seen-at (make-hash-table :test 'equal)))
      (enqueue (list sr sc 0 0 0) q)
      (while-not (queue-empty-p q)
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


(defun reachable? (step max-steps)
  (and (<= step max-steps) (evenp (- max-steps step))))


(defun count-all-reachable? (max-steps &optional (input (parse-map)))
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


(define-solution (2023 21) (input parse-map)
  (values (count-if [reachable? _ 64] (hash-table-values (distances 0 input)))
          (count-all-reachable? 26501365)))

(define-test (2023 21) (3746 623540829615589))
