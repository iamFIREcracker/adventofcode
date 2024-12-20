(defpackage :aoc/2024/20 #.cl-user::*aoc-use*)
(in-package :aoc/2024/20)

(defun parse-input (&optional (strings (uiop:read-file-lines #P"src/2024/day20.txt")))
  (let ((map (make-hash-table :test 'equal))
        start end)
    (doeseq (i s strings)
      (doeseq (j ch s)
        (setf (gethash (list i j) map) ch)
        (if (char= ch #\S)
            (setf start (list i j))
            (if (char= ch #\E)
                (setf end (list i j))))))
    (list map start end)))
#+#:excluded (parse-input)

(defun paths-to-end (&optional (input (parse-input)))
  (destructuring-bind (map start end) input
    (prog1-let (all-paths (make-hash-table :test 'equal))
      (recursively ((pos start)
                    prev)
        (setf (gethash pos all-paths)
              (cond ((equal pos end) nil)
                    (t (destructuring-bind (i j) pos
                         (doseq ((di dj) '((-1 0) (0 1) (1 0) (0 -1)))
                           (let1 next (list (+ i di) (+ j dj))
                             (when (not (equal next prev))
                               (when (aand (gethash next map) (char/= it #\#))
                                 (return (cons next (recur next pos)))))))))))))))
#+#:excluded (paths-to-end)

#+#:excluded (defun cheat-cuts (&optional (input (parse-input)))
               (let1 all-paths (paths-to-end input)
                 (looping
                   (dohash ((i j) path all-paths)
                     (doseq ((di dj) '((-1 0) (0 1) (1 0) (0 -1)))
                       (let ((i1 (+ i di)) (j1 (+ j dj)))
                         ;; Check if we are going off the beaten path -- i.e., into a wall
                         (when (not (hash-table-key-exists-p all-paths (list i1 j1)))
                           (doseq ((di1 dj1) '((-1 0) (0 1) (1 0) (0 -1)))
                             (let ((i2 (+ i1 di1)) (j2 (+ j1 dj1)))
                               (when (or (/= i i2) (/= j j2))
                                 ;; Check if we are back on the beaten track
                                 (when (hash-table-key-exists-p all-paths (list i2 j2))
                                   (let1 new-path (list* (list i1 j1) (list i2 j2) (gethash (list i2 j2) all-paths))
                                     (if (< (length new-path) (length path))
                                         (collect! (- (length path) (length new-path))))))))))))))))
#+#:excluded (~> (cheat-cuts) frequencies (sort ~ #'< :key 'car))
#+#:excluded (count-if [>= _ 100] (cheat-cuts))

(defun cheat-cuts (&optional (max-cut-distance 2) (input (parse-input)))
  (let1 all-paths (paths-to-end input)
    (looping
      (dohash (pos path all-paths)
        (dolist (pos1 path)
          (let1 distance (manhattan-distance pos1 pos)
            (when (<= distance max-cut-distance)
              (let1 new-path-len (+ distance (length (gethash pos1 all-paths)))
                (if (< new-path-len (length path))
                    (collect! (- (length path) new-path-len)))))))))))
#;
(~> (cheat-cuts 2) frequencies (sort ~ #'< :key 'car))
(count-if [>= _ 100] (cheat-cuts 2))
(count-if [>= _ 100] (cheat-cuts 20))
985332


(define-solution (2024 20) (input parse-input)
  (destructuring-bind (patterns designs) input
    (values (count-if #'plusp designs :key [design-possible? patterns _])
            (reduce #'+ designs :key [design-possible? patterns _]))))

(define-test (2024 20) (350 769668867512623))
