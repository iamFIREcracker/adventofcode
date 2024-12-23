(defpackage :aoc/2024/16 #.cl-user::*aoc-use*)
(in-package :aoc/2024/16)

(defun parse-input (&optional (strings (uiop:read-file-lines #P"src/2024/day16.txt")))
  (let ((map (make-hash-table :test 'equal)) (start) (end))
    (doseq ((i s) (enumerate strings))
      (doseq ((j ch) (enumerate s))
        (setf (gethash (list i j) map) ch)
        (if (char= ch #\S) (setf start (list (list i j) (list 0 1))))
        (if (char= ch #\E) (setf end (list i j)))))
    (list map start end)))
#+#:excluded (parse-input)


(defun move-straight (dir pos) (mapcar #'+ pos dir))
(defun rotate-cw (dir) (list (second dir) (- (first dir))))
(defun rotate-ccw (dir) (list (- (second dir)) (first dir)))


;; Modified Dijkstra returning all the paths leading
;; of all the optimal solutions
(defun minimize-cost (&optional (input (parse-input)))
  (destructuring-bind (map (pos dir) end) input
    (let (best bestpaths)
      (let ((hq (make-hq :key #'car))
            (seen (make-hash-table :test 'equal)))
        (hq-insert hq (list 0 pos dir nil))
        (until (hq-empty-p hq)
          (destructuring-bind (cost pos dir path) (hq-pop hq)
            (when (equal pos end)
              (if (null best)
                  (setf best cost))
              (when (= cost best)
                (push (cons pos path) bestpaths)))
            (when (<= cost (gethash (list pos dir) seen most-positive-fixnum))
              (setf (gethash (list pos dir) seen) cost)
              (let1 next (move-straight pos dir)
                (unless (char= (gethash next map) #\#)
                  (hq-insert hq (list (+ cost 1) next dir (cons pos path)))))
              (hq-insert hq (list (+ cost 1000) pos (rotate-cw dir) path))
              (hq-insert hq (list (+ cost 1000) pos (rotate-ccw dir) path))))))
      (values best bestpaths))))


(define-solution (2024 16) (input parse-input)
  (multiple-value-bind (cost paths) (minimize-cost input)
    (values cost
            (length (looping
                      (dolist (path paths)
                        (doseq (tile path)
                          (adjoin! tile))))))))

(define-test (2024 16) (135536 583))
