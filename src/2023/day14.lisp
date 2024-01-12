(defpackage :aoc/2023/14 #.cl-user::*aoc-use*)
(in-package :aoc/2023/14)


(defun parse-input (&optional (strings (uiop:read-file-lines #P"src/2023/day14.txt")))
  (bnd* ((rows (length strings))
         (cols (length (first strings)))
         (rocks (make-hash-table :test 'equal))
         (walls (make-hash-table :test 'equal)))
    (doseq ((i s) (enumerate strings))
      (doseq ((j ch) (enumerate s))
        (when (char= ch #\O)
          (setf (gethash (list i j) rocks) ch))
        (when (char= ch #\# )
          (setf (gethash (list i j) walls) ch))))
    (list rocks walls rows cols)))

(defun north-first? (pos1 pos2) (< (first pos1) (first pos2)))

(defun move (dir sort-predicate &optional (input (parse-input)))
  (destructuring-bind (rocks walls rows cols) input
    (dolist (pos (sort (hash-table-keys rocks) sort-predicate))
      (bnd1 (npos (mapcar #'+ pos dir))
        (while (not (or (gethash npos walls)
                        (gethash npos rocks)
                        (< (first npos) 0) (>= (first npos) rows)
                        (< (second npos) 0) (>= (second npos) cols)))
          (remhash pos rocks)
          (setf (gethash npos rocks) #\O
                pos npos
                npos (mapcar #'+ pos dir))))))
  input)


(defun north-load (input)
  (destructuring-bind (rocks _1 rows _2) input
    (declare (ignore _1 _2))
    (reduce #'+ (hash-table-keys rocks) :key [- rows (car _)])))


(defun part1 (&optional (input (parse-input)))
  (move (list -1 0) #'north-first? input)
  (north-load input))


(defun west-first? (pos1 pos2) (< (second pos1) (second pos2)))
(defun south-first? (pos1 pos2) (> (first pos1) (first pos2)))
(defun east-first? (pos1 pos2) (> (second pos1) (second pos2)))

(defun cycle (&optional (input (parse-input)))
  (move (list -1  0) #'north-first? input)
  (move (list  0 -1) #'west-first? input)
  (move (list  1  0) #'south-first? input)
  (move (list  0  1) #'east-first? input))

(defun part2 (&optional (input (parse-input)))
  (destructuring-bind (cycles-at cycle-size input)
      (floyd #'cycle input
             :copier (lambda (state)
                       (destructuring-bind (rocks walls rows cols) state
                         (list (copy-hash-table rocks)
                               walls
                               rows
                               cols)))
             :key (lambda (state)
                    (destructuring-bind (rocks walls rows cols) state
                      (looping
                        (dorange (i 0 rows)
                          (dorange (j 0 cols)
                            (when (gethash (list i j) rocks)
                              (collect! (list i j))))))))
             :test 'equalp)
    (bnd1 (rem-steps (rem (- 1000000000 cycles-at) cycle-size))
      (repeat rem-steps
        (cycle input))
      (north-load input))))

#+#:excluded (time (part2))
;; 112452
