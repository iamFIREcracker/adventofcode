(defpackage :aoc/2024/18 #.cl-user::*aoc-use*)
(in-package :aoc/2024/18)

#;
(sb-ext:gc :full t)

(defvar *side* 70)

(defun parse-input (&optional (strings (uiop:read-file-lines #P"src/2024/day18.txt"))
                    (n 1024))
  (prog1-let (grid (make-hash-table :test 'equal))
    (dotimes (i (1+ *side*))
      (dotimes (j (1+ *side*))
        (setf (gethash (list i j) grid) #\.)))
    (dolist (s (take n strings))
      (destructuring-bind (j i) (extract-positive-integers s)
        (setf (gethash (list i j) grid) #\#)))))
#+#:excluded (parse-input)

(let1 grid (parse-input)
  (let ((start (list 0 0))
        (end (list *side* *side*)))
    (search-cost (a* (list 0 0) :goal-state end :test 'equal
                     :neighbors (fn (pos)
                                  (destructuring-bind (i j) pos
                                    (looping
                                      (doseq ((di dj) '((-1 0) (0 1) (1 0) (0 -1)))
                                        (let1 pos1 (list (+ i di) (+ j dj))
                                          (when (aand (gethash pos1 grid) (char= it #\.))
                                            (collect! (cons pos1 1))))))))
                     :heuristic [manhattan-distance _ end]))))

(doseq ((i s) (enumerate (uiop:read-file-lines #P"src/2024/day18.txt")))
  (let1 grid (parse-input (uiop:read-file-lines #P"src/2024/day18.txt") (1+ i))
    (let ((start (list 0 0))
          (end (list *side* *side*)))
      (unless (a* (list 0 0) :goal-state end :test 'equal
                       :neighbors (fn (pos)
                                    (destructuring-bind (i j) pos
                                      (looping
                                        (doseq ((di dj) '((-1 0) (0 1) (1 0) (0 -1)))
                                          (let1 pos1 (list (+ i di) (+ j dj))
                                            (when (aand (gethash pos1 grid) (char= it #\.))
                                              (collect! (cons pos1 1))))))))
                       :heuristic [manhattan-distance _ end])
        (dbgl s)
        (break)))))
