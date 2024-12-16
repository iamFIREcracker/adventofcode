(defpackage :aoc/2024/16 #.cl-user::*aoc-use*)
(in-package :aoc/2024/16)

#;
(sb-ext:gc :full t)

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

(defun solve (&optional (input (parse-input)))
  (destructuring-bind (map start end) input
    (search-cost (a* start
                     :goalp [equal (car _) end]
                     :test 'equal :neighbors (fn (curr)
                                               (destructuring-bind (pos dir) curr
                                                 (looping
                                                   (let1 next (move-straight pos dir)
                                                     (unless (char= (gethash next map) #\#)
                                                       (collect! (cons (list next dir) 1))))
                                                   (collect! (cons (list pos (rotate-cw dir)) 1000))
                                                   (collect! (cons (list pos (rotate-ccw dir)) 1000)) )))
                     :heuristic [manhattan-distance (car _) end]))))
(solve)

(defun solve2 (&optional (input (parse-input)))
  (let1 best (solve input)
    (destructuring-bind (map start end) input
      (looping
        (a* start
            :goalp (fn (curr) nil)
            :goalp1 (fn (curr cost path)
                      (when (and (equal (car curr) end) (= cost best))
                        (dolist (tile path)
                          (adjoin! (car tile) :test 'equal)))
                      nil)
            :test 'equal :neighbors (fn (curr)
                                      (destructuring-bind (pos dir) curr
                                        (looping
                                          (let1 next (move-straight pos dir)
                                            (unless (char= (gethash next map) #\#)
                                              (collect! (cons (list next dir) 1))))
                                          (collect! (cons (list pos (rotate-cw dir)) 1000))
                                          (collect! (cons (list pos (rotate-ccw dir)) 1000)))))
            :heuristic [manhattan-distance (car _) end])))))
(solve)
(length (remove-duplicates (solve2) :test 'equal))
(length (solve2))
(length *)
672 too high
537 too low
(defun solve2 (&optional (input (parse-input)))
  (let1 best (solve input)
    (let1 memo (make-hash-table :test 'equal)
      (looping
        (destructuring-bind (map (pos dir) end) input
          (labels ((recur (pos dir cost path)
                     (when (or (not (gethash (list pos dir) memo))
                               (<= cost (gethash (list pos dir) memo)))
                       #+#:excluded (dbgl pos dir cost path)
                       (setf (gethash (list pos dir) memo) cost)
                       (cond ((and (equal pos end) (= cost best)) #+#:excluded (dbgl path)
                                                                  (dolist (step path)
                                                                    (adjoin! step :test 'equal)))
                             ((> cost best) nil)
                             (t (let1 next (move-straight pos dir)
                                  (unless (char= (gethash next map) #\#)
                                    (recur next dir (1+ cost) (cons next path)))
                                  (recur pos (rotate-cw dir) (+ cost 1000) path)
                                  (recur pos (rotate-ccw dir) (+ cost 1000) path)))))))
            (recur pos dir 0 nil)))))))
(solve)
(solve2)
(length *)








(define-solution (2024 16) (input parse-input)
  (values (looping
            (doseq ((a b prize) input)
              (awhen (solve a b prize)
                (sum! it))))
          (looping
            (doseq ((a b (px py)) input)
              (awhen (solve a b (list (+ px 10000000000000) (+ py 10000000000000)))
                (sum! it))))))

(define-test (2024 16) (39290 73458657399094))
