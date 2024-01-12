(defpackage :aoc/2022/12 #.cl-user::*aoc-use*)
(in-package :aoc/2022/12)


#+#:excluded (&optional (program (uiop:read-file-forms #P"src/2022/day12.txt")))
#+#:excluded (uiop:read-file-forms #P"scratch.txt")

; (clockin 739)
; (clockout 909)
; (clockin 958)
; (idea repetitions w/ deltas)
; (clockout 1000)
; (clockin 1042)
; (idea lcm of all the divisors)

(defun parse-map (&optional (strings (uiop:read-file-lines #P"src/2022/day12.txt")))
  (let* ((n (length strings))
         (m (length (car strings)))
         (map (make-array (list n m)))
         start
         end)
    (loop for s in strings for i from 0 do
          (loop for ch across s for j from 0 do
                (if (char= ch #\S) (setf start (list i j)
                                         (aref map i j) -1)
                  (if (char= ch #\E) (setf end (list i j)
                                           (aref map i j) 25)
                    (setf (aref map i j) (- (char-code ch) (char-code #\a)))))))
    (list map start end)))
#+#:excluded (parse-map)
(- (char-code #\b) (char-code #\a))

(defun solve ()
  (destructuring-bind (map start end) (parse-map)
    (search-cost (a* start
                     :goal-state end
                     :test 'equal
                     :neighbors [neighbors map _]
                     :heuristic [manhattan-distance end _]))))
#+#:excluded (solve)

(defparameter *nhood-d1-list* '((-1 0) (0 1) (1 0) (0 -1)))

(defun neighbors (map state)
  (destructuring-bind (i j) state
    (loop for (di dj) in *nhood-d1-list*
          for ii = (+ di i) for jj = (+ dj j)
          when (and (array-in-bounds-p map ii jj)
                 (<= (aref map ii jj) (1+ (aref map i j))))
          collect (cons (list ii jj) 1))))

(defun solve2 ()
  (destructuring-bind (map start end) (parse-map)
    (search-cost (bfs end
                     :goalp [= (aref map (car _) (cadr _)) 0]
                     :test 'equal
                     :neighbors [neighbors map _]))))

(defun neighbors2 (map state)
  (destructuring-bind (i j) state
    (loop for (di dj) in *nhood-d1-list*
          for ii = (+ di i) for jj = (+ dj j)
          when (and (array-in-bounds-p map ii jj)
                 (>= (aref map ii jj) (1- (aref map i j))))
          collect (list ii jj))))
#+#:excluded (solve2)

;;; Parsing nightmare: flipped char-code logic, did not set elevation for start/end position
;;; Part2: forgot to remove heuristic after I switched to GOALP
