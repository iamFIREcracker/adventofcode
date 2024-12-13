(defpackage :aoc/2024/12 #.cl-user::*aoc-use*)
(in-package :aoc/2024/12)

(defun parse-input (&optional (strings (uiop:read-file-lines #P"src/2024/day12.txt")))
  (prog1-let (map (make-hash-table :test 'equal))
    (doseq ((i s) (enumerate strings))
      (doseq ((j ch) (enumerate s))
        (setf (gethash (list i j) map) ch)))))
#+#:excluded (parse-input)


(defun regions (&optional (map (parse-input)))
  (looping
    (let1 seen (make-hash-table :test 'equal)
      (dohash ((i j) type map)
        (let ((area 0) fence
              (frontier (list (list i j))))
          (while frontier
            (destructuring-bind (i j) (pop frontier)
              (unless (gethash (list i j) seen)
                (setf (gethash (list i j) seen) t)
                (incf area)
                (doseq ((di dj) '((-1 0) (0 1) (1 0) (0 -1)))
                  (let1 next (list (+ i di) (+ j dj))
                    (if (eql (gethash next map) type)
                        (push next frontier)
                        (push (list (list i j) (list di dj)) fence)))))))
          (when (and (plusp area) fence)
            (collect! (list type area fence))))))))
(defun rtype (r) (car r))
(defun area (r) (cadr r))
(defun fence (r) (caddr r))
(defun perimeter (r) (length (fence r)))


;; Each straight section of fence counts as a side, regardless of how long it
;; is.  So for each section, we can check if there is another one adjacent,
;; facing the same direction, and if there is, we subtract one from the total
;; sides count.
(defun sides (r &aux (fence (fence r)))
  (prog1-let (total (length fence))
    (flet ((adjacent? (p1 p2)
             (= (+ (abs (- (car p1) (car p2)))
                   (abs (- (cadr p1) (cadr p2))))
                1)))
      (dosublists (((pos dir) . remaining) fence)
        (doseq ((pos1 dir1) remaining)
          (when (and (equal dir dir1) (adjacent? pos pos1))
            (decf total)))))))


(define-solution (2024 12) (map parse-input)
  (let1 regions (regions map)
    (values (reduce #'+ regions :key [* (area _)  (perimeter _)])
            (reduce #'+ regions :key [* (area _)  (sides _)]) )))

(define-test (2024 12) (1431440 869070))
