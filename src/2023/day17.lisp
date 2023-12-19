(defpackage :aoc/2023/17 #.cl-user::*aoc-use*)
(in-package :aoc/2023/17)


(defparameter *north* '(-1 0))
(defparameter *east*  '(0 1))
(defparameter *south* '(1 0))
(defparameter *west*  '(0 -1))

(defun move-straight (pos dir) (mapcar #'+ pos dir))
(defun rotate-cw (dir)
  (cond ((equal dir *north*) *east*)
        ((equal dir *east*) *south*)
        ((equal dir *south*) *west*)
        ((equal dir *west*) *north*)))


(defun parse-map (&optional (strings (uiop:read-file-lines #P"src/2023/day17.txt")))
  (bnd* ((rows (length strings))
         (cols (length (first strings)))
         (map (make-hash-table :test 'equal)))
    (dolist+ ((i s) (enumerate strings))
      (dolist+ ((j ch) (enumerate s))
        (setf (gethash (list i j) map) (parse-integer (mkstr ch)))))
    (list map rows cols)))


(defun straight? (dir prev) (equal dir prev))
(defun turn? (dir prev) (not (straight? dir prev)))
(defun turn-back? (dir prev) (equal (rotate-cw (rotate-cw dir)) prev))


(defun minimize-heat-loss (input &key min-streak-to-turn max-streak)
  (destructuring-bind (map rows cols) input
    (bnd* ((init-state `((0 0) (0 0) 0))
           (end (list (1- rows) (1- cols))))
      (flet ((neighbors (state)
               (destructuring-bind (pos dir streak) state
                 (looping
                   (dolist (ndir (list *north* *east* *south* *west*))
                     (bnd* ((npos (move-straight pos ndir))
                            (hl (gethash npos map)))
                       (when hl
                         (unless (turn-back? ndir dir)
                           (when (or (zerop streak) ; first turn is allowed!
                                     (and (turn? ndir dir) (>= streak min-streak-to-turn))
                                     (and (straight? ndir dir) (< streak max-streak)))
                             (collect!
                               (cons (list npos ndir (if (straight? ndir dir) (1+ streak) 1))
                                     hl)))))))))))
        (search-cost
          (a* init-state
              :goalp [equal (first _) end]
              :test 'equal
              :neighbors #'neighbors
              :heuristic [manhattan-distance (first _) end]))))))


(define-solution (2023 17) (input parse-map)
  (values (minimize-heat-loss input :min-streak-to-turn 0 :max-streak 3)
          (minimize-heat-loss input :min-streak-to-turn 4 :max-streak 10)))

(define-test (2023 17) (963 1178))
