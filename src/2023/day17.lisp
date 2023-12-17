(defpackage :aoc/2023/17 #.cl-user::*aoc-use*)
(in-package :aoc/2023/17)


(defun parse-map (&optional (strings (uiop:read-file-lines #P"src/2023/day17.txt")))
  (bnd* ((rows (length strings))
         (cols (length (first strings)))
         (map (make-hash-table :test 'equal)))
    (dolist+ ((i s) (enumerate strings))
      (dolist+ ((j ch) (enumerate s))
        (setf (gethash (list i j) map) (parse-integer (mkstr ch)))))
    (list map rows cols)))
#+#:excluded (parse-map)


(defparameter *north* '(-1 0))
(defparameter *east*  '(0 1))
(defparameter *south* '(1 0))
(defparameter *west*  '(0 -1))

(defun advance (pos dir) (mapcar #'+ pos dir))
(defun rotate-right (dir)
  (cond ((equal dir *north*) *east*)
        ((equal dir *east*) *south*)
        ((equal dir *south*) *west*)
        ((equal dir *west*) *north*)))


(defun turn-back? (dir prev-dirs)
  (aand (car prev-dirs)
        (equal dir
               (rotate-right (rotate-right it)))))
#+#:excluded (turn-back? *north* (list *east*))

(defun straight-for-too-long? (dir prev-dirs)
  (and (equal dir (first prev-dirs))
       (equal dir (second prev-dirs))
       (equal dir (third prev-dirs)) ))
#+#:excluded (straight-for-too-long? *north* (list *north*))
#+#:excluded (straight-for-too-long? *north* (list *north* *north*))
#+#:excluded (straight-for-too-long? *north* (list *south* *north* *north*))

(defun slice (seq end) (subseq seq 0 (min (length seq) end)))
#+#:excluded (slice (cons *north* (cons *north* (cons *north* nil))) 3)
#+#:excluded (slice (list 1 2 3 4) 3)

(defun neighbors (map state)
  ; (declare (optimize (debug 3)))
  ; (break)
  (destructuring-bind (pos prev-dirs) state
    (looping
      (dolist (dir (list *north* *east* *south* *west*))
        (bnd* ((npos (advance pos dir))
               (hl (gethash npos map)))
          (when hl
            (unless (turn-back? dir prev-dirs)
              (unless (straight-for-too-long? dir prev-dirs)
                (collect!
                  (cons (list npos (slice (cons dir prev-dirs) 3))
                        hl))))))))))

(defun part1 (&optional (input (parse-map)))
  ; (declare (optimize (debug 3)))
  (destructuring-bind (map rows cols) input
    (bnd* ((start (list (list 0 0) nil))
           (goal (list (1- rows) (1- cols))))
      (a* start
          :init-cost 0 ; as per text
          :goalp [equal (first _) goal]
          :test 'equal
          :state-key (lambda (state)
                       (destructuring-bind (pos prev-dirs) state
                         (list pos
                               (first prev-dirs)
                               (streak-length prev-dirs))))
          :neighbors [neighbors map _]
          :heuristic [manhattan-distance (first _) goal]))))
#+#:excluded (part1)
; 963

(defun streak-length (prev-dirs)
  (or (awhen (car prev-dirs)
        (1+ (loop for cur in (slice (cdr prev-dirs) 9)
                  while (equal it cur) count 1)))
      0))
; (streak-length nil)
; (streak-length (list 1))
; (streak-length (list 1 2))
; (streak-length (list 1 1 2))

(defun turn-but-cannot? (dir prev-dirs)
  (and (car prev-dirs)
       (not (equal (car prev-dirs) dir))
       (< (streak-length prev-dirs) 4)))

(defun straight-but-cannot? (dir prev-dirs)
  (and (equal (car prev-dirs) dir)
       (= (streak-length prev-dirs) 10)))


(defun neighbors2 (map state)
  (destructuring-bind (pos prev-dirs) state
    (looping
      (dolist (dir (list *north* *east* *south* *west*))
        (bnd* ((npos (advance pos dir))
               (hl (gethash npos map)))
          (when hl
            (unless (turn-back? dir prev-dirs)
              (unless (turn-but-cannot? dir prev-dirs)
                (unless (straight-but-cannot? dir prev-dirs)
                  (collect!
                    (cons (list npos (slice (cons dir prev-dirs) 10))
                          hl)))))
            ))))))

(defun part2 (&optional (input (parse-map)))
  ; (declare (optimize (debug 3)))
  (destructuring-bind (map rows cols) input
    (bnd* ((start (list (list 0 0) nil))
           (goal (list (1- rows) (1- cols))))
      (a* start
          :init-cost 0 ; as per text
          :goalp (lambda (state)
                   (and (equal (first state) goal)
                        (>= (streak-length (second state)) 4)))
          :test 'equal
          :state-key (lambda (state)
                       (destructuring-bind (pos prev-dirs) state
                         (list pos
                               (first prev-dirs)
                               (streak-length prev-dirs))))
          :neighbors [neighbors2 map _]
          :heuristic [manhattan-distance (first _) goal]))))
(untrace neighbors turn-but-cannot? straight-but-cannot?)

#+#:excluded (part2)
; 835 nope
; 1176 nope
; 1178
