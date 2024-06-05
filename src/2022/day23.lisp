(defpackage :aoc/2022/23 #.cl-user::*aoc-use*)
(in-package :aoc/2022/23)

(defun grid (&optional (file #P"src/2022/day23.txt"))
  (bnd1 grid (make-hash-table :test 'equal)
    (loop for string in (uiop:read-file-lines file) for row from 0 do
          (loop for ch across string for col from 0 do
                (when (char= ch #\# )
                  (setf (gethash (list row col) grid) t))))
    grid))

(defparameter *nhood-d1-list* '((-1 0) (0 1) (1 0) (0 -1) (-1 1) (1 1) (1 -1) (-1 -1)))

(defun neighbors (grid pos &optional (deltas *nhood-d1-list*))
  (loop for d in deltas for pos-next = (mapcar #'+ pos d)
        when (gethash pos-next grid) collect pos-next))

(defparameter *to-check* (list
                           ;; North
                           '((-1 -1) (-1 0) (-1 1))
                           ;; South
                           '((1 -1) (1 0) (1 1))
                           ;; West
                           '((-1 -1) (0 -1) (1 -1))
                           ;; East
                           '((-1 1) (0 1) (1 1))))
(defparameter *moves* (list
                        ;; North
                        '(-1 0)
                        ;; South
                        '(1 0)
                        ;; West
                        '(0 -1)
                        ;; East
                        '(0 1)))

(defun turn (grid to-check)
  (let ((proposals (make-hash-table :test 'equal))
        (candidates (make-hash-table :test 'equal))
        (grid-next (make-hash-table :test 'equal)))
    (loop for pos being the hash-keys of grid do
          (loop repeat 4
                for to-check-next = to-check then (mod (1+ to-check-next) 4)
                for move = (nth to-check-next *moves*)
                for deltas = (nth to-check-next *to-check*) do
                (when (neighbors grid pos)
                  (unless (neighbors grid pos deltas)
                    (bnd1 pos-next (mapcar #'+ pos move)
                      (setf (gethash pos proposals) pos-next)
                      #+#:excluded (pr pos '-> pos-next)
                      (push pos (gethash pos-next candidates))
                      (return))))))
    (bnd1 moved nil
      (loop for pos being the hash-keys of grid using (hash-value to-check)
            for pos-next = (gethash pos proposals) do
            (if (= (length (gethash pos-next candidates)) 1)
              (setf (gethash pos-next grid-next) t moved t)
              (setf (gethash pos grid-next) t)))
      (values grid-next moved))))

(defun count-empty (grid)
  (loop for (row col) being the hash-keys of grid
        minimize row into row-min
        maximize row into row-max
        minimize col into col-min
        maximize col into col-max
        finally (return (loop for row from row-min upto row-max sum
                              (loop for col from col-min upto col-max
                                    count (not (gethash (list row col) grid)))))))

(defun print-grid (grid)
  (loop for (row col) being the hash-keys of grid
        minimize row into row-min
        maximize row into row-max
        minimize col into col-min
        maximize col into col-max
        finally (progn
                  (loop for row from row-min upto row-max do
                        (loop for col from col-min upto col-max do
                              (aif (gethash (list row col) grid)
                                (princ #\# )
                                (princ #\.)))
                        (terpri))
                  (pr (list (1+ (- row-max row-min))
                            (1+ (- col-max col-min)))
                      (hash-table-count grid))))
  (terpri))
#; Scratch

(let ((grid (grid #+#:excluded #P"scratch.txt"))
      (to-check 0)
      moved
      (turn 0))
  (print-grid grid)
  (repeat 10000
    (setf (values grid moved) (turn grid to-check)
          to-check (mod (1+ to-check) 4)
          turn (1+ turn))
    (unless moved
      (error turn))
    (print-grid grid)
    #+#:excluded (break))
  (count-empty grid))

1510
;; did not realize elves move only move n,s,w,e, and not on the diagonals as well
;; was not breaking out of the inner loop after I found a candidate
;; none of the adj cells in one direction have to contain another elf -,-
;; it's wrong that each elf has memory as to what the next move will be; the next direction to check is on a turn by turn basis... irrespective of whether the elves had moved or not
;; using today's input instead of yesterday's would have helped
29194 -- too high
