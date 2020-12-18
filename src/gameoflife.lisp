(defpackage :gol
  (:use :cl :hset)
  (:export
    :next
    :print-state))
(in-package :gol)

(defparameter *nhood-deltas*
  (loop for x from -1 upto 1 nconc
        (loop for y from -1 upto 1
              unless (= x y 0) collect (list x y))))

(defun neighbors (pos)
  "Returns the positions of all the cells which are neighbors to `pos`."
  (loop for delta in *nhood-deltas* collect (mapcar #'+ pos delta)))

(defun frequencies (list &key (test 'eql))
  "Returns an association list mapping unique elements of `list` to the number
  of times they occur in `list`."
  (let ((f (make-hash-table :test test)))
    (dolist (each list)
      (incf (gethash each f 0)))
    (loop for k being the hash-keys of f collect (cons k (gethash k f)))))

(defun cownway-rules (pos n state)
  "Returns a predicate implementing Conway's Game of Life rules.

  - Any live cell with fewer than two live neighbours dies, as if by
  underpopulation.
  - Any live cell with two or three live neighbours lives on to the next
  generation.
  - Any live cell with more than three live neighbours dies, as if by
  overpopulation.
  - Any dead cell with exactly three live neighbours becomes a live cell, as if
  by reproduction."
  (or (= n 3) (and (= n 2) (hset-contains-p pos state))))

(defun next (state &key (neighbors #'neighbors) (should-be-alive-p #'cownway-rules))
  "Applies evolutionary rules to `state`, and returns the next state of the
  game.

  `state` is a SET, listing the positions of all the _alive_ cells.

  Instead of iterating each cell (dead or alive), count which of their
  neighbors happen to be dead / alive, and finally update the current cell
  state, we do the opposite.  We start from all the cells which are alive,
  collect their neighbor positions, count how many times each position is
  found, and use that information to update the state of the current cell - the
  idea behind this is that if a position is mentioned/collected twice, it means
  that that position would have 2 neighbor cells which are alive."
  (make-hset
    (loop
      for (pos . n) in (frequencies (mapcan neighbors (hset-values state)) :test 'equal)
      when (funcall should-be-alive-p pos n state) collect pos)
    :test 'equal))

(defun print-state (state width height)
  "Prints given game-of-life `state`."
  (dotimes (y height)
    (dotimes (x width)
      (princ (if (hset-contains-p (list x y) state) #\# #\-)))
    (terpri)))
