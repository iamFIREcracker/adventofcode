(defpackage :aoc/2019/13 #.cl-user::*aoc-use*)
(in-package :aoc/2019/13)

(defparameter *interactive* NIL)

(defstruct (game (:constructor make-game%))
  program
  in
  out
  screen
  score)

(defun make-game (program)
  (let* ((program (intcode:make-program (copy-hash-table (intcode:program-memory program)))))
    (make-game% :program program
                :in (intcode:program-in program)
                :out (intcode:program-out program)
                :screen (make-hash-table)
                :score 0)))

(defun game-print (game &aux (screen (game-screen game)))
  (let ((min-x (minimization (hash-table-keys screen) :key #'realpart))
        (max-x (maximization (hash-table-keys screen) :key #'realpart))
        (min-y (minimization (hash-table-keys screen) :key #'imagpart))
        (max-y (maximization (hash-table-keys screen) :key #'imagpart)))
    (doirange (y max-y min-y -1)
      (doirange (x min-x max-x)
        (format T "~a" (case (gethash (complex x y) screen)
                         (0 #\Space)
                         (1 #\|)
                         (2 #\#)
                         (3 #\-)
                         (4 #\o))))
      (format T "~&"))
    (format T "~a~%" (game-score game))))

(defun game-play (game &key (interactive T))
  (let* ((program (game-program game))
         (memory (intcode:program-memory program))
         (*interactive* interactive))
    (hash-table-insert memory 0 2)
    (game-run game)))

(defun game-guess-move (game ball bar)
  (let ((ball-x (realpart ball))
        (bar-x (realpart bar)))
    (- ball-x bar-x)))

(defun game-run (game)
  (loop
    :with program = (game-program game)
    :with screen = (game-screen game)
    :with ball
    :with bar
    :for running = (intcode:program-run program)
    :do (loop
          :for x = (dequeue (intcode:program-out program))
          :for y = (dequeue (intcode:program-out program))
          :for pos = (complex x (- y))
          :for tile-id = (dequeue (intcode:program-out program))
          :when (= tile-id 4) :do (setf ball pos)
          :when (= tile-id 3) :do (setf bar pos)
          :when (>= x 0) :do (hash-table-insert screen pos tile-id)
          :when (= x -1) :do (setf (game-score game) tile-id)
          :until (queue-empty-p (intcode:program-out program)))
    :while running
    :do (enqueue (game-guess-move game ball bar) (game-in game)) 
    :when *interactive* :do (progn (game-print game) (sleep .2))
    :finally (return (game-score game))))

(define-problem (2019 13) (program intcode:read-program)
  (values
    (let ((game (make-game program)))
      (game-run game)
      (count 2 (hash-table-values (game-screen game))))
    (let ((game (make-game program)))
      (game-play game :interactive NIL))))

(1am:test test-2019/13
  (multiple-value-bind (part1 part2) (problem-run)
    (1am:is (= 255 part1))
    (1am:is (= 12338 part2))))
