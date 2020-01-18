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

(defun game-print (game)
  (print-hash-table-map (game-screen game) (lambda (value &optional key)
                                             (declare (ignore key))
                                             (case value
                                               (0 #\Space)
                                               (1 #\|)
                                               (2 #\#)
                                               (3 #\-)
                                               (4 #\o))))
  (format T "~a~%" (game-score game)))

(defun game-play (game &key (interactive T))
  (let* ((program (game-program game))
         (memory (intcode:program-memory program))
         (*interactive* interactive))
    (hash-table-insert memory 0 2)
    (game-run game)))

(defun guess-move (ball bar)
  (let ((ball-x (realpart ball))
        (bar-x (realpart bar)))
    (if (< ball-x bar-x)
      -1
      (if (> ball-x bar-x)
        1
        0))))

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
    :do (enqueue (guess-move ball bar) (game-in game))
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
