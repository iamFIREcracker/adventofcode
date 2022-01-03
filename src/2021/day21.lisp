(defpackage :aoc/2021/21 #.cl-user::*aoc-use*)
(in-package :aoc/2021/21)


(defun parse-positions (data)
  (flet ((player-position (string)
           (cl-ppcre:register-groups-bind ((#'parse-integer pos))
               ("Player \\d+ starting position: (\\d+)" string)
             pos)))
    (cons (player-position (first data)) (player-position (second data)))))
(defun player1 (positions) (car positions))
(defun player2 (positions) (cdr positions))

(defun part1 (positions) (play (player1 positions) (player2 positions)))

(defun play (p1 p2 &optional (s1 0) (s2 0) (last-die 100) (roll-count 0)
                 &aux
                 (result (mod1 (+ (+ last-die 1) (+ last-die 2) (+ last-die 3)) 10))
                 (last-die (mod1 (+ last-die 3) 100))
                 (p1 (mod1 (+ p1 result) 10))
                 (s1 (+ s1 p1))
                 (roll-count (+ roll-count 3)))
  (if (>= s1 1000) (* s2 roll-count) (play p2 p1 s2 s1 last-die roll-count)))


(defun mod1 (n max)
  (loop while (> n max) do (decf n max))
  n)


(defun part2 (positions)
  (destructuring-bind (wc1 . wc2) (count-wins (player1 positions) (player2 positions))
    (max wc1 wc2)))

(defun count-wins (p1 p2
                      &optional (s1 0) (s2 0) (dp (make-hash-table :test 'equal))
                      &aux (key (list p1 s1 p2 s2)))
  (uiop:if-let (wins (gethash key dp))
    wins
    (setf (gethash key dp)
          (let ((wc1 0) (wc2 0))
            (loop for d1 from 1 to 3 do
                  (loop for d2 from 1 to 3 do
                        (loop for d3 from 1 to 3
                              for p1-next = (mod1 (+ p1 d1 d2 d3) 10)
                              for s1-next = (+ s1 p1-next) do
                              (if (>= s1-next 21)
                                (incf wc1)
                                (destructuring-bind (w2 . w1) (count-wins p2 p1-next s2 s1-next dp)
                                  (incf wc1 w1)
                                  (incf wc2 w2))))))
            (cons wc1 wc2)))))

(define-solution (2021 21) (positions parse-positions)
  (values (part1 positions) (part2 positions)))

(define-test (2021 21) (998088 306621346123766))
