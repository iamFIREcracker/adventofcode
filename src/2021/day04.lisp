(defpackage :aoc/2021/04 #.cl-user::*aoc-use*)
(in-package :aoc/2021/04)


(defstruct (bingo (:conc-name nil))
  to-draw
  boards)

(defun parse-bingo (data)
  (let ((to-draw (extract-integers (car data)))
        (boards (parse-boards (cdr data))))
    (make-bingo :to-draw to-draw :boards boards)))

(defun parse-boards (data)
  (when data
    (cons
      (make-array '(5 5)
                  :initial-contents
                  (let ((lines (subseq data 1 6)))
                    (uiop:while-collecting (rows)
                      (dolist (s lines)
                        (let ((row (extract-integers s)))
                          (rows row))))))
      (parse-boards (subseq data 6)))))

(defun extract-integers (s)
  (mapcar #'parse-integer (cl-ppcre:all-matches-as-strings "\\d+" s)))


(defun play (game)
  (uiop:while-collecting (scores)
    (with-slots (to-draw boards) game
      (dolist (n to-draw)
        (dolist (b boards)
          (when (mark-number b n)
            (scores (* (score b) n))
            (setf boards (remove b boards))))))))


(defun mark-number (board n)
  (loop for i below 5 do
        (loop for j below 5
              when (eql (aref board i j) n) do
              (setf (aref board i j) nil)
              (return-from mark-number (board-won-p board i j)))))

(defun board-won-p (board last-marked-i last-marked-j)
  (or (loop for i below 5 never (aref board i last-marked-j))
      (loop for j below 5 never (aref board last-marked-i j))))

(defun score (board)
  (loop for i below 25 for v = (row-major-aref board i) when v sum v))


(define-solution (2021 04) (game parse-bingo)
  (let ((scores (play game)))
    (values (first scores) (car (last scores)))))

(define-test (2021 04) (82440 20774))
