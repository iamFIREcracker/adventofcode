(defpackage :aoc/2022/22 #.cl-user::*aoc-use*)
(in-package :aoc/2022/22)

(defun input (&optional (file #P"src/2022/day22.txt"))
  (destructuring-bind (grid intstructions)
      (split-sequence:split-sequence "" (uiop:read-file-lines file) :test #'string=)
    (list (parse-grid grid) (parse-instructions (car intstructions)))))

(defstruct (grid :conc-name)
  cells row-min row-max col-min col-max)

(defun row (pos) (car pos))
(defun col (pos) (cadr pos))

(defun parse-grid (lines)
  (let* ((cells (make-hash-table :test 'equal))
         (rows (length lines))
         (cols (length (car lines)))
         (row-min (make-hash-table))
         (row-max (make-hash-table))
         (col-min (make-hash-table))
         (col-max (make-hash-table)))
    (loop for s in lines for row from 1 do
          (loop for ch across s for col from 1 do
                (when (find ch ".#")
                  (setf (gethash (list row col) cells) ch))))
    (loop for row from 1 upto rows
          for row-col-min = (loop for col from 1 upto cols
                                  when (gethash (list row col) cells) return col)
          for row-col-max = (loop for col from cols downto 1
                                  when (gethash (list row col) cells) return col)
          do (setf (gethash row col-min) row-col-min
                   (gethash row col-max) row-col-max))
    (loop for col from 1 upto cols
          for col-row-min = (loop for row from 1 upto rows
                                  when (gethash (list row col) cells) return row)
          for col-row-max = (loop for row from rows downto 1
                                  when (gethash (list row col) cells) return row)
          do (setf (gethash col row-min) col-row-min
                   (gethash col row-max) col-row-max))
    (make-grid :cells cells
               :row-min row-min
               :row-max row-max
               :col-min col-min
               :col-max col-max)))

(defun parse-instructions (s)
  (let ((steps (mapcar #'parse-integer (cl-ppcre:all-matches-as-strings "\\d+" s)))
        (turns (cl-ppcre:all-matches-as-strings "[LR]" s)))
    (list steps turns)))

(defun wallp (grid pos) (char= (gethash pos (cells grid)) #\# ))
(defun out-of-grid-p (grid pos) (not (gethash pos (cells grid))))

(defun walk (&optional (input (input)))
  (destructuring-bind (grid (steps turns)) input
    (let ((pos (list 1 (gethash 1 (col-min grid))))
          (facing (list 0 1)))
      (print-grid grid pos facing)
      (loop for n in steps for d in (append turns (list nil)) do
            (pr pos facing n d)
            (repeat n
              (destructuring-bind (pos-next facing-next) (forward grid pos facing)
                (pr pos-next facing-next)
                (if (wallp grid pos-next)
                  (return)
                  (setf pos pos-next
                        facing facing-next))))
            #+#:excluded (pr pos facing)
            (assert (gethash pos (cells grid)))
            (assert (char= (gethash pos (cells grid)) #\.))
            (when d
              #+#:excluded (pr facing d)
              (setf facing (turn facing d))
              #+#:excluded (pr facing ))
            (print-grid grid pos facing)
            #+#:excluded (break))

      (+ (pr (* (row pos) 1000) (row pos))
         (pr (* (col pos)    4) (col pos))
         (pr (position facing (list (list 0  1)
                                    (list 1  0)
                                    (list 0 -1)
                                    (list -1 0))
                       :test 'equal)
             facing)))))

(defun region (pos)
  (1+ (+ (* (floor (1- (row pos)) 50) 3)
         (floor (1- (col pos)) 50))))
#+#:excluded (region (list 1 1))
#+#:excluded (region (list 50 100))

(defun forward (grid pos facing)
  (bnd1 (pos-next (mapcar [+ _1 _2] pos facing))
    (pr pos pos-next (gethash pos-next (cells grid)))
    (if (out-of-grid-p grid pos-next)
      (wrap grid pos facing) ; XXX we are passing the current pos, not next
      (list pos-next facing))))

(defun wrap (grid pos facing)
  (bnd1 (region (region pos))
    (cond ((equal (list 0   1) facing)
           (ecase region
             (3 (list
                  (list (- 151 (row pos)) 100)
                  (list 0 -1)))
             (5 (list
                  (list 50 (+ 100 (- (row pos) 50)))
                  (list -1 0)))
             (8 (list
                  (list (- 151 (row pos)) 150)
                  (list 0 -1)))
             (10 (list
                   (list 150 (+ 50 (- (row pos) 150)))
                   (list -1 0)))))
          ((equal (list 0   -1) facing)
           (ecase region
             (2 (list
                  (list (- 151 (row pos)) 1)
                  (list 0 1)))
             (5 (list
                  (list 101 (- (row pos) 50))
                  (list 1 0)))
             (7 (list
                  (list (- 51 (- (row pos) 100)) 51)
                  (list 0 1)))
             (10 (list
                   (list 1 (+ (- (row pos) 150) 50))
                   (list 1 0)))))
          ((equal (list 1 0) facing)
           (ecase region
             (3 (list
                  (list (+ (- (col pos) 100) 50) 100)
                  (list 0 -1)))
             (8 (list
                  (list (+ (- (col pos) 50) 150) 50)
                  (list 0 -1)))
             (10 (list
                   (list 1 (+ (col pos) 100))
                   (list 1 0)))))
          ((equal (list -1 0) facing)
           (ecase region
             (2 (list
                  (list (+ (- (col pos) 50) 150) 1)
                  (list 0 1)))
             (3 (list
                  (list 200 (- (col pos) 100))
                  (list -1 0)))
             (7 (list
                  (list (+ (col pos) 50) 51)
                  (list 0 1))))))))

(defun turn (facing d)
  (cond ((string= d "R") (list (col facing) (- (car facing))))
        ((string= d "L") (list (- (col facing)) (car facing)))))

(defun print-grid (grid pos facing)
  (loop for row from (- (row pos) 10) to (+ (row pos) 10) do
        (loop for col from (- (col pos) 10) to (+ (col pos) 10) for curr = (list row col) do
              (princ
                (if (equal curr pos)
                  (cond ((equal (list 0 1) facing) #\>)
                        ((equal (list 1 0) facing) #\v)
                        ((equal (list 0 -1) facing) #\<)
                        ((equal (list -1 0) facing) #\^))
                  (or (gethash (list row col) (cells grid))
                      #\Space))))
        (terpri))
  (pr '----))

#; Scratch
;; Right
;; 3
(assert (equal (wrap (car (input)) (list 1 150) (list 0 1))
               '((150 100) (0 -1))))
(assert (equal (wrap (car (input)) (list 50 150) (list 0 1))
               '((101 100) (0 -1))))
;; 5
(assert (equal (wrap (car (input)) (list 51 100) (list 0 1))
               '((50 101) (-1 0))))
(assert (equal (wrap (car (input)) (list 100 100) (list 0 1))
               '((50 150) (-1 0))))
;; 8
(assert (equal (wrap (car (input)) (list 101 100) (list 0 1))
               '((50 150) (0 -1))))
(assert (equal (wrap (car (input)) (list 150 100) (list 0 1))
               '((1 150) (0 -1))))
;; 10
(assert (equal (wrap (car (input)) (list 151 50) (list 0 1))
               '((150 51) (-1 0))))
(assert (equal (wrap (car (input)) (list 200 50) (list 0 1))
               '((150 100) (-1 0))))
;; Left
;; 2
(assert (equal (wrap (car (input)) (list 1 51) (list 0 -1))
               '((150 1) (0 1))))
(assert (equal (wrap (car (input)) (list 50 51) (list 0 -1))
               '((101 1) (0 1))))
;; 5
(assert (equal (wrap (car (input)) (list 51 51) (list 0 -1))
               '((101 1) (1 0))))
(assert (equal (wrap (car (input)) (list 100 51) (list 0 -1))
               '((101 50) (1 0))))

;; 7
(assert (equal (wrap (car (input)) (list 101 1) (list 0 -1))
               '((50 51) (0 1))))
(assert (equal (wrap (car (input)) (list 150 1) (list 0 -1))
               '((1 51) (0 1))))
;; 10
(assert (equal (wrap (car (input)) (list 151 1) (list 0 -1))
               '((1 51) (1 0))))
(assert (equal (wrap (car (input)) (list 200 1) (list 0 -1))
               '((1 100) (1 0))))
;; Down
;; 3
(assert (equal (wrap (car (input)) (list 50 101) (list 1 0))
               '((51 100) (0 -1))))
(assert (equal (wrap (car (input)) (list 50 150) (list 1 0))
               '((100 100) (0 -1))))
;; 8
(assert (equal (wrap (car (input)) (list 150 51) (list 1 0))
               '((151 50) (0 -1))))
(assert (equal (wrap (car (input)) (list 150 100) (list 1 0))
               '((200 50) (0 -1))))
;; 10
(assert (equal (wrap (car (input)) (list 200 1) (list 1 0))
               '((1 101) (1 0))))
(assert (equal (wrap (car (input)) (list 200 50) (list 1 0))
               '((1 150) (1 0))))
;; Up
;; 2
(assert (equal (wrap (car (input)) (list 1 51) (list -1 0))
               '((151 1) (0 1))))
(assert (equal (wrap (car (input)) (list 1 100) (list -1 0))
               '((200 1) (0 1))))
;; 3
(assert (equal (wrap (car (input)) (list 1 101) (list -1 0))
               '((200 1) (-1 0))))
(assert (equal (wrap (car (input)) (list 1 150) (list -1 0))
               '((200 50) (-1 0))))
;; 7
(assert (equal (wrap (car (input)) (list 101 1) (list -1 0))
               '((51 51) (0 1))))
(assert (equal (wrap (car (input)) (list 101 50) (list -1 0))
               '((100 51) (0 1))))

(turn (list 0 1) "L")
(turn (list 0 1) "R")
(turn (list 0 -1) "L")
(turn (list 0 -1) "R")
(input)
(walk)
(walk (input #P"scratch.txt"))
(&optional (file #P"src/2022/day22.txt"))
#P"scratch.txt"
489126
21562
;; flipped rows with cols
;; had to reimplement rotation
;; forgot about map wrapping when walking
;; did not realize you could bump into the tiles... the example is maliciously crafted so you would not notice...
54286 ; nope wtf?
54284 ; nope
54287 ; nope
54285 ; nope
