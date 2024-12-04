(defpackage :aoc/2024/04 #.cl-user::*aoc-use*)
(in-package :aoc/2024/04)

#;

(defun parse-grid (&optional (strings (uiop:read-file-lines #P"src/2024/day04.txt")))
  (let1 grid (make-hash-table :test 'equal)
    (doseq ((i s) (enumerate strings))
      (doseq ((j ch) (enumerate s))
        (setf (gethash (list i j) grid) ch)))
    grid))

(defparameter *nhood-d1-list* '((-1 0) (0 1) (1 0) (0 -1)))

(defparameter *nhood-diagonal-list* (concatenate 'list
                                                    *nhood-d1-list*
                                                    '((-1 1) (1 1) (1 -1) (-1 -1))))
#|
(defun xmas-north? (grid i j)
  (and (eql (gethash (list i j) grid) #\X)
       (eql (gethash (list (- i 1) j) grid) #\M)
       (eql (gethash (list (- i 2) j) grid) #\A)
       (eql (gethash (list (- i 3) j) grid) #\S)))
 
(defun xmas-east? (grid i j)
  (and (eql (gethash (list i j) grid) #\X)
       (eql (gethash (list i (+ j 1)) grid) #\M)
       (eql (gethash (list i (+ j 2)) grid) #\A)
       (eql (gethash (list i (+ j 3)) grid) #\S)))

(defun xmas-south? (grid i j)
  (and (eql (gethash (list i j) grid) #\X)
       (eql (gethash (list (+ i 1) j) grid) #\M)
       (eql (gethash (list (+ i 2) j) grid) #\A)
       (eql (gethash (list (+ i 3) j) grid) #\S)))
#+#:excluded (xmas-south? (parse-grid) 0 0)

(defun xmas-west? (grid i j)
  (and (eql (gethash (list i j) grid) #\X)
       (eql (gethash (list i (- j 1)) grid) #\M)
       (eql (gethash (list i (- j 2)) grid) #\A)
       (eql (gethash (list i (- j 3)) grid) #\S)))
|#

(defun count-xmas (grid i j &aux (word "XMAS"))
  (looping
    (doseq ((di dj) *nhood-diagonal-list*)
      (count!
        (looping
          (doseqs ((ch word)
                   (mul (iota (length word))))
            (let ((i1 (+ (* di mul) i))
                  (j1 (+ (* dj mul) j)))
              (always! (eql (gethash (list i1 j1) grid) ch)))))))))
#+#:excluded (count-xmas (parse-grid) 0 0)

#+#:excluded (let1 grid (parse-grid)
               (looping
                 (dotimes (i 140)
                   (dotimes (j 140)
                     (sum! (count-xmas grid i j))))))

; M.S
; .A.
; M.S

(defun xmas-shape? (grid i j)
  (or
    (and
     (eql (gethash (list (- i 1) (- j 1)) grid) #\M)
     (eql (gethash (list (- i 1) (+ j 1)) grid) #\S)
     (eql (gethash (list i j) grid) #\A)
     (eql (gethash (list (+ i 1) (- j 1)) grid) #\M)
     (eql (gethash (list (+ i 1) (+ j 1)) grid) #\S))
    (and
     (eql (gethash (list (- i 1) (- j 1)) grid) #\M)
     (eql (gethash (list (- i 1) (+ j 1)) grid) #\M)
     (eql (gethash (list i j) grid) #\A)
     (eql (gethash (list (+ i 1) (- j 1)) grid) #\S)
     (eql (gethash (list (+ i 1) (+ j 1)) grid) #\S))
    (and
     (eql (gethash (list (- i 1) (- j 1)) grid) #\S)
     (eql (gethash (list (- i 1) (+ j 1)) grid) #\M)
     (eql (gethash (list i j) grid) #\A)
     (eql (gethash (list (+ i 1) (- j 1)) grid) #\S)
     (eql (gethash (list (+ i 1) (+ j 1)) grid) #\M))
    (and
     (eql (gethash (list (- i 1) (- j 1)) grid) #\S)
     (eql (gethash (list (- i 1) (+ j 1)) grid) #\S)
     (eql (gethash (list i j) grid) #\A)
     (eql (gethash (list (+ i 1) (- j 1)) grid) #\M)
     (eql (gethash (list (+ i 1) (+ j 1)) grid) #\M))))

#+#:excluded (let1 grid (parse-grid)
               (looping
                 (dotimes (i 140)
                   (dotimes (j 140)
                     (count! (xmas-shape? grid i j))))))
