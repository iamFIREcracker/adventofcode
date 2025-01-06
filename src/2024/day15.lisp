(defpackage :aoc/2024/15 #.cl-user::*aoc-use*)
(in-package :aoc/2024/15)

(defstruct (warehouse (:conc-name nil))
  grid robot)

(defun parse-input (&optional (strings (uiop:read-file-lines #P"src/2024/day15.txt")))
  (destructuring-bind (warehouse moves) (split-sequence:split-sequence "" strings :test 'string=)
    (list (prog1-let x (make-warehouse :grid (make-hash-table :test 'equal))
            (doeseq (i s warehouse)
              (doeseq (j ch s)
                (setf (gethash (list i j) ~x.grid) ch)
                (if (char= ch #\@) (setf ~x.robot (list i j))))))
          (looping
            (dolist (s moves)
              (doseq (ch s)
                (collect! (ecase ch
                            (#\^ (list -1 0))
                            (#\> (list 0 1))
                            (#\v (list 1 0))
                            (#\< (list 0 -1))))))))))


(defun moving-vertically? (dir) (/= ~dir.car 0))
(defun moving-left? (dir) (< ~dir.cadr 0))
(defun moving-right? (dir) (> ~dir.cadr 0))

(defun move-straight (pos dir)
  (list (+ ~pos.car ~dir.car) (+ ~pos.cadr ~dir.cadr)))
(defun right-of (pos) (list ~pos.car (1+ ~pos.cadr)))
(defun left-of (pos) (list ~pos.car (1- ~pos.cadr)))


(defun can-move? (warehouse dir)
  (recursively ((pos ~warehouse.robot))
    (let1 next (move-straight pos dir)
      (ecase (gethash next ~warehouse.grid)
        (#\. t)
        (#\# nil)
        (#\O (recur next))
        (#\[ (if (moving-vertically? dir)
                 (and (recur next)
                      (recur (right-of next)))
                 (recur next)))
        (#\] (if (moving-vertically? dir)
                 (and (recur next)
                      (recur (left-of next)))
                 (recur next)))))))

(defun move-robot! (warehouse dir)
  (assert (can-move? warehouse dir))
  (flet ((swap (pos1 pos2)
           (rotatef (gethash pos1 ~warehouse.grid)
                    (gethash pos2 ~warehouse.grid))))
    (recursively ((pos ~warehouse.robot))
      (let1 next (move-straight pos dir)
        (ecase (gethash next ~warehouse.grid)
          (#\. (swap pos next))
          (#\O (recur next)                ; push small box out
               (swap pos next))            ; move into the empty space
          (#\[ (cond
                 ((moving-vertically? dir)
                   (recur next)            ; push first half-box out
                   (recur (right-of next)) ; push second half-box out
                   (swap pos next)         ; move into the empty space
                   )
                 (t
                   (recur next)            ; make box out
                   (swap pos next))))      ; move into the empty space
          (#\] (cond
                 ((moving-vertically? dir)
                   (recur next)            ; push first half-box out
                   (recur (left-of next))  ; push second half-box out
                   (swap pos next)         ; move into the empty space
                   )
                 (t
                   (recur next)            ; make room
                   (swap pos next)         ; move into the empty space
                   ))))
        next))))

(defun push-boxes (&optional (input (parse-input)))
  (destructuring-bind (w moves) input
    (doseq (dir moves)
      (when (can-move? w dir)
        (setf ~w.robot (move-robot! w dir))))
    w))


(defun result (warehouse)
  (looping
    (dohash ((i j) ch ~warehouse.grid)
      (when (find ch "O[")
        (sum! (+ (* i 100) j))))))


(defun massage-input (&optional (strings (uiop:read-file-lines #P"src/2024/day15.txt")))
  (mapcar (fn (s)
            (~> s
              (cl-ppcre:regex-replace-all "#" ~ "##")
              (cl-ppcre:regex-replace-all "\\." ~ "..")
              (cl-ppcre:regex-replace-all "O" ~ "[]")
              (cl-ppcre:regex-replace-all "@" ~ "@.")))
          strings))
#+#:excluded (massage-input)


(define-solution (2024 15) (strings)
  (values (~> strings parse-input push-boxes result)
          (~> strings massage-input parse-input push-boxes result)))
#+#:excluded (time (solution-run))

(define-test (2024 15) (1406392 1429013))
