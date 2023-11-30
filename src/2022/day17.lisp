(defpackage :aoc/2022/17 #.cl-user::*aoc-use*)
(in-package :aoc/2022/17)

#+#:excluded (&optional (file #P"src/2022/day17.txt"))
#P"scratch.txt"

(defparameter *width* 7)

(defparameter *shapes* (list
                         ;;   |012345
                         ;; -4|..####
                         ;; -3|......
                         ;; -2|......
                         ;; -1|......
                         ;;   +------
                         (list
                           (list -4 2) (list -4 3) (list -4 4) (list -4 5))
                         ;;   |012345
                         ;; -6|...#
                         ;; -5|..###
                         ;; -4|...#
                         ;; -3|.....
                         ;; -2|.....
                         ;; -1|.....
                         ;;   +------
                         (list
                           (list -6 3)
                           (list -5 2) (list -5 3) (list -5 4)
                           (list -4 3))
                         ;;    012345
                         ;; -6|....#
                         ;; -5|....#
                         ;; -4|..###
                         ;; -3|.....
                         ;; -2|.....
                         ;; -1|.....
                         ;;   +------
                         (list
                           (list -6 4)
                           (list -5 4)
                           (list -4 2) (list -4 3) (list -4 4))
                         ;;    012345
                         ;; -7|..#
                         ;; -6|..#
                         ;; -5|..#
                         ;; -4|..#
                         ;; -3|...
                         ;; -2|...
                         ;; -1|...
                         ;;   +---
                         (list
                           (list -7 2)
                           (list -6 2)
                           (list -5 2)
                           (list -4 2))
                         ;;    012345
                         ;; -5|..##
                         ;; -4|..##
                         ;; -3|....
                         ;; -2|....
                         ;; -1|....
                         ;;   +----
                         (list
                           (list -5 2) (list -5 3)
                           (list -4 2) (list -4 3))
                         ))

(defun moves (&optional (file #P"src/2022/day17.txt"))
  (map 'vector
       (lambda (ch)
         (if (char= ch #\<)
           -1
           (if (char= ch #\>)
             1
             (error "UNEXPECTED"))))
       (mkstr (uiop:read-file-form file))))
#+#:excluded (moves #P"scratch.txt")


;; e.g. relative to absolute
(defun materialize (shape height)
  (loop for (row col) in shape collect (list (- height row) col)))
#+#:excluded (car *shapes*)
#+#:excluded (materialize (car *shapes*) 0)
#+#:excluded (materialize (car *shapes*) 1)

(defun move-horizontally (rock dir)
  (loop for (row col) in rock collect (list row (+ col dir))))

#+#:excluded (move-horizontally (car *shapes*) -1)
#+#:excluded (move-horizontally * -1)

(defun can-move-horizontally-p (rock dir tower)
  (loop for (row col) in rock for col1 = (+ col dir)
        never (or (= col1 -1)
                  (= col1 *width*)
                  (gethash (list row col1) tower))))
#+#:excluded (can-move-horizontally-p (car *shapes*) -1 (make-hash-table))
#+#:excluded (can-move-horizontally-p (move-horizontally (car *shapes*) -1) -1 (make-hash-table))
#+#:excluded (can-move-horizontally-p (move-horizontally (move-horizontally (car *shapes*) -1) -1) -1 (make-hash-table))
#+#:excluded (can-move-horizontally-p (car *shapes*) 1 (make-hash-table))
#+#:excluded (can-move-horizontally-p (move-horizontally (car *shapes*) 1) 1 (make-hash-table))

(defun move-down (shape)
  (loop for (row col) in shape collect (list (1- row) col)))

#+#:excluded (materialize (car *shapes*) 0)
#+#:excluded (move-down *)

(defun can-move-down-p (rock tower)
  (loop for (row col) in rock for row1 = (1- row)
        never (or (= row1 0)
                  (gethash (list row1 col) tower))))
#+#:excluded (can-move-down-p (move-down (car *shapes*)) '())
#+#:excluded (can-move-down-p (move-down (move-down (car *shapes*))) '())
#+#:excluded (can-move-down-p (move-down (move-down (move-down (car *shapes*)))) '((1)))

(defun place (rock tower)
  (loop for (row col) in rock
        maximize row
        do (setf (gethash (list row col) tower) #\# ))) ; XXX stupid vim indentation...

#; Scratch

(let ((moves (ncycle (copy-seq (moves #+#:excluded #P"scratch.txt"))))
      (tower (make-hash-table :test 'equal))
      (height 0))
  (flet ((next-move ()
           (prog1 (car moves)
             (setf moves (cdr moves)))))
    (loop repeat 2022
          for shape in (ncycle (copy-seq *shapes*))
          for rock = (materialize shape height)
          ; do (pr shape height)
          ; do (pr rock)
          ; do (break)
          do (loop for dir = (next-move) do
                   #+#:excluded (print-tower tower rock)
                   ; (break)
                   (if (can-move-horizontally-p rock dir tower)
                     (setf rock (move-horizontally rock dir)))
                   (if (can-move-down-p rock tower)
                     (setf rock (move-down rock))
                     (progn
                       (setf height (max (place rock tower) height))
                       #+#:excluded (print-tower tower rock)
                       (return))))))
  height)

(defun print-tower (tower rock)
  (bnd1 (max (loop for (row) in rock maximize row))
    (dorange (row max -1 -1)
      (dorange (col -1 (1+ *width*))
        (princ
          (if (= row 0)
            #\+
            (if (member col (list -1 *width*))
              #\|
              (aif (or (and (member (list row col) rock :test 'equal) #\@)
                       (gethash (list row col) tower))
                it
                #\.)))))
      (terpri))))

(let ((moves (ncycle (copy-seq (moves #+#:excluded #P"scratch.txt"))))
      (shapes (ncycle (copy-seq *shapes*)))
      (tower (make-hash-table :test 'equal)))
  (labels ((next-shape ()
             (prog1 (car shapes)
               (setf shapes (cdr shapes))))
           (next-move ()
             (prog1 (car moves)
               (setf moves (cdr moves))))
           (next (height)
             (bnd1 (rock (materialize (next-shape) height))
               (loop
                 (bnd1 (dir (next-move))
                   (if (can-move-horizontally-p rock dir tower)
                     (setf rock (move-horizontally rock dir)))
                   (if (can-move-down-p rock tower)
                     (setf rock (move-down rock))
                     (progn
                       (return (max (place rock tower) height)))))))))
    (loop repeat 2022 maximize (next height) into height finally (return height))))



(defstruct (state :conc-name)
  move shape height tower)

(defun place (rock tower &aux (tower (copy-hash-table tower)))
  (dolist (point rock)
    (setf (gethash point tower) #\# )) ; XXX vim...without the space, it fucks up the indentation
  tower)

(let ((shapes (coerce *shapes* 'vector))
      (moves (moves #+#:excluded #P"scratch.txt")))
  (flet ((next-step (state &aux (state-next (copy-state state)))
           (flet ((next-shape () (setf (shape state-next)
                                       (mod (1+ (shape state-next)) (length shapes))))
                  (next-move () (setf (move state-next)
                                      (mod (1+ (move state-next)) (length moves)))))

             (let* ((shape (next-shape))
                    (rock (materialize (aref shapes shape) (height state-next))))
               (loop
                 (let* ((move (next-move))
                        (dir (aref moves move)))
                   (if (can-move-horizontally-p rock dir (tower state))
                     (setf rock (move-horizontally rock dir)))
                   ; (print-tower (tower state) rock)
                   (if (can-move-down-p rock (tower state))
                     (setf rock (move-down rock))
                     (bnd1 (tower-next (place rock (tower state-next)))
                       (setf (height state-next) (tower-height tower-next)
                             (tower state-next) tower-next)
                       ; (print-tower (tower state) rock)
                         ; (break)
                       (return state-next)))))))))
      (floyd #'next-step (make-state :move -1 :shape -1 :height 0 :tower (make-hash-table :test 'equal))
             :key #'state-key :test 'equal)
    (loop with state = (make-state :move -1 :shape -1 :height 0 :tower (make-hash-table :test 'equal))
          repeat 2022 do (setf state (next-step state))
          finally (return (height state)))))

(let ((shapes (coerce *shapes* 'vector))
      (moves (moves #+#:excluded #P"scratch.txt")))
  (flet ((next-step (state &aux (state-next (copy-state state)))
           (flet ((next-shape () (setf (shape state-next)
                                       (mod (1+ (shape state-next)) (length shapes))))
                  (next-move () (setf (move state-next)
                                      (mod (1+ (move state-next)) (length moves)))))

             (let* ((shape (next-shape))
                    (rock (materialize (aref shapes shape) (height state-next))))
               (loop
                 (let* ((move (next-move))
                        (dir (aref moves move)))
                   (if (can-move-horizontally-p rock dir (tower state))
                     (setf rock (move-horizontally rock dir)))
                   ; (print-tower (tower state) rock)
                   (if (can-move-down-p rock (tower state))
                     (setf rock (move-down rock))
                     (bnd1 (tower-next (place rock (tower state-next)))
                       (setf (height state-next) (tower-height tower-next)
                             (tower state-next) tower-next)
                       ; (print-tower (tower state) rock)
                       ; (break)
                       (return state-next)))))))))
    (destructuring-bind (cycles-at cycle-size loop-state)
        (floyd #'next-step (make-state :move -1 :shape -1 :height 0 :tower (make-hash-table :test 'equal))
               :key #'state-key :test 'equal)
      (pr loop-state)
      (bnd1 (delta-height (loop with state = loop-state
                                repeat cycle-size do (setf state (next-step state))
                                finally (return (- (height state) (height loop-state)))))
        (pr delta-height)
        (let ((rem-cycles (floor (- 1000000000000 cycles-at) cycle-size))
              (rem-steps (rem (- 1000000000000 cycles-at) cycle-size)))
          (+
            (height loop-state)
            (* delta-height rem-cycles)
            (loop with state = loop-state
                  repeat rem-steps do (setf state (next-step state))
                  finally (return (- (height state) (height loop-state))))))))))

(defun tower-height (tower)
  (loop for (row col) being the hash-keys of tower maximize row))

(defun state-key (state)
  (list
    (shape state)
    (move state)
    (loop with height = (height state)
          for row from height downto (max (- height 10) 0)
          append (loop for col from 0 to *width* collect (if (gethash (list row col) (tower state)) #\# #\.)))))
