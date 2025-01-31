(defpackage :aoc/2022/24 #.cl-user::*aoc-use*)
(in-package :aoc/2022/24)

(defstruct (valley (:conc-name))
  blizzards rows cols)
(defun valley (&optional (file #P"src/2022/day24.txt"))
  (let* ((lines (uiop:read-file-lines file))
         (rows (- (length lines) 2))
         (cols (- (length (car lines)) 2))
         (blizzards (make-hash-table :test 'equal)))
    (loop for row from 0 for s in (subseq lines 1 (1+ rows)) do
          (loop for col from 0 for ch across (subseq s 1 (1+ cols))
                unless (char= ch #\.) do (setf (gethash (list row col) blizzards)
                                               (parse-direction ch))))
    (make-valley :blizzards blizzards
                 :rows rows
                 :cols cols)))

(defparameter *directions* '((-1 0) (1 0) (0 -1) (0 1)))
(defun parse-direction (ch) (nth (position ch "^v<>") *directions*))
#+#:excluded (parse-direction #\^)

(defun/memo blizzards-at (valley time)
  (let* ((blizzards (make-hash-table :test 'equal)))
    (loop for (row col) being the hash-keys of (blizzards valley) using (hash-value dir)
          for (delta-row delta-col) = (mapcar [* _ time] dir)
          for pos-next = (list (mod (+ row delta-row) (rows valley))
                               (mod (+ col delta-col) (cols valley)))
          do
          (setf (gethash pos-next blizzards) t))
    blizzards))
#+#:excluded (blizzards-at (valley) 0)
#+#:excluded (blizzards-at (valley) 1)
#+#:excluded (blizzards-at (valley) 2)

(defun start-pos () (list -1 0))
(defun target-pos (valley) (list (rows valley) (1- (cols valley))))
#+#:excluded (target-pos (valley))
#+#:excluded (target-pos (valley #P"scratch.txt"))

(defun stay-put (blizzards pos)
  (unless (gethash pos blizzards)
    pos))
#+#:excluded (stay-put (blizzards-at (valley) 0) (start-pos))
#+#:excluded (stay-put (blizzards-at (valley) 0) (list 0 0))
#+#:excluded (stay-put (blizzards-at (valley) 1) (list 0 0))

(defun out-of-valley-p (valley pos)
  (not (or (and (= (car pos) -1)
                (= (cadr pos) 0))
           (and (= (car pos) (rows valley))
                (= (cadr pos) (1- (cols valley))))
           (and (<= 0 (car pos) (1- (rows valley)))
                (<= 0 (cadr pos) (1- (cols valley)))))))
#+#:excluded (out-of-valley-p (valley) (start-pos))
#+#:excluded (out-of-valley-p (valley) (target-pos (valley)))
#+#:excluded (out-of-valley-p (valley) (list 0 0))

(defun move (valley blizzards pos)
  (loop for delta in *directions* for pos-next = (mapcar #'+ pos delta)
        unless (or (out-of-valley-p valley pos-next)
                   (gethash pos-next blizzards)) collect pos-next))
#+#:excluded (move (valley) (blizzards-at (valley) 0) (start-pos))
#+#:excluded (move (valley) (blizzards-at (valley) 5) (start-pos))

(defun next (valley time pos)
  (bnd1 blizzards (blizzards-at valley time)
    (nconc
      (aand (stay-put blizzards pos) (list it))
      (move valley blizzards pos))))
#+#:excluded (next (valley) 0 (list -1 0))


(defstruct (state (:conc-name))
  pos elapsed)

(defun init-state ()
  (make-state :pos (start-pos)
              :elapsed 0))
#+#:excluded (init-state)

(defun elapsed-next (valley elapsed)
  (bnd1 period (lcm (rows valley) (cols valley))
    (mod (1+ elapsed) period)))

(defun traverse (valley start-pos target-pos elapsed)
  (1- (search-cost (a* (make-state :pos start-pos
                                   :elapsed elapsed)
                       :goalp [equal (pos _) target-pos]
                       :neighbors (search-unit-cost
                                    (lambda (s)
                                      ; (bnd1 elapsed-next (elapsed-next valley (elapsed s))
                                      (bnd1 elapsed-next (1+ (elapsed s))
                                        (loop for n in (next valley (elapsed s) (pos s))
                                              collect (make-state :pos n
                                                                  :elapsed elapsed-next)))))
                       :heuristic [manhattan-distance (pos _) target-pos]
                       :test 'equalp))))


#; Scratch
248 too high
247 off by one -,-
(valley)
(traverse (valley) (start-pos) (target-pos (valley)) 0)
(time
  (let ((valley (valley))
        (elapsed 0))
    (pr (incf elapsed (traverse valley (start-pos) (target-pos valley) elapsed)))
    (pr (incf elapsed (traverse valley (target-pos valley) (start-pos) elapsed)))
    (pr (incf elapsed (traverse valley (start-pos) (target-pos valley) elapsed)))))


(loop for r below (array-dimension (grid) 0) do
      (loop for c below (array-dimension (grid) 1) do
            (princ (aref (grid) r c)))
      (terpri))
(&optional (file #P"src/2022/day24.txt"))
#P"scratch.txt"
(sb-ext:gc :full t)
;; Forgot to add start/end to the list of cells _permitted_
