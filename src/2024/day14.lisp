(defpackage :aoc/2024/14 #.cl-user::*aoc-use*)
(in-package :aoc/2024/14)

(defparameter *width* 101)
(defparameter *height* 103)

(defun parse-input (&optional (strings (uiop:read-file-lines #P"src/2024/day14.txt")))
  (prog1-let grid (make-hash-table :test 'equal)
    (dolist (s strings)
      (destructuring-bind (x y vx vy) (extract-integers s)
        (push (list vx vy) (gethash (list x y) grid))))))
#+#:excluded (parse-input)


(defun tick (curr)
  (prog1-let next (make-hash-table :test 'equal)
    (dohash ((x y) robots curr)
      (doseq ((vx vy) robots)
        (let ((x1 (mod (+ x vx) *width*))
              (y1 (mod (+ y vy) *height*)))
          (push (list vx vy) (gethash (list x1 y1) next)))))))

(defun safety-factor (grid &aux (middlex (floor *width* 2)) (middley (floor *height* 2)))
  (let ((first 0) (second 0) (third 0) (fourth 0))
    (dohash ((x y) robots grid)
      (cond ((or (= x middlex) (= y middley)) nil)
            ((and (< x middlex) (< y middley)) (incf first (length robots)))
            ((< y middley) (incf second (length robots)))
            ((< x middlex) (incf third (length robots)))
            (t (incf fourth (length robots)))))
    (* first second third fourth)))


(defun all-alone? (grid)
  (looping
    (dohashv (robots grid)
      (never! (> (length robots) 1)))))

(defun grid->string (grid)
  (looping
    (dotimes (y *height*)
      (dotimes (x *width*)
        (spr! (if (gethash (list x y) grid) "#" " ")))
      (spr! #\Newline))))
#+#:excluded (pr (grid->string (parse-input)))

(defun contains-xmas-tree? (grid)
  (and
    ;; I assume no two robots should be on the same location
    ;; when the xmas tree is visible...and I was lucky, because
    ;; that's actually what happens!
    (all-alone? grid)
    ;; Turns out there are multiple frames where robots are
    ;; all in distinct locations, but only one of them contains
    ;; the xmas three.  When solving this, I looked at all these
    ;; frames individually, and picked the correct one, but now
    ;; that we know how the tree looks like, we can hard-code its
    ;; shape here.
    (let1 s (grid->string grid)
      (and (search "    #    " s)
           (search "   ###   " s)
           (search "  #####  " s)
           (search " ####### " s)
           (search "#########" s)
           ))))


(define-solution (2024 14) (grid parse-input)
  (let1 part1 nil
    (dorange (elapsed 1 (* *width* *height*))
      (zapf grid [tick _])
      (if (= elapsed 100)
          (setf part1 (safety-factor grid)))
      (when (contains-xmas-tree? grid)
        (pr (grid->string grid))
        (return (values part1 elapsed))))))

(define-test (2024 14) (211692000 6587))
