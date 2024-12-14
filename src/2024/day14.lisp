(defpackage :aoc/2024/14 #.cl-user::*aoc-use*)
(in-package :aoc/2024/14)

#;
(sb-ext:gc :full t)
(defun parse-input (&optional (strings (uiop:read-file-lines #P"src/2024/day14.txt")))
  (prog1-let (grid (make-hash-table :test 'equal))
    (dolist (s strings)
      (destructuring-bind (x y vx vy) (extract-integers s)
        (push (list vx vy) (gethash (list x y) grid))))))
#+#:excluded (parse-input)

(defun tick (curr)
  (prog1-let (next (make-hash-table :test 'equal))
    (dohash ((x y) robots curr)
      (doseq ((vx vy) robots)
        (let ((x1 (mod (+ x vx) 101))
              (y1 (mod (+ y vy) 103)))
          (push (list vx vy) (gethash (list x1 y1) next)))))))

(defun safety-factor (grid &aux (middlex (floor 101 2)) (middley (floor 103 2)))
  (let ((first 0) (second 0) (third 0) (fourth 0))
    (dohash ((x y) robots grid)
      (cond ((or (= x middlex) (= y middley)) nil)
            ((and (< x middlex) (< y middley)) (incf first (length robots)))
            ((< y middley) (incf second (length robots)))
            ((< x middlex) (incf third (length robots)))
            (t (incf fourth (length robots)))))
    (* first second third fourth)))

(let1 grid (parse-input)
  (repeat 100
    (zapf grid [tick _]))
  (safety-factor grid))

(defun display (grid)
  (dotimes (y 103)
    (dotimes (x 101)
      (aif (gethash (list x y) grid)
           (pr (length it))
           (pr ".")))
    (terpri)
    (finish-output)))

(let1 grid (parse-input)
  (prog1-let (elapsed 0)
    (repeat 100
      (while (looping
               (dohashv (robots grid)
                 (thereis! (> (length robots) 1))))
        (zapf grid [tick _])
        (incf elapsed))
      (display grid)
      (dbg elapsed)
      (sleep 1)
      (zapf grid [tick _])
      (incf elapsed))))
1286 too low
