(defpackage :aoc/2022/18 #.cl-user::*aoc-use*)
(in-package :aoc/2022/18)

(defun parse-cubes (&optional (file #P"src/2022/day18.txt"))
  (bnd1 (map (make-hash-table :test 'equal ))
    (dolist (s (uiop:read-file-lines file))
      (bnd1 (coord (mapcar #'parse-integer (split-sequence:split-sequence #\, s)))
        (setf (gethash coord map) t)))
    map))

(defun neighbors (coord)
  (destructuring-bind (x y z) coord
    (loop for dx in (list -1 0 1)
          for xn = (+ x dx)
          append (loop for dy in (list -1 0 1)
                       for yn = (+ y dy)
                       append (loop for dz in (list -1 0 1)
                                    for zn = (+ z dz)
                                    for next = (list xn yn zn)
                                    unless (/= (manhattan-distance coord next) 1)
                                    collect next )))))

(defun grid (map)
  (loop for (x y z) being the hash-keys of map
        minimizing x into x-min
        minimizing y into y-min
        minimizing z into z-min
        maximizing x into x-max
        maximizing y into y-max
        maximizing z into z-max
        finally (return (bnd1 (grid (make-hash-table :test 'equal))
                          (loop for x from (1- x-min) to (1+ x-max)
                                do (loop for y from (1- y-min) to (1+ y-max)
                                         do (loop for z from (1- z-min) to (1+ z-max)
                                                  for coord = (list x y z)
                                                  do (setf (gethash coord grid) t))))
                          grid))))

(defun trapped (map &aux (grid (grid map)))
  (bnd1 (trapped (make-hash-table :test 'equal))
    (loop for coord being the hash-keys of grid
          unless (or (gethash coord map)
                     (escapes map grid trapped coord))
          do (setf (gethash coord trapped) t))
    trapped))

(defun escapes (map grid trapped coord)
  (labels ((recur (coord visited)
             (cond ((gethash coord trapped) (return-from escapes nil))
                   ((not (gethash coord grid)) (return-from escapes t))
                   ((gethash coord map) nil)
                   (t (loop for next in (neighbors coord)
                            unless (member next visited :test 'equal)
                            do (recur next (cons coord visited)))))))
    (recur coord ())))


(defun external-grid (map)
  (loop for (x y z) being the hash-keys of map
        minimizing x into x-min
        minimizing y into y-min
        minimizing z into z-min
        maximizing x into x-max
        maximizing y into y-max
        maximizing z into z-max
        finally (return (bnd1 (grid (make-hash-table :test 'equal))
                          (loop for x from (1- x-min) to (1+ x-max)
                                do (loop for y from (1- y-min) to (1+ y-max)
                                         do (setf (gethash (list x y (1- z-min)) grid) t
                                                  (gethash (list x y (1+ z-max)) grid) t)))
                          (loop for x from (1- x-min) to (1+ x-max)
                                do (loop for z from (1- z-min) to (1+ z-max)
                                         do (setf (gethash (list x (1- y-min) z) grid) t
                                                  (gethash (list x (1+ y-max) z) grid) t)))
                          (loop for y from (1- y-min) to (1+ y-max)
                                do (loop for z from (1- z-min) to (1+ z-max)
                                         do (setf (gethash (list (1- x-min) y z) grid) t
                                                  (gethash (list (1+ x-max) y z) grid) t)))
                          grid))))

(defun count-exterior (map &aux (grid (grid map)))
  (let ((sum 0)
        (visited (make-hash-table :test 'equal)))
    (labels ((recur (coord)
               (cond ((not (gethash coord grid)) nil)
                     ((gethash coord visited) nil)
                     ((gethash coord map) nil)
                     (t (setf (gethash coord visited) t)
                        (dolist (next (neighbors coord))
                          (if (gethash next map)
                            (incf sum)
                            (recur next)))))))
      (loop for coord being the hash-keys of (external-grid map) do (recur coord)))
    sum))


#; Scratch

;; initially counted diagonals too
(let ((map (parse-cubes #P"scratch.txt")))
  (loop for coord being the hash-keys of map
        sum (loop for next in (neighbors coord)
                  count (not (gethash next map)))))

(let* ((map (parse-cubes #P"scratch.txt"))
       (exterior (exterior map)))
  (loop for coord being the hash-keys of map
        sum (loop for next in (neighbors coord)
                  count (and (not (gethash next map))
                             (gethash next exterior)))))
(- ** (* * 6))

(let ((map (parse-cubes #+#:excluded #P"scratch.txt")))
  (loop for coord being the hash-keys of map
        sum (loop for next in (neighbors coord)
                  count (not (gethash next map)))))
(neighbors (list 0 0 0))
(print-hash-table (grid (parse-cubes)))
(loop)

(exterior (parse-cubes #+#:excluded #P"scratch.txt"))
#+#:excluded (&optional (file #P"src/2022/day17.txt"))
#P"scratch.txt"

