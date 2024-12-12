(defpackage :aoc/2024/12 #.cl-user::*aoc-use*)
(in-package :aoc/2024/12)

#;

(defun parse-input (&optional (strings (uiop:read-file-lines #P"src/2024/day12.txt")))
  (prog1-let (map (make-hash-table :test 'equal))
    (doseq ((i s) (enumerate strings))
      (doseq ((j ch) (enumerate s))
        (setf (gethash (list i j) map) ch)))))
#+#:excluded (parse-input)

#+#:overall-area-and-perimeter-instead-of-by-region (let1 map (parse-input)
                                                      (let ((areas (make-hash-table))
                                                            (perimeters (make-hash-table)))
                                                        (dohash ((i j) type map)
                                                          (incf (gethash type areas 0))
                                                          (doseq ((di dj) '((-1 0) (0 1) (1 0) (0 -1)))
                                                            (when-not (eql (gethash (list (+ i di) (+ j dj)) map) type)
                                                              (incf (gethash type perimeters 0)))))
                                                        (looping
                                                          (dohash (type area areas)
                                                            (let1 perimeter (gethash type perimeters)
                                                              (dbgl type area perimeter (* area perimeter))
                                                              (sum! (* area perimeter)))))))
13799638 too high

(let1 map (parse-input)
  (let1 seen (make-hash-table :test 'equal)
    (let1 regions (looping
                    (dohash ((i j) type map)
                      (let ((area 0) (perimeter 0)
                            (frontier (list (list i j))))
                        (while frontier
                          (destructuring-bind (i j) (pop frontier)
                            (unless (gethash (list i j) seen)
                              (setf (gethash (list i j) seen) t)
                              (incf area)
                              (doseq ((di dj) '((-1 0) (0 1) (1 0) (0 -1)))
                                (let1 next (list (+ i di) (+ j dj))
                                  (if (eql (gethash next map) type)
                                      (push next frontier)
                                      (incf perimeter)))))))
                        (when (and (plusp area) (plusp perimeter))
                          (collect! (list type area perimeter))))))

      (looping
        (doseq ((type area perimeter) regions)
          ; (dbgl type area perimeter (* area perimeter))
          (sum! (* area perimeter)))))))

1431440!

(defun rotate-cw (dir) (list (second dir) (- (first dir))))
(defun rotate-ccw (dir) (list (- (second dir)) (first dir)))

(defun sides (perimeter &aux (perimeter (copy-seq perimeter)))
  ; (dbgl perimeter)
  (looping
    (while perimeter
      (destructuring-bind ((i j) dir) (pop perimeter)
        (count! 1)
        (doseq ((di1 dj1) (list (rotate-cw dir) (rotate-ccw dir)))
          (let1 next (list (+ i di1) (+ j dj1))
            ; (dbgl (list i j) dir di1 dj1 next)

            (while (find (list next dir) perimeter :test 'equal)
              ; (break)
              (zapf perimeter [remove (list next dir) _ :test 'equal])
              (incf (car next) di1)
              (incf (cadr next) dj1))))
        ))))
#+#:excluded (sides '(((0 3) (1 0)) ((0 3) (0 1)) ((0 3) (-1 0)) ((0 2) (1 0))
                      ((0 2) (-1 0)) ((0 1) (1 0)) ((0 1) (-1 0)) ((0 0) (0 -1))
                      ((0 0) (1 0)) ((0 0) (-1 0))))

(let1 map (parse-input)
  (let1 seen (make-hash-table :test 'equal)
    (let1 regions (looping
                    (dohash ((i j) type map)
                      (let ((area 0) (perimeter nil)
                            (frontier (list (list i j))))
                        (while frontier
                          (destructuring-bind (i j) (pop frontier)
                            (unless (gethash (list i j) seen)
                              (setf (gethash (list i j) seen) t)
                              (incf area)
                              (doseq ((di dj) '((-1 0) (0 1) (1 0) (0 -1)))
                                (let ((next (list (+ i di) (+ j dj))))
                                  (if (eql (gethash next map) type)
                                      (push next frontier)
                                      (push (list (list i j) (list di dj)) perimeter)))))))
                        (when (and (plusp area) (length perimeter))
                          (collect! (list type area perimeter))))))

      (looping
        (doseq ((type area perimeter) regions)
          (let1 sides (sides perimeter)
            ; (dbgl type area perimeter sides)
            (sum! (* area sides))))))))
1411840 too high
869070!
