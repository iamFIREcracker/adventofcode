(defpackage :aoc/2021/22 #.cl-user::*aoc-use*)
(in-package :aoc/2021/22)


(defun parse-instructions (data)
  (mapcar #'parse-instruction data))
(defun parse-instruction (string)
  (cl-ppcre:register-groups-bind ((#'as-keyword state) (#'parse-integer x1 x2 y1 y2 z1 z2))
      ("(on|off) x=(-?\\d+)..(-?\\d+),y=(-?\\d+)..(-?\\d+),z=(-?\\d+)..(-?\\d+)" string)
    (list state (min x1 x2) (max x1 x2) (min y1 y2) (max y1 y2) (min z1 z2) (max z1 z2))))


(defun part1 (instructions &aux (cuboids (make-hash-table :test 'equal)))
  (loop for (state x1 x2 y1 y2 z1 z2) in instructions do
        (setf x1 (max x1 -50) x2 (min x2 50)
              y1 (max y1 -50) y2 (min y2 50)
              z1 (max z1 -50) z2 (min z2 50))
        (loop for x from x1 to x2 do
              (loop for y from y1 to y2 do
                    (loop for z from z1 to z2 do
                          (setf (gethash (list x y z) cuboids) state)))))
  (loop for state being the hash-values of cuboids count (eql state :on)))


(defun part2 (instructions &aux)
  (destructuring-bind (xx yy zz) (compress-coordinates instructions)
    (let ((grid (make-array (list (length xx) (length yy) (length zz))
                            :element-type 'bit
                            :initial-element 0)))
      (flet ((index-of (item vector &aux (start 0) (end (length vector)))
               (binary-search start end (partial-1 #'<=> (aref vector _) item))))
        (loop for (state x1 x2 y1 y2 z1 z2) in instructions
              for i-min = (index-of x1 xx) for i-max = (index-of (1+ x2) xx)
              for j-min = (index-of y1 yy) for j-max = (index-of (1+ y2) yy)
              for k-min = (index-of z1 zz) for k-max = (index-of (1+ z2) zz) do
              (loop for i from i-min below i-max do
                (loop for j from j-min below j-max do
                      (loop for k from k-min below k-max do
                            (setf (aref grid i j k) (ecase state (:on 1) (:off 0))))))))
      (loop with i-max = (1- (length xx)) and j-max = (1- (length yy)) and k-max = (1- (length zz))
            for i below i-max for x1 = (aref xx i) for x2 = (aref xx (1+ i)) sum
            (loop for j below j-max for y1 = (aref yy j) for y2 = (aref yy (1+ j)) sum
                  (loop for k below k-max for z1 = (aref zz k) for z2 = (aref zz (1+ k))
                        when (= (aref grid i j k) 1) sum (* (- x2 x1) (- y2 y1) (- z2 z1))))))))

(defun compress-coordinates (instructions)
  (flet ((unique&sorted (list &aux (list (remove-duplicates list)))
           (make-array (length list) :initial-contents (sort list #'<))))
    (loop for (_ x1 x2 y1 y2 z1 z2) in instructions
          collect x1 into xx collect (1+ x2) into xx
          collect y1 into yy collect (1+ y2) into yy
          collect z1 into zz collect (1+ z2) into zz
          finally (return (list (unique&sorted xx) (unique&sorted yy) (unique&sorted zz))))))


(define-solution (2021 22) (instructions parse-instructions)
  (values (part1 instructions) (part2 instructions)))

(define-test (2021 22) (570915 1268313839428137))
