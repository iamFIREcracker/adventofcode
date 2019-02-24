(defpackage :aoc/2017/03 #.cl-user::*aoc-use*)
(in-package :aoc/2017/03)

(defstruct (spiral-gen (:constructor make-spiral-gen%))
  storage
  last
  last-pos
  dir)

(defun make-spiral-gen (&aux (storage (make-hash-table)))
  (setf (gethash #C(0 0) storage) 1)
  (make-spiral-gen% :storage storage
                    :last 1
                    :last-pos #C(1 -1)
                    :dir #C(0 1)))

(defun rotate-cw (c)
  (complex (imagpart c) (- (realpart c))))

(defun rotate-ccw (c)
  (complex (- (imagpart c)) (realpart c)))

(defun spiral-gen-next (gen)
  "To recap, a `SPIRAL-GEN` has the following properties:

  - `STORAGE`: used to keep track of already visited cells
  - `LAST`: the last value used
  - `LAST-POS`: where `LAST` was last placed inside `STORAGE`
  - `DIR`: where to check for an empty cell
  

  XXX

   ...
   .12
   ..."
  (let* ((storage (spiral-gen-storage gen)) ; there should be a simple way to do stuff like this -- similar to `WITH-SLOTS`
         (last (spiral-gen-last gen))
         (last-pos (spiral-gen-last-pos gen))
         (dir (spiral-gen-dir gen))
         (next (1+ last))
         (next-pos-busy (gethash (+ last-pos dir) storage))
         (next-pos (+ last-pos (if next-pos-busy (rotate-cw dir) dir)))
         (next-dir (if next-pos-busy dir (rotate-ccw dir))))
    (setf (gethash next-pos storage) next
          (spiral-gen-last gen) next
          (spiral-gen-last-pos gen) next-pos
          (spiral-gen-dir gen) next-dir))
  (values (spiral-gen-last-pos gen)
          (spiral-gen-last gen)))

(defun adjacents (p)
  (gathering
    (doirange (y -1 1)
      (doirange (x -1 1)
        (unless (= 0 x y)
          (gather (+ p (complex x y))))))))

(define-problem (2017 3) (data read-integer)
  (let ((part2-grid (make-hash-table)))
    (setf (gethash #C(0 0) part2-grid) 1)
    (values
      (loop
        :with gen = (make-spiral-gen)
        :for (pos n) = (multiple-value-list (spiral-gen-next gen))
        :do (when (= n data)
              (return (manhattan-distance #C(0 0) pos))))
      (loop
        :with gen = (make-spiral-gen)
        :for (pos) = (multiple-value-list (spiral-gen-next gen))
        :for value = (summation (adjacents pos) :key (curry #'gethash >< part2-grid 0))
        :do (when (> value data)
              (return value))
        :do (setf (gethash pos part2-grid) value)))))

(1am:test test-2017/03
  (multiple-value-bind (part1 part2) (problem-run)
    (1am:is (= 419 part1))
    (1am:is (= 295229 part2))))
