(defpackage :aoc/2017/03 #.cl-user::*aoc-use*)
(in-package :aoc/2017/03)

(defstruct (spiral-gen (:constructor make-spiral-gen%))
  storage
  last
  last-pos
  dir)

(defun make-spiral-gen (&aux (storage (make-hash-table)))
  "Create a spiral sequence generator configured as below:

    ...   ...
    .1.   .C.
    ...   ..^

  `LAST`     = 1
  `LAST-POS` = #C(1 -1)
  `DIR`      = #C(0  1)

  This might look a little weird, but if you look at how SPIRAL-GEN-NEXT work,
  you will realize this is required so that:

  - `2` will be placed on `LAST-POS` + `DIR` (to the right of `1`)
  - `DIR` will be rotated counter-clockwise (facing the center)
  "
  (hash-table-insert storage #C(0 0) 1)
  (make-spiral-gen% :storage storage
                    :last 1
                    :last-pos #C(1 -1)
                    :dir #C(0 1)))

(defun spiral-gen-next (gen)
  "Generate successive values of the spiral sequnce, returning the last position
  used, as well as the last value.

  To recap, a SPIRAL-GEN has the following properties:

  - `STORAGE`: used to keep track of already visited cells
  - `LAST`: the last value used
  - `LAST-POS`: where `LAST` was last placed inside `STORAGE`
  - `DIR`: where to check for an empty cell

  The idea is _simple_:

  - if `LAST-POS` + `DIR` is empty, use that and rotate `DIR` counter-clockwise
  (facing the center of the spiral)
  - if `LAST-POS` + `DIR` is *not* empty, temporarily rotate `DIR` clockwise and
  use `LAST-POS` + `DIR-TEMP`

  Example:

  Suppose we just placed number 2 on the grid:

    ...   ...
    .12   .C<
    ...   ...

  `LAST`     = 2
  `LAST-POS` = #C( 1 0)
  `DIR`      = #C(-1 0)

  `LAST-POS` + `DIR` is already occupied, so we temporarily rotate `DIR`
  clockwise place `3` in `LAST-POS` + `DIR-TEMP`, and move on (leaving `DIR`
  unchanged).

    ..3   ..<
    .12   .C.
    ...   ...

  `LAST`      = 3
  `LAST-POST` = #C( 1 1)
  `DIR`       = #C(-1 0)

  This time, `LAST-POS` + `DIR` is not occupied, so we place `4` there, and then
  rotate `DIR` clockwise.

    .43   .v.
    .12   .C.
    ...   ...

  `LAST`      = 4
  `LAST-POST` = #C(0  1)
  `DIR`       = #C(0 -1)

  And so on, and so forth.
  "
  (with-slots (storage last last-pos dir) gen
    (let* ((next (1+ last))
           (next-pos-busy (gethash (+ last-pos dir) storage))
           (next-pos (+ last-pos (if next-pos-busy (complex-rotate-cw dir) dir)))
           (next-dir (if next-pos-busy dir (complex-rotate-ccw dir))))
      (setf (gethash next-pos storage) next
            (spiral-gen-last gen) next
            (spiral-gen-last-pos gen) next-pos
            (spiral-gen-dir gen) next-dir)
      (values (spiral-gen-last-pos gen)
              (spiral-gen-last gen)))))

(define-problem (2017 3) (data read-integer)
  (values
    (loop
      :with gen = (make-spiral-gen)
      :for (pos n) = (multiple-value-list (spiral-gen-next gen))
      :when (= n data) :return (manhattan-distance #C(0 0) pos))
    (loop
      :with gen = (make-spiral-gen)
      :with grid = (make-hash-table)
      :initially (hash-table-insert grid #C(0 0) 1)
      :for (pos) = (multiple-value-list (spiral-gen-next gen))
      :for value = (summation (adjacents pos :include-diagonal T)
                              :key (partial-1 #'gethash _ grid 0))
      :when (> value data) :return value
      :do (hash-table-insert grid pos value))))

(1am:test test-2017/03
  (multiple-value-bind (part1 part2) (problem-run)
    (1am:is (= 419 part1))
    (1am:is (= 295229 part2))))
