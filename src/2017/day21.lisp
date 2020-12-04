(defpackage :aoc/2017/21 #.cl-user::*aoc-use*)
(in-package :aoc/2017/21)

(defun size (source)
  (multiple-value-bind (d r)
      (truncate (sqrt (length source)))
    (unless (zerop r)
      (error "~s is not square" source))
    d))

(defun indices-flip-v (size)
  (if (= size 2)
    `((1 0) (1 1)
      (0 0) (0 1))
    `((2 0) (2 1) (2 2)
      (1 0) (1 1) (1 2)
      (0 0) (0 1) (0 2))))

(defun indices-rotate-cw (size)
  (if (= size 2)
    `((1 0) (0 0)
      (1 1) (0 1))
    `((2 0) (1 0) (0 0)
      (2 1) (1 1) (0 1)
      (2 2) (1 2) (0 2))))

(defun indices-square (top left size)
  (gathering
    (dorange (i top (+ top size))
      (dorange (j left (+ left size))
        (gather (list i j))))))

(defun select-pixels (source indices &aux (size (size source)))
  (loop
    :for (i j) :in indices
    :for flattened = (+ (* i size) j)
    :collect (aref source flattened) :into pixels
    :finally (return (apply #'mkstr pixels))))

(defun generate-patterns (original &aux (size (size original)))
  "Generate all the transformations of `original` by rotating it, flipping
  it, or rotate and flipping it.

  It turns out all the possible transformations can be generated by first
  rotating (either clockwise, or counter-clockwise) and then flipping the result
  (again, either vertically, or horizontally); so:

    - original
    - original | flip
    - original | cw
    - original | cw | flip
    - original | cw | cw
    - original | cw | cw | flip
    - original | cw | cw | cw
    - original | cw | cw | cw | flip
  "
  (let* ((rotated-cw1 (select-pixels original (indices-rotate-cw size)))
         (rotated-cw2 (select-pixels rotated-cw1 (indices-rotate-cw size)))
         (rotated-cw3 (select-pixels rotated-cw2 (indices-rotate-cw size))))
    (list original
          (select-pixels original (indices-flip-v size))
          rotated-cw1
          (select-pixels rotated-cw1 (indices-flip-v size))
          rotated-cw2
          (select-pixels rotated-cw2 (indices-flip-v size))
          rotated-cw3
          (select-pixels rotated-cw3 (indices-flip-v size)))))

(defun parse-rules (x &aux (rules (make-hash-table :test 'equal)))
  (flet ((parse-rule (s &aux (splits (split-sequence:split-sequence #\Space s)))
           (let ((from (remove #\/ (first splits)))
                 (to (split-sequence:split-sequence #\/ (third splits))))
             (list from to))))
    (loop
      :for (from to) :in (mapcar #'parse-rule x)
      :do (loop
            :for p :in (generate-patterns from)
            :do (hash-table-insert rules p to)))
    rules))

(defun break-up (pixels &aux (size (size pixels)))
  (let ((sub-size (if (dividesp 2 size) 2 3)))
    (gathering
      (dorange (i 0 size sub-size)
        (dorange (j 0 size sub-size)
          (gather (select-pixels pixels (indices-square i j sub-size))))))))

(defun combine (squares &aux (size (size squares)))
  "Combines `squares` together into a bigger pixel grid.

  Given the following list of squares (outputs of the rules book):

    (((##.)
      (#..)
      (...))
     ((##.)
      (#..)
      (...))
     ((##.)
      (#.#)
      (.##))
     ((##.)
      (#.#)
      (.##)))

  COMBINE will generate the following grid, serialized into a string -- without
  pipes and dashes:

    ##.|##.
    #..|#..
    ...|...
    ---+----
    ##.|##.
    #.#|#.#
    .##|.##

  How? `size` input squares are required to generate a row of tiles, and since
  each square is a list of lists containing the configuration of pixel for each
  square row, we can use MAPCAR with MKSTR incrementally build 1 x `size` new
  tile; finally, all the 1 x `size` tiles will be joined together into the final
  string representing the new pixel grid

  Example:

    (((##.)
      (#..)
      (...))
     ((##.)
      (#..)
      (...))
     ((##.)
      (#.#)
      (.##))
     ((##.)
      (#.#)
      (.##)))

  For the first 1 x `size` tile, we will use the first two squares

    (((##.)
      (#..)
      (...))
     ((##.)
      (#..)
      (...))

  Passing these to MAPCAR and MKSTR we would get:

    ##.##.#..#........

  The second set of squares would instead generate the following string:

    ##.##.#.##.#.##.##

  You combine them together with MKSTR and you get

    ##.##.#..#........##.##.#.##.#.##.##

  _painting_
  "
  (loop
    :for start = 0 :then end
    :for end = (+ start size)
    :while (< start (length squares))
    :for line-squares = (subseq squares start end)
    :collect (apply #'mkstr (apply #'mapcar #'mkstr line-squares)) :into lines
    :finally (return (apply #'mkstr lines))))

(define-solution (2017 21) (data parse-rules)
  (flet ((pixels-on (pixels)
           (count #\# pixels)))
    (loop
      :with pixels = ".#...####"
      :with part1
      :for iteration :from 1 :upto 18
      :for sub-grids = (break-up pixels)
      :for expansions = (mapcar (partial-1 #'gethash _ data) sub-grids)
      :do (setf pixels (combine expansions))
      :when (= iteration 5) :do (setf part1 (pixels-on pixels))
      :finally (return (values part1 (pixels-on pixels))))))

(define-test (2017 21) (188 2758764))
