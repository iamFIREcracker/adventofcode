(defpackage :aoc/2020/20 #.cl-user::*aoc-use*)
(in-package :aoc/2020/20)

(defun parse-tile-id (string)
  (cl-ppcre:register-groups-bind ((#'parse-integer id))
      ("Tile (\\d+):" string)
    id))

(defun parse-tile (data)
  (cons (parse-tile-id (first data))
        (rest data)))

(defun id (tile) (car tile))
(defun data (tile) (cdr tile))
(defun data-top (tile) (first (data tile)))
(defun data-bottom (tile) (first (last (data tile))))
(defun data-left (tile) (data-bottom (rotate tile)))
(defun data-right (tile) (data-top (rotate tile)))

(defun parse-tiles (data)
  (let (groups current)
    (dolist (string (append data '("")))
      (if (string= string "")
        (setf groups (cons (reverse current) groups) current nil)
        (setf current (cons string current))))
    (mapcar #'parse-tile groups)))

(defun rotate (tile)
  (cons
    (id tile)
    (reverse
      (apply #'map 'list (lambda (&rest args) (format nil "~{~a~}" args))
             (data tile)))))

(defun flip (tile &aux flipped)
  (dolist (x (data tile) (cons (id tile) flipped))
    (push x flipped)))

(defun transformations (tile)
  (loop repeat 4
        for x = tile then (rotate x)
        collect x collect (flip x)))

(defun index-key (&key top left) (format nil "~a~a" top left))

(defun create-tiles-index (tiles &aux (index (make-hash-table :test 'equal)))
  (flet ((index-set (k v)
           (multiple-value-bind (existing existsp)
               (gethash k index)
             (if existsp
               (setf (gethash k index) (cons v existing))
               (setf (gethash k index) (list v))))))
    (dolist (tile tiles index)
      (dolist (ttile (transformations tile))
        (let ((top (data-top ttile))
              (left (data-left ttile)))
          (index-set (index-key) ttile)
          (index-set (index-key :top top) ttile)
          (index-set (index-key :left left) ttile)
          (index-set (index-key :top top :left left) ttile))))))

(defun reorder-tiles (tiles &aux (size (isqrt (length tiles))))
  (let ((index (create-tiles-index tiles))
        (image (make-array `(,size ,size) :initial-element nil)))
    (labels ((recur (used row col)
               (cond
                 ((not (array-in-bounds-p image row col))
                  (return-from reorder-tiles image))
                 (t
                   (let ((top (and (> row 0) (data-bottom (aref image (1- row) col))))
                         (left (and (> col 0) (data-right (aref image row (1- col))))))
                     (loop with key = (index-key :top top :left left)
                           for tile in (gethash key index)
                           unless (member (id tile) used) do
                           (setf (aref image row col) tile)
                           (if (array-in-bounds-p image row (1+ col))
                             (recur (cons (id tile) used) row (1+ col))
                             (recur (cons (id tile) used) (1+ row) 0))
                           (setf (aref image row col) nil)))))))
      (recur nil 0 0))))

(setf *sea-monster* (list
                      "..................#."
                      "#....##....##....###"
                      ".#..#..#..#..#..#..."))

(defun create-image (tiles)
  (let ((image (make-array '(96 96))))
    (loop for tile-row below 12 do
          (loop for tile-col below 12
                for tile = (aref tiles tile-row tile-col) do
                (loop for row from 0 for string in (subseq (data tile) 1 9)
                      for image-row = (+ (* tile-row 8) row) do
                      (loop for col from 0 for ch across (subseq string 1 9)
                            for image-col = (+ (* tile-col 8) col) do
                            (setf (aref image image-row image-col) ch)))))
    image))

(defun crop (image top left height width)
  (loop for row from top below (+ top height) collect
        (loop for col from left below (+ left width)
              collect (aref image row col) into characters
              finally (return (coerce characters 'string)))))

(defun tiles-match-p (tile1 tile2)
  (every (lambda (x1 x2) (cl-ppcre:scan x1 x2)) tile1 tile2))

(defun count-matches (tile image)
  (let ((height (length (data tile)))
        (width (length (first (data tile)))))
    (loop with (image-height image-width) = (array-dimensions image)
          for row from 0 below (- image-height height) sum
          (loop for col from 0 below (- image-width width)
                count (tiles-match-p (data tile)
                                     (crop image row col height width))))))

(define-solution (2020 20) (tiles parse-tiles)
  (let* ((tiles (reorder-tiles tiles))
         (image (create-image tiles)))
    (values
      (* (id (aref tiles 0 0))
         (id (aref tiles 0 11))
         (id (aref tiles 11 0))
         (id (aref tiles 11 11)))
      (- (loop for index below (* 96 96)
               count (char= (row-major-aref image index) #\#))
         (* 15
            (loop for tile in (transformations (cons 666 *sea-monster*))
                  for matches = (count-matches tile image)
                  when (> matches 0) return matches))))))

(define-test (2020 20) (63187742854073 2152))
