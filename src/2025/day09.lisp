(defpackage :aoc/2025/09 #.cl-user::*aoc-use*)
(in-package :aoc/2025/09)

(defun read-red-tiles (&optional (strings (uiop:read-file-lines #P"src/2025/day09.txt")))
  (mapcar #'extract-integers strings))


(defun area (x1 y1 x2 y2)
  (* (1+ (- (max x1 x2) (min x1 x2)))
     (1+ (- (max y1 y2) (min y1 y2)))))

(defun part1 (&optional (reds (read-red-tiles)))
  (looping
    (dosublists (((x1 y1) . reds2) reds)
      (doseq ((x2 y2) reds2)
        (maximize! (area x1 y1 x2 y2))))))


(defun compress-coordinates (reds)
  (flet ((unique&sorted (list &aux (list (remove-duplicates list)))
           (make-array (length list) :initial-contents (sort list #'<))))
    (loop for (x y) in reds
          collect x into xx
          collect y into yy
          finally (return (list (unique&sorted xx) (unique&sorted yy))))))


(defun edge? (vv x y)
  (looping
    (loop
      for (x1 y1) in (cons (last-elt vv) vv)
      for (x2 y2) in vv
      do (cond ((= x x1 x2) (thereis! (<= (min y1 y2) y (max y1 y2))))
               ((= y y1 y2) (thereis! (<= (min x1 x2) x (max x1 x2))))))))


;; We cast a horizontal ray to the right and count how many
;; vertical edges of the polygon it crosses.  An odd count means the point
;; is inside.
;;
;; Details / caveats:
;;
;; - The polygon is assumed to be axis-aligned, so edges are either vertical
;;   or horizontal.  Only vertical edges can intersect a horizontal ray.
;;
;; - Horizontal edges are ignored entirely since they contribute no crossings
;;   (the ray is parallel to them).
;;
;; - For each vertical edge, we check whether Y lies within that edge’s
;;   Y-span, using a half-open interval [ymin, ymax):
;;       (<= ymin y)  and  (< y ymax)
;;   This avoids double-counting when the ray passes exactly through a
;;   vertex shared by two vertical edges: only the “lower” edge includes
;;   the vertex; the “upper” edge does not.
;;
;; - We also require (< x x1) so that only edges strictly to the right of
;;   the test point count as intersections.  Note: equality here is handled
;;   elsewhere (EDGE?), so we deliberately do not treat x = x1 as a crossing.
;;
;; The final parity of the crossing count (odd/even) determines the result.
(defun inside? (vv x y)
  (oddp
    (looping
      (loop
        for (x1 y1) in (cons (last-elt vv) vv)
        for (x2 y2) in vv
        do (when (= x1 x2)
             (count! (and (<= (min y1 y2) y)
                          (<  y (max y1 y2))
                          (< x x1))))))))

(defun part2 (&optional (reds (read-red-tiles)))
  (destructuring-bind (xx yy) (compress-coordinates reds)
    (let1 grid (make-array (list (length yy) (length xx))
                           :element-type 'boolean
                           :initial-element nil)
      (doseq ((i y) (enumerate yy))
        (doseq ((j x) (enumerate xx))
          (setf (aref grid i j) (and (or (edge? reds x y)
                                         (inside? reds x y))
                                     t))))
      (flet ((index-of (item vector)
               (binary-search 0 (length vector) (partial-1 #'<=> (aref vector _) item))))
        (looping
          (dosublists (((x1 y1) . reds2) reds)
            (doseq ((x2 y2) reds2)
              (let1 valid (looping
                            (dorangei (i (index-of (min y1 y2) yy) (index-of (max y1 y2) yy))
                              (dorangei (j (index-of (min x1 x2) xx) (index-of (max x1 x2) xx))
                                (always! (aref grid i j)))))
                (when valid
                  (maximize! (area x1 y1 x2 y2)))))))))))


(define-solution (2025 09) (reds read-red-tiles)
  (values (part1 reds) (part2 reds)))

(define-test (2025 09) (4782268188 1574717268))
