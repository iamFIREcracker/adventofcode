(defpackage :aoc/2019/20 #.cl-user::*aoc-use*)
(in-package :aoc/2019/20)

(defun read-map (data &aux (map (make-hash-table)))
  (prog1 map
    (loop
      :for str :in data
      :for i = 0 :then (1- i)
      :do (loop
            :for c :across str
            :for j = 0 :then (1+ j)
            :for pos = (complex j (- i))
            :do (hash-table-insert map pos c)))))

(defun wallp (c)
  (and c (char= c #\#)))

(defun passagep (c)
  (and c (char= c #\.)))

(defun emptyp (c)
  (and c (char= c #\Space)))

(defun labelp (c)
  (and c (not (or (wallp c)
                  (passagep c)
                  (emptyp c)))))

(defun 2-above (map pos)
  (mkstr (gethash (+ pos #C(0 -2)) map)
         (gethash (+ pos #C(0 -1)) map)))

(defun 2-right (map pos)
  (mkstr (gethash (+ pos #C(1 0)) map)
         (gethash (+ pos #C(2 0)) map)))

(defun 2-below (map pos)
  (mkstr (gethash (+ pos #C(0 1)) map)
         (gethash (+ pos #C(0 2)) map)))

(defun 2-left (map pos)
  (mkstr (gethash (+ pos #C(-2 0)) map)
         (gethash (+ pos #C(-1 0)) map)))

(defun read-portals (map &aux (labels (make-hash-table :test 'equal)))
  (prog1 labels
    (loop
      :for pos :being :the :hash-key :of map
      :for passage = (passagep (gethash pos map))
      :for 2-above = (2-above map pos)
      :for 2-right = (2-right map pos)
      :for 2-below = (2-below map pos)
      :for 2-left = (2-left map pos)
      :when (and passage (every #'labelp 2-above)) :do (push pos (gethash 2-above labels))
      :when (and passage (every #'labelp 2-right)) :do (push pos (gethash 2-right labels))
      :when (and passage (every #'labelp 2-below)) :do (push pos (gethash 2-below labels))
      :when (and passage (every #'labelp 2-left)) :do (push pos (gethash 2-left labels)))))

(defstruct (state (:conc-name s-))
  pos
  level)

(defstruct (donut (:constructor make-donut%)
                  (:conc-name d-))
  map
  portals
  outer-boundary-min-x
  outer-boundary-max-x
  outer-boundary-min-y
  outer-boundary-max-y)

(defun make-donut (data)
  (let* ((map (read-map data))
         (portals (read-portals map))
         (walls (loop
                  :for k :being :the :hash-keys :of map
                  :for v = (gethash k map)
                  :when (wallp v) :collect k)))
    (make-donut% :map map
                 :portals portals
                 :outer-boundary-min-x (reduce #'min walls :key #'realpart)
                 :outer-boundary-max-x (reduce #'max walls :key #'realpart)
                 :outer-boundary-min-y (reduce #'min walls :key #'imagpart)
                 :outer-boundary-max-y (reduce #'max walls :key #'imagpart))))

(defun d-start (d)
  (first (gethash "AA" (d-portals d))))

(defun d-end (d)
  (first (gethash "ZZ" (d-portals d))))

(defun d-cell (d pos)
  (gethash pos (d-map d)))

(defun d-other-side (d pos)
  (loop
    :for doors :being :the :hash-values :of (d-portals d)
    :for door1 = (first doors)
    :for door2 = (second doors)
    :when (and door2 (= pos door1)) :return door2
    :when (and door2 (= pos door2)) :return door1))

(defun d-neighbors (d pos)
  (loop
    :for next-pos :in (adjacents pos)
    :when (passagep (d-cell d next-pos)) :collect next-pos))

(defun d-neighbors-part1 (d pos)
  (append
    (d-neighbors d pos)
    (let ((other-side (d-other-side d pos)))
      (when other-side
        (list other-side)))))

(defun d-outer-boundary-p (d pos)
  (or (= (d-outer-boundary-min-x d) (realpart pos))
      (= (d-outer-boundary-max-x d) (realpart pos))
      (= (d-outer-boundary-min-y d) (imagpart pos))
      (= (d-outer-boundary-max-y d) (imagpart pos))))

(defun d-portal-active-p (d level pos)
  (or (not (= 0 level)) (not (d-outer-boundary-p d pos))))

(defun d-neighbors-part2 (d state)
  (let ((pos (s-pos state))
        (level (s-level state)))
    (append
      (mapcar (partial-1 #'make-state :pos _ :level level) (d-neighbors d pos))
      (when (or (not (zerop level)) (not (d-outer-boundary-p d pos)))
        (let ((other-side (d-other-side d pos)))
          (when (and other-side (d-portal-active-p d level pos))
            (list (make-state :pos other-side
                              :level (+ level (if (d-outer-boundary-p d pos) -1 1))))))))))

(define-solution (2019 20) (d make-donut)
  (values
    (multiple-value-bind (end-state end-state-cost)
        (bfs (d-start d)
             :goal-state (d-end d)
             :neighbors (partial-1 #'d-neighbors-part1 d))
      (declare (ignore end-state))
      end-state-cost)
    (multiple-value-bind (end-state end-state-cost)
        (bfs (make-state :pos (d-start d) :level 0)
             :goal-state (make-state :pos (d-end d) :level 0)
             :neighbors (partial-1 #'d-neighbors-part2 d)
             :test 'equalp)
      (declare (ignore end-state))
      end-state-cost)))

(define-test (2019 20) (496 5886))
