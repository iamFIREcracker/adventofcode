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

(defstruct (donut (:constructor make-donut%)
                  (:conc-name d-))
  map
  portals)

(defun make-donut (data)
  (let* ((map (read-map data))
         (portals (read-portals map)))
    (make-donut% :map map
                 :portals portals)))

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

(defun d-neighbors-part1 (d pos)
  (gathering
    (let ((other-side (d-other-side d pos)))
      (when other-side
        (gather other-side)))
    (loop
      :for delta :in (list #C(0 1) #C(1 0) #C(0 -1) #C(-1 0))
      :for next-pos = (+ pos delta)
      :when (passagep (d-cell d next-pos)) :do (gather next-pos))))

(defstruct (state (:conc-name s-))
  pos
  level)

(defun d-outerp (d pos)
  (or (= 2 (realpart pos))
      (= 110 (realpart pos))
      (= 2 (imagpart pos))
      (= 112 (imagpart pos))))

(defun d-neighbors-part2 (d state steps)
  (let ((pos (s-pos state))
        (level (s-level state)))
    (gathering
      (progn
        (when (or (not (zerop level)) (not (d-outerp d pos)))
          (let* ((other-side (d-other-side d pos)))
            (when other-side
              (gather (list
                        (make-state :pos other-side
                                    :level (+ level (if (d-outerp d other-side) 1 -1)))
                        (1+ steps))))))
        (loop
          :for delta :in (list #C(0 1) #C(1 0) #C(0 -1) #C(-1 0))
          :for next-pos = (+ pos delta)
          :when (passagep (d-cell d next-pos)) :do (gather (list
                                                             (make-state :pos next-pos :level level)
                                                             (1+ steps))))))))

(define-problem (2019 20) (d make-donut)
  (values
    (let ((cost-so-far (bfs (d-start d)
                            0
                            (d-end d)
                            (partial-1 #'d-neighbors-part1 d))))
      (gethash (d-end d) cost-so-far))
    (let* ((init-state (make-state :pos (d-start d) :level 0))
           (end-state (make-state :pos (d-end d) :level 0))
           (cost-so-far (a-star init-state
                                0
                                end-state
                                (partial-2 #'d-neighbors-part2 d)
                                (lambda (state)
                                  (let ((pos (s-pos state))
                                        (level (s-level state))
                                        (target-pos (s-pos end-state))
                                        (target-level (s-level end-state)))
                                    (manhattan-distance (list (realpart pos) (imagpart pos) (* 1000 level))
                                                        (list (realpart target-pos) (imagpart target-pos) (* 1000 target-level))))))))
      (gethash end-state cost-so-far))))

(1am:test test-2019/20
  (multiple-value-bind (part1 part2) (problem-run)
    (1am:is (= 496 part1))
    (1am:is (= 5886 part2))))
