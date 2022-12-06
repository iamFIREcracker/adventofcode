(defpackage :aoc/2018/23 #.cl-user::*aoc-use*)
(in-package :aoc/2018/23)

(defstruct (nanobot (:type list)
                    :conc-name)
  pos r)
(defun x (pos) (first pos))
(defun y (pos) (second pos))
(defun z (pos) (third pos))

(defun parse-nanobots (data)
  (mapcar #'parse-nanobot data))

(defun parse-nanobot (string)
  (cl-ppcre:register-groups-bind ((#'parse-integer x y z r))
      ("pos=<(-?\\d+),(-?\\d+),(-?\\d+)>, r=(\\d+)" string)
    (make-nanobot :pos (list x y z) :r r)))


(defun part1 (bots)
  (let ((strongest (find-max bots :key #'r)))
    (count-if (partial-1 #'nanobot-contains-p strongest (pos _)) bots)))

(defun nanobot-contains-p (nb pos)
  (<= (manhattan-distance (pos nb) pos) (r nb)))


(defstruct (box (:type list)
                (:constructor make-box%))
  range-x range-y range-z)

(defun make-box (x-min x-max y-min y-max z-min z-max)
  (let* ((box (make-box% :range-x (list x-min x-max)
                         :range-y (list y-min y-max)
                         :range-z (list z-min z-max)))
         (volume (box-volume box)))
    (assert (or (= volume 0) (pow2p volume)))
    box))

(defun pow2p (n)
  (and (/= n 0) (zerop (logand n (1- n)))))

(defun bots-box (bots)
  (loop :for bot :in bots :for pos = (pos bot)
        :minimize (x pos) :into x-min
        :maximize (x pos) :into x-max
        :minimize (y pos) :into y-min
        :maximize (y pos) :into y-max
        :minimize (z pos) :into z-min
        :maximize (z pos) :into z-max
        :finally (return (let* ((largest (max (- x-max x-min)
                                              (- y-max y-min)
                                              (- z-max z-min)))
                                (side (make-pow2 largest)))
                           (make-box x-min (+ x-min side)
                                     y-min (+ y-min side)
                                     z-min (+ z-min side))))))

(defun make-pow2 (number)
  (expt 2 (ceiling (log number 2))))


(defstruct (state (:type list)
                  (:constructor make-state%)
                  :conc-name)
  box num-nanobots volume distance)

(defun make-state (bots box)
  (let ((x (caar box))
        (y (caadr box))
        (z (caaddr box)))
    (make-state% :box box
                 :num-nanobots (count-in-range bots box)
                 :volume (box-volume box)
                 :distance (manhattan-distance (list x y z) (list 0 0 0)))))


(defun count-in-range (nanobots box)
  (count-if (partial-1 #'nanobot-overlaps-p _ box) nanobots))

(defun nanobot-overlaps-p (nb box)
  (loop :with distance = 0
        :for (v-min v-max) :in box
        :for v :in (pos nb)
        :if (< v v-min) :do (incf distance (- v-min v))
        :else if (> v v-max) :do (incf distance (- v v-max))
        :never (> distance (r nb))))

(defun box-volume (box)
  (destructuring-bind ((x-min x-max) (y-min y-max) (z-min z-max)) box
    (* (- x-max x-min) (- y-max y-min) (- z-max z-min))))


(defun part2 (bots)
  (let ((box (bots-box bots))
        (best))
    (loop :with queue = (make-hq :predicate #'state-better-p)
          :initially (hq-insert queue (make-state bots box))
          :until (hq-empty-p queue)
          :for state = (hq-pop queue)
          :do (when (or (not best) (>= (num-nanobots state) (num-nanobots best)))
                (if (box-point-p (box state))
                  (if (or (not best) (state-better-p state best))
                    (setf best state))
                  (dolist (subbox (box-subdivisions (box state)))
                    (hq-insert queue (make-state bots subbox)))))
          :finally (return (distance best)))))


(defun state-better-p (state other)
  (destructuring-bind (num-nanobots volume distance) (rest state)
    (destructuring-bind (o-num-nanobots o-volume o-distance) (rest other)
      (or (> num-nanobots o-num-nanobots)
          (and (= num-nanobots o-num-nanobots)
               (or (< volume o-volume)
                   (and (= volume o-volume)
                        (< distance o-distance))))))))

(defun box-point-p (box)
  (loop :for (min max) :in box
        :always (= min max)))

(defun box-subdivisions (box)
  (unless (box-point-p box)
    (if (= (box-volume box) 1)
      (loop :for (x y z) :in (box-vertices box)
            :collect (make-box x x y y z z))
      (destructuring-bind ((x-min x-max) (y-min y-max) (z-min z-max)) box
        (let ((x-mid (+ x-min (floor (- x-max x-min) 2)))
              (y-mid (+ y-min (floor (- y-max y-min) 2)))
              (z-mid (+ z-min (floor (- z-max z-min) 2))))
          (loop :for (x-min x-max) :on (list x-min x-mid x-max)
                :when x-max :append
                (loop :for (y-min y-max) :on (list y-min y-mid y-max)
                      :when y-max :append
                      (loop :for (z-min z-max) :on (list z-min z-mid z-max)
                            :when z-max :collect (make-box x-min x-max
                                                           y-min y-max
                                                           z-min z-max)))))))))

(defun box-vertices (box)
  (destructuring-bind (x-range y-range z-range) box
    (loop :for x :in x-range :append
          (loop :for y :in y-range :append
                (loop :for z :in z-range :collect (list x y z))))))


(define-solution (2018 23) (bots parse-nanobots)
  (values (part1 bots) (part2 bots)))

(define-test (2018 23) (433 107272899))
