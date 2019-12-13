(defpackage :aoc/2019/12 #.cl-user::*aoc-use*)
(in-package :aoc/2019/12)

(defstruct (moon
             (:constructor make-moon%)
             (:copier NIL)
             (:conc-name m-))
  pos
  vel)

(defun make-moon (pos)
  (make-moon% :pos pos :vel (list 0 0 0)))

(defun copy-moon (m)
  (make-moon% :pos (copy-list (m-pos m))
              :vel (copy-list (m-vel m))))

(defun read-moon (str &aux (parts (split-sequence:split-sequence #\= str)))
  (list
    (parse-integer (nth 1 parts) :junk-allowed T)
    (parse-integer (nth 2 parts) :junk-allowed T)
    (parse-integer (nth 3 parts) :junk-allowed T)))

(defun read-moons(data)
  (mapcar #'read-moon data))

(defun create-velocities ()
  (list (list 0 0 0) (list 0 0 0) (list 0 0 0) (list 0 0 0)))

(defun update-velocities (moons velocities)
  (prog1 velocities
    (loop
       :with max = (1- (length moons))
       :for i :upto max
       :for (ix iy iz) = (nth i moons)
       :for iv = (nth i velocities)
       :do (loop
             :for j :from (1+ i) :upto max
             :for (jx jy jz) = (nth j moons)
             :for jv = (nth j velocities)
             :when (< ix jx) :do (progn (incf (nth 0 iv)) (decf (nth 0 jv)))
             :when (> ix jx) :do (progn (decf (nth 0 iv)) (incf (nth 0 jv)))
             :when (< iy jy) :do (progn (incf (nth 1 iv)) (decf (nth 1 jv)))
             :when (> iy jy) :do (progn (decf (nth 1 iv)) (incf (nth 1 jv)))
             :when (< iz jz) :do (progn (incf (nth 2 iv)) (decf (nth 2 jv)))
             :when (> iz jz) :do (progn (decf (nth 2 iv)) (incf (nth 2 jv)))))))

(defun apply-velocity (moons velocities)
  (loop
    :for moon :in moons
    :for velocity :in velocities
    :collecting (mapcar #'+ moon velocity)))

(defun total-energy (moons velocities)
  (loop
    :for moon :in moons
    :for velocity :in velocities
    :for pot = (summation moon :key #'abs)
    :for kit = (summation velocity :key 'abs)
    :for total = (* pot kit)
    :summing total))

(define-problem (2019 12) (moons read-moons)
  (values
    (loop
      :for step = 0 :then (1+ step)
      :for velocities = (create-velocities) :then (update-velocities positions velocities)
      :for positions = moons :then (apply-velocity positions velocities)
      :until (= step 1000)
      :finally (return (total-energy positions velocities)))
    (loop
      :for step = 0 :then (1+ step)
      :for velocities = (create-velocities) :then (update-velocities positions velocities)
      :for positions = moons :then (apply-velocity positions velocities)
      :do (if (zerop (mod step 10000)) (prl step))
      :until (and (plusp step) (equal positions moons))
      :finally (return (1+ step)))))

(1am:test test-2019/12
  (multiple-value-bind (part1 part2) (problem-run)
    (1am:is (= 8310 part1))))
