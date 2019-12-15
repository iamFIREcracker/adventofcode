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
  (make-moon (list
               (parse-integer (nth 1 parts) :junk-allowed T)
               (parse-integer (nth 2 parts) :junk-allowed T)
               (parse-integer (nth 3 parts) :junk-allowed T))))

(defun moon-update-velf (m o)
  (let ((vel-update (mapcar #'(lambda (v1 v2)
                                (if (< v1 v2)
                                  1
                                  (if (> v1 v2)
                                    -1
                                    0)))
                            (m-pos m)
                            (m-pos o))))
    (prog1 m
      (setf (m-vel o) (mapcar #'- (m-vel o) vel-update)
            (m-vel m) (mapcar #'+ (m-vel m) vel-update)))))

(defun moon-apply-velf (m)
  (setf (m-pos m) (mapcar #'+ (m-pos m) (m-vel m))))

(defun moon-energy (m)
  (let* ((pot (summation (m-pos m) :key #'abs))
         (kit (summation (m-vel m) :key #'abs)))
    (* pot kit)))

(defstruct (universe
             (:copier NIL)
             (:conc-name u-))
  moons)

(defun copy-universe (u)
  (make-universe :moons (mapcar #'copy-moon (u-moons u))))

(defun read-universe (data)
  (make-universe :moons (mapcar #'read-moon data)))

(defun universe-tick (u)
  (prog1 u
    (loop
      :for (moon . remaining) :on (u-moons u)
      :do (loop
            :for other :in remaining
            :do (moon-update-velf moon other)))
    (loop
      :for moon :in (u-moons u)
      :do (moon-apply-velf moon))))

(defun universe-energy (u)
  (summation (u-moons u) :key #'moon-energy))

(defun find-cycle-on-axis (axis u)
  (flet ((by-axis ()
           (gathering
             (dolist (m (u-moons u))
               (gather (nth axis (m-pos m)))
               (gather (nth axis (m-vel m)))))))
    (destructuring-bind (cycles-at cycle-size u)
        (floyd #'universe-tick
               u
               :copier #'copy-universe
               :key (lambda (u) (universe-by-axis u axis))
               :test 'equal)
      (declare (ignore cycles-at u))
      cycle-size)))

(define-problem (2019 12) (data)
  (values
    (let ((u (read-universe data)))
      (dotimes (step 1000)
        (universe-tick u))
      (universe-energy u))
    (lcm (find-cycle-on-axis 0 (read-universe data))
         (find-cycle-on-axis 1 (read-universe data))
         (find-cycle-on-axis 2 (read-universe data)))))

(1am:test test-2019/12
  (multiple-value-bind (part1 part2) (problem-run)
    (1am:is (= 8310 part1))
    (1am:is (= 319290382980408 part1))))
