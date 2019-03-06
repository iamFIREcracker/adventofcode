(defpackage :aoc/2017/20 #.cl-user::*aoc-use*)
(in-package :aoc/2017/20)

(defstruct particle
  id
  position
  velocity
  acceleration)

(defun particle-movef (p)
  (let* ((velocity (mapcar #'+ (particle-velocity p) (particle-acceleration p)))
         (position (mapcar #'+ (particle-position p) velocity)))
    (setf (particle-velocity p) velocity
          (particle-position p) position)))

(defun parse-particles (x &aux (id -1))
  (labels ((parse-xyz (s &aux (splits (split-sequence:split-sequence #\, s)))
             (mapcar (curry #'parse-integer >< :junk-allowed T)
                     (subseq splits 0 3)))
           (parse-particle (s &aux (splits (split-sequence:split-sequence #\< s)))
             (let ((position (parse-xyz (second splits)))
                   (velocity (parse-xyz (third splits)))
                   (acceleration (parse-xyz (fourth splits))))
               (make-particle :id (incf id)
                              :position position
                              :velocity velocity
                              :acceleration acceleration))))
    (mapcar #'parse-particle x)))

(define-problem (2017 20) (data parse-particles)
  (labels ((copy-particles (particles)
             (mapcar #'copy-structure particles))
           (move-particles (particles)
             (dolist (p particles)
               (particle-movef p)))
           (distance-to-origin (p)
             (manhattan-distance (list 0 0 0) (particle-position p)))
           (closest-to-origin (particles)
             (minimization particles :key #'distance-to-origin)))
    (values
      (loop
        :with particles = (copy-particles data)
        :with min = (closest-to-origin particles)
        :with closest = (find min particles :key #'distance-to-origin)
        :repeat 1000
        :do (move-particles particles)
        :do (let ((min-next (closest-to-origin particles)))
              (setf min min-next
                    closest (find min-next particles :key #'distance-to-origin)))
        :finally (return (particle-id closest)))
      (loop
        :with particles = (copy-particles data)
        :repeat 1000
        :do (move-particles particles)
        :do (setf particles (unique-only particles :key #'particle-position :test 'equal))
        :finally (return (length particles))))))

(1am:test test-2017/20
  (multiple-value-bind (part1 part2) (problem-run)
    (1am:is (= 258 part1))
    (1am:is (= 707 part2))))
