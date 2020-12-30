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
             (mapcar (partial-1 #'parse-integer _ :junk-allowed T)
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

(defun remove-collisions (particles)
  (let* ((all-positions (mapcar #'particle-position particles))
         (freqs (frequencies all-positions :test 'equal))
         (unique-positions (mapcar #'car (remove 1 freqs :test-not 'eql :key #'cdr))))
    (remove-if-not
      (partial-1 #'member _ unique-positions) particles
      :key #'particle-position)))

(define-solution (2017 20) (data parse-particles)
  (labels ((copy-particles (particles)
             (mapcar #'copy-structure particles))
           (move-particles (particles)
             (dolist (p particles)
               (particle-movef p)))
           (distance-to-origin (p)
             (manhattan-distance (list 0 0 0) (particle-position p)))
           (closest-to-origin (particles)
             (let ((min (reduce #'min particles :key #'distance-to-origin)))
               (find min particles :key #'distance-to-origin))))
    (values
      (loop
        :with particles = (copy-particles data)
        :repeat 500
        :do (move-particles particles)
        :finally (return (particle-id (closest-to-origin particles))))
      (loop
        :with particles = (copy-particles data)
        :repeat 500 :do
        (move-particles particles)
        (setf particles (remove-collisions particles))
        :finally (return (length particles))))))

(define-test (2017 20) (258 707))
