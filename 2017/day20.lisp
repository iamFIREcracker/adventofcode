(defpackage :aoc/2017/20 #.cl-user::*aoc-use*)
(in-package :aoc/2017/20)

(defstruct (particle (:conc-name NIL))
  id
  px
  py
  pz
  vx
  vy
  vz
  ax
  ay
  az)

(defun particle-movef (p)
  (let ((px (px p))
        (py (py p))
        (pz (pz p))
        (vx (vx p))
        (vy (vy p))
        (vz (vz p))
        (ax (ax p))
        (ay (ay p))
        (az (az p)))
    (setf (vx p) (+ vx ax)
          (vy p) (+ vy ay)
          (vz p) (+ vz az)
          (px p) (+ px vx ax)
          (py p) (+ py vy ay)
          (pz p) (+ pz vz az))))

(defun particle-position (p)
  (list (px p) (py p) (pz p)))

(defun parse-particles (x &aux (id -1))
  (labels ((parse-xyz (s &aux (splits (split-sequence:split-sequence #\, s)))
             (mapcar (curry #'parse-integer >< :junk-allowed T)
                     (subseq splits 0 3)))
           (parse-particle (s &aux (splits (split-sequence:split-sequence #\< s)))
             (let ((position (parse-xyz (second splits)))
                   (velocity (parse-xyz (third splits)))
                   (acceleration (parse-xyz (fourth splits))))
               (make-particle :id (incf id)
                              :px (first position)
                              :py (second position)
                              :pz (third position)
                              :vx (first velocity)
                              :vy (second velocity)
                              :vz (third velocity)
                              :ax (first acceleration)
                              :ay (second acceleration)
                              :az (third acceleration)))))
    (mapcar #'parse-particle x)))

(defun unique-only (x &key (key 'identity) (test 'eql))
  (let ((kvs (mapcar #'(lambda (k)
                         (list k (funcall key k)))
                     x)))
    (recursively ((kv (first kvs))
                  (rest (rest kvs)))
      (when kv
        (let ((k (first kv))
              (v (second kv)))
          (cond ((member v rest :key #'second :test test)
                 (let ((without-duplicates (remove v rest :key #'second :test test)))
                   (recur (first without-duplicates) (rest without-duplicates))))
                (T (cons k (recur (first rest) (rest rest))))))))))

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
        :finally (return (id closest)))
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
