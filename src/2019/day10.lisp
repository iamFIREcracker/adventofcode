(defpackage :aoc/2019/10 #.cl-user::*aoc-use*)
(in-package :aoc/2019/10)

(defstruct region
  height
  width
  asteroids)

(defun read-asteroids (data height width &aux (asteroids (make-hash-table)))
  (prog1 asteroids
    (dorange (i 0 height)
      (let ((row (nth i data)))
        (dorange (j 0 width)
          (when (eql (aref row j) #\#)
            (let ((key (complex j (- i))))
              (hash-table-insert asteroids key T))))))))

(defun read-region (data)
  (let* ((height (length data))
         (width (length (first data))))
    (make-region :height height
                 :width width
                 :asteroids (read-asteroids data height width))))

(defun sortable-phase (number &aux (ph (phase (complex-rotate-cw number))))
  (if (zerop ph) (* 2 (phase -1))
    (if (>= ph 0)
        ph
        (+ (phase -1) ph (phase -1)))))

(defun sortable-phase-test ()
  (prl
    (sortable-phase #C(0 1))
    (sortable-phase #C(1 1))
    (sortable-phase #C(1 0))
    (sortable-phase #C(1 -1))
    (sortable-phase #C(0 -1))
    (sortable-phase #C(-1 -1))
    (sortable-phase #C(-1 0))
    (sortable-phase #C(-1 1))))

(defun clockwise (asteroids current)
  (sort (copy-seq asteroids) #'> :key (partial-1 #'sortable-phase (- _ current))))

(defun sight-direction (from to &aux (dir (- to from)))
  (let ((real (realpart dir))
        (imag (imagpart dir)))
    (cond ((= 0 dir) 0)
          ((= 0 real) (complex 0 (/ imag (abs imag))))
          ((= 0 imag) (complex (/ real (abs real)) 0))
          (T (let ((rational (/ real imag)))
                (complex
                  (* (if (< real 0) -1 1) (abs (numerator rational)))
                  (* (if (< imag 0) -1 1) (abs (denominator rational)))))))))

(defun visible-asteroids (region current)
  (loop
    :with remaining = (copy-hash-table (region-asteroids region))
    :initially (remhash current remaining)
    :for other :in (clockwise (hash-table-keys remaining) current)
    :for dir = (sight-direction current other)
    :unless (or (= dir 0) (not (hash-table-key-exists-p remaining other)))
    :do (loop
          :for next = (+ other dir) :then (+ next dir)
          :while (and
                   (and (>= (realpart next) 0) (< (realpart next) (region-width region)))
                   (and (<= (imagpart next) 0) (> (imagpart next) (- (region-height region)))))
          :do (remhash next remaining))
    :finally (return (hash-table-keys remaining))))

(define-solution (2019 10) (region read-region)
  (let* ((station (find-max
                    (hash-table-keys (region-asteroids region))
                    :key (partial-1 #'length (visible-asteroids region _))))
         (visible (visible-asteroids region station)))
    (values
      (length visible)
      (let* ((vaporized (clockwise visible station))
             (nth (nth 199 vaporized)))
        (+ (* 100 (realpart nth)) (- (imagpart nth)))))))

(define-test (2019 10) (284 404))
