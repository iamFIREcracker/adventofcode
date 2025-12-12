(defpackage :aoc/2025/12 #.cl-user::*aoc-use*)
(in-package :aoc/2025/12)

(defun read-input (&optional (strings (uiop:read-file-lines #P"src/2025/day12.txt")))
  (destructuring-bind (regions . shapes) (reverse (split-sequence:split-sequence "" strings :test 'equal))
    (list (looping
            (dolist (sh (reverse shapes))
              (let1 sh2 (rest sh)
                (collect! (apply 'spr sh2)))))
          (looping
            (dolist (s regions)
              (destructuring-bind (w h . cc) (extract-integers s)
                (collect! (list* w h cc))))))))
#+#:excluded (read-input)


; borrowed from: 2017 21
(defun indices-flip-v ()
  `((2 0) (2 1) (2 2)
    (1 0) (1 1) (1 2)
    (0 0) (0 1) (0 2)))

(defun indices-rotate-cw ()
  `((2 0) (1 0) (0 0)
    (2 1) (1 1) (0 1)
    (2 2) (1 2) (0 2)))

(defun select-pixels (source indices)
  (loop
    :for (i j) :in indices
    :for flattened = (+ (* i 3) j)
    :collect (aref source flattened) :into pixels
    :finally (return (apply #'spr pixels))))

(defun generate-patterns (original)
  (let* ((rotated-cw1 (select-pixels original (indices-rotate-cw)))
         (rotated-cw2 (select-pixels rotated-cw1 (indices-rotate-cw)))
         (rotated-cw3 (select-pixels rotated-cw2 (indices-rotate-cw))))
    (remove-duplicates
      (list original
            (select-pixels original (indices-flip-v))
            rotated-cw1
            (select-pixels rotated-cw1 (indices-flip-v))
            rotated-cw2
            (select-pixels rotated-cw2 (indices-flip-v))
            rotated-cw3
            (select-pixels rotated-cw3 (indices-flip-v)))
      :test 'string=)))
#+#:excluded (generate-patterns (~> (read-input) first fifth ))


(defun overlap-not? (region w sh i j)
  (looping
    (dotimes (ii 3)
      (dotimes (jj 3)
        (when (char= (aref sh (+ (* ii 3) jj)) #\#)
          (let1 pos (+ (* (+ i ii) w) (+ j jj))
            (always! (char= (aref region pos) #\.))))))))

(defun place! (region w sh i j)
  (dotimes (ii 3)
    (dotimes (jj 3)
      (when (char= (aref sh (+ (* ii 3) jj)) #\#)
        (let1 pos (+ (* (+ i ii) w) (+ j jj))
          (setf (aref region pos) #\#))))))

(defun unplace! (region w sh i j)
  (dotimes (ii 3)
    (dotimes (jj 3)
      (when (char= (aref sh (+ (* ii 3) jj)) #\#)
        (let1 pos (+ (* (+ i ii) w) (+ j jj))
          (setf (aref region pos) #\.))))))

(defun impossible? (region shapes)
  (destructuring-bind (w h . cc) region
    (let ((region-area (* w h))
          (presents-area (looping
                           (dolists ((sh shapes)
                                     (c cc))
                             (sum! (* (count #\# sh) c))))))
      (> presents-area region-area))))

(defun solvable? (region shapes)
  (unless (impossible? region shapes)
    (destructuring-bind (w h . cc) region
      (let ((region (make-string (* w h) :initial-element #\.))
            (all-shapes (looping
                          (dolist (sh shapes)
                            (collect! (generate-patterns sh))))))
        (looping
          (recursively ()
            (thereis! (every 'zerop cc))
            (let1 n (position-if 'plusp cc)
              (dolist (sh (nth n all-shapes))
                (dotimes (i (- h 2))
                  (dotimes (j (- w 2))
                    (when (overlap-not? region w sh i j)
                      (place! region w sh i j)
                      (decf (nth n cc))
                      (recur)
                      (incf (nth n cc))
                      (unplace! region w sh i j))))))))))))

(define-solution (2025 12) (input read-input)
  (destructuring-bind (shapes regions) input
    (looping
      (dolist (r regions)
        (count! (solvable? r shapes))))))

(define-test (2025 12) (593))
