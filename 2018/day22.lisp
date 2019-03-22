(defpackage :aoc/2018/22 #.cl-user::*aoc-use*)
(in-package :aoc/2018/22)

(defun parse-depth-target (list)
  (flet ((parse-depth (s &aux (splits (split-sequence #\Space s)))
           (parse-integer (second splits)))
         (parse-target (s &aux (splits (split-sequence #\Space s)))
           (let ((coords (split-sequence #\, (second splits))))
             (complex (parse-integer (first coords))
                      (parse-integer (second coords))))))
    (values (parse-depth (first list))
            (parse-target (second list)))))

(defun geologic-index (cave pos target)
  (cond ((zerop pos) 0)
        ((= pos target) 0)
        ((zerop (imagpart pos)) (* (realpart pos) 16807))
        ((zerop (realpart pos)) (* (imagpart pos) 48271))
        (T (* (gethash (- pos #C(1 0)) cave)
              (gethash (- pos #C(0 1)) cave)))))

(defun erosion-level (cave pos target depth)
  (mod (+ (geologic-index cave pos target) depth) 20183))

(defun make-cave (target depth &aux (cave (make-hash-table)))
  (doirange (i 0 1000)
    (doirange (j 0 300)
      (setf (gethash (complex j i) cave)
            (erosion-level cave (complex j i) target depth))))
  cave)

(defun area-type (erosion-level) (mod erosion-level 3))

(defstruct (state
             (:constructor make-state%)
             (:conc-name NIL))
  pos
  tool)

(defun make-state (pos tool)
  (make-state% :pos pos :tool tool))

(defun change-tool (current-tool area)
  (case area
    (0 (ecase current-tool
         (climbing-gear 'torch)
         (torch 'climbing-gear)))
    (1 (ecase current-tool
         (climbing-gear 'none)
         (none 'climbing-gear)))
    (2 (ecase current-tool
         (torch 'none)
         (none 'torch)))))

(defun adjacents (pos) ;; XXX there is another ADJACENTS function
  (remove-if-not #'(lambda (c)
                     (and (>= (realpart c) 0) (>= (imagpart c) 0)))
                 (list
                   (- pos #C(0 1))
                   (- pos #C(1 0))
                   (+ pos #C(1 0))
                   (+ pos #C(0 1)))))

(defun tools-by-area (area)
  (case area
    (0 '(climbing-gear torch))
    (1 '(climbing-gear none))
    (2 '(torch none))))

(defun cave-possible-moves (cave state time)
  (let* ((pos (pos state))
         (tool (tool state))
         (area (area-type (gethash pos cave))))
    (cons
      (list (make-state pos (change-tool tool area)) (+ time 7))
      (loop
        :for adj :in (adjacents pos)
        :for adj-area = (area-type (gethash adj cave))
        :for adj-tools = (tools-by-area adj-area)
        :when (member tool adj-tools) :collect (list (make-state adj tool)
                                                     (1+ time))))))

(define-problem (2018 22) (data)
  (multiple-value-bind (depth target) (parse-depth-target data)
    (let ((cave (make-cave target depth)))
      (values
        (summation
          (gathering
            (doirange (i 0 (imagpart target))
              (doirange (j 0 (realpart target))
                (gather (gethash (complex j i) cave)))))
          :key #'area-type)
        (let* ((init-state (make-state #C(0 0) 'torch))
               (target-state (make-state target 'torch))
               (cost-so-far (a-star init-state 0 target-state
                                    #'(lambda (state cost)
                                        (cave-possible-moves cave state cost))
                                    #'(lambda (state)
                                        (manhattan-distance (pos state)
                                                            (pos target-state))))))
          (gethash target-state cost-so-far))))))

(1am:test test-2018/22
  (multiple-value-bind (part1 part2) (problem-run)
    (1am:is (= 5400 part1))
    (1am:is (= 1048 part2))))
