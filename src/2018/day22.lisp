(defpackage :aoc/2018/22 #.cl-user::*aoc-use*)
(in-package :aoc/2018/22)

(defun parse-depth-target (list)
  (flet ((parse-depth (s &aux (splits (split-sequence:split-sequence #\Space s)))
           (parse-integer (second splits)))
         (parse-target (s &aux (splits (split-sequence:split-sequence #\Space s)))
           (let ((coords (split-sequence:split-sequence #\, (second splits))))
             (complex (parse-integer (first coords))
                      (parse-integer (second coords))))))
    (values (parse-depth (first list))
            (parse-target (second list)))))

(defun geologic-index (pos target depth)
  (cond ((zerop pos) 0)
        ((= pos target) 0)
        ((zerop (imagpart pos)) (* (realpart pos) 16807))
        ((zerop (realpart pos)) (* (imagpart pos) 48271))
        (T (* (erosion-level (- pos #C(1 0)) target depth)
              (erosion-level (- pos #C(0 1)) target depth)))))

(defun/memo erosion-level (pos target depth)
  (mod (+ (geologic-index pos target depth) depth) 20183))

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

(defun tools-by-area (area)
  (case area
    (0 '(climbing-gear torch))
    (1 '(climbing-gear none))
    (2 '(torch none))))

(defun complex-negp (n)
  (assert (or (complexp n) (numberp n)))
  (or (< (realpart n) 0)
      (< (imagpart n) 0)))

(defun cave-possible-moves (target depth state)
  (let* ((pos (pos state))
         (tool (tool state))
         (area (area-type (erosion-level pos target depth))))
    (cons
      (cons (make-state pos (change-tool tool area)) 7)
      (loop
        :for adj :in (remove-if #'complex-negp (adjacents pos))
        :for adj-area = (area-type (erosion-level adj target depth))
        :for adj-tools = (tools-by-area adj-area)
        :when (member tool adj-tools) :collect (cons (make-state adj tool) 1)))))

(define-solution (2018 22) (data)
  (multiple-value-bind (depth target) (parse-depth-target data)
    (values
      (reduce
        #'+ (loop for i from 0 upto (imagpart target)
                  append (loop for j from 0 upto (realpart target)
                               collect (erosion-level (complex j i)
                                                      target
                                                      depth)))
        :key #'area-type)
      (let* ((init-state (make-state #C(0 0) 'torch))
             (goal-state (make-state target 'torch)))
        (search-cost
          (a* init-state
              :goal-state goal-state
              :heuristic (partial-1 #'manhattan-distance
                                    (pos _)
                                    (pos goal-state))
              :neighbors (partial-1 #'cave-possible-moves target depth)
              :test 'equalp))))))

(define-test (2018 22) (5400 1048))
