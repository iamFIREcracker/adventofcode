(defpackage :aoc/2019/15 #.cl-user::*aoc-use*)
(in-package :aoc/2019/15)

(defparameter *directions* '(1 4 2 3))
(defparameter *deltas* '(#C(0 1) #C(1 0) #C(0 -1) #C(-1 0)))
(defparameter *opposites* '(2 3 1 4))

(defun build-map (program)
  (let ((map (make-hash-table))
        oxygen)
    (labels ((move (i)
               (enqueue i (intcode:program-in program))
               (intcode:program-run program)
               (dequeue (intcode:program-out program)))
             (explore-dfs (pos)
               (unless (hash-table-key-exists-p map pos)
                 (hash-table-insert map pos #\Space)
                 (loop
                   :for i :in *directions* :and d :in *deltas* :and i-opp :in *opposites*
                   :for pos-next = (+ pos d) :and out = (move i)
                   :do (ecase out
                         (0 (hash-table-insert map pos-next #\#))
                         ((1 2)
                          (when (= out 2) (setf oxygen pos-next))
                          (explore-dfs pos-next)
                          (move i-opp))))))) ;; move back after done exploring
      (explore-dfs 0)
      (values 0 oxygen map))))

(defun neighbors (map pos)
  (loop
    :for d :in *deltas*
    :for pos-next = (+ pos d)
    :for value = (gethash pos-next map)
    :unless (eql value #\#)
    :collect pos-next))

(defun print-map (start oxygen h)
  (print-hash-table-map h (lambda (value pos)
                            (cond ((null value) #\#)
                                  ((= start pos) #\S)
                                  ((= oxygen pos) #\O)
                                  (T value)))))

(define-solution (2019 15) (program intcode:read-program)
  (multiple-value-bind (start oxygen map)
      (build-map program)
    (multiple-value-bind (end-state end-state-cost end-state-path cost-so-far)
        (bfs oxygen
             :neighbors (partial-1 #'neighbors map))
      (declare (ignore end-state end-state-cost end-state-path))
      (values
        (gethash start cost-so-far)
        (reduce #'max (hash-table-values cost-so-far))))))

(define-test (2019 15) (336 360))
