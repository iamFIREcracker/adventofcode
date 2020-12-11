(defpackage :aoc/2020/11 #.cl-user::*aoc-use*)
(in-package :aoc/2020/11)

(defun parse-layout (data)
  (let* ((layout (make-hash-table))
         (row 0)
         pos)
    (dolist (string data layout)
      (dotimes (col (length string))
        (setf pos (complex col row)
              (gethash pos layout) (char string col)))
      (incf row))))

(defparameter *neighbors-deltas* '(#C(-1 -1) #C(0 -1) #C(1 -1) #C(-1 0) #C(1 0) #C(-1 1) #C(0 1) #C(1 1)))

(defun neighbors-occupied (seat layout)
  (count #\# *neighbors-deltas*
         :key (lambda (d) (gethash (+ seat d) layout #\.))))

(defun in-sight-occupied (seat layout)
  (labels ((recur (seat d &aux (state (gethash seat layout)))
             (cond ((not state) #\.)
                   ((char= #\. state) (recur (+ seat d) d))
                   (t state))))
    (count #\# *neighbors-deltas*
           :key (lambda (d) (recur (+ seat d) d)))))

(defun next (layout neighbors threshold &aux (result (make-hash-table)))
  (loop for seat being the hash-keys of layout
        for state = (gethash seat layout)
        for occupied-count = (funcall neighbors seat layout)
        do (setf (gethash seat result) state)
        when (and (char= state #\L) (zerop occupied-count)) do (setf (gethash seat result) #\#)
        when (and (char= state #\#) (>= occupied-count threshold)) do (setf (gethash seat result) #\L)
        finally (return result)))

(defun simulate (layout neighbors threshold)
  (loop for prev = layout then next
        for next = (next prev neighbors threshold)
        until (equalp next prev)
        finally (return (count #\# (hash-table-values prev)))))

(define-solution (2020 11) (layout parse-layout)
  (values
    (simulate layout #'neighbors-occupied 4)
    (simulate layout #'in-sight-occupied 5)))

(define-test (2020 11) (2424 2208))
