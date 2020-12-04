(defpackage :aoc/2016/17 #.cl-user::*aoc-use*)
(in-package :aoc/2016/17)

(defparameter *nhood* '(#C(0 1) #C(0 -1) #C(-1 0) #C(1 0)))
(defparameter *steps* "UDLR")

(defun door-states (seed path)
  (let* ((string (format nil "~A~A" seed path))
         (bytes (md5:md5sum-string string))
         (result (make-array 4 :element-type t :fill-pointer 0)))
    (dotimes (i 2)
      (let ((byte1 (ldb (byte 4 4) (aref bytes i)))
            (byte2 (ldb (byte 4 0) (aref bytes i))))
        (vector-push (> byte1 10) result)
        (vector-push (> byte2 10) result)))
    result))

(defstruct (state (:conc-name)) pos path)

(defun neighbors (seed state)
  (with-slots (pos path) state
    (loop for delta in *nhood*
          for step across *steps*
          for openp across (door-states seed path)
          for next = (+ pos delta)
          when (and (<= 0 (realpart next) 3)
                    (>= 0 (imagpart next) -3)
                    openp)
          collect (make-state :pos next
                              :path (format nil "~A~C" path step)))))

(defun part1 (seed)
  (path (bfs (make-state :pos #c(0 0) :path "")
             :goalp #'(lambda (state) (= (pos state) #c(3 -3)))
             :neighbors (partial-1 #'neighbors seed)
             :test 'equalp)))

(defun part2 (seed)
  (let (max)
    (bfs (make-state :pos #c(0 0) :path "")
         :prunep #'(lambda (state cost)
                    (when (null state) (break))
                    (with-slots (pos) state
                      (when (= pos #c(3 -3))
                        (when (or (null max)
                                  (> cost max))
                          (setf max cost))
                        t)))
         :neighbors (partial-1 #'neighbors seed)
         :test 'equalp)
    max))

(define-solution (2016 17) (seed first)
  (values
    (part1 seed)
    (part2 seed)))

(define-test (2016 17) ("DUDDRLRRRD" 578))
