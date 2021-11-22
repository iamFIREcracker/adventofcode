(defpackage :aoc/2018/18 #.cl-user::*aoc-use*)
(in-package :aoc/2018/18)

(defun parse-area (data)
  (let ((rows (length data))
        (cols (length (first data))))
    (make-array (list rows cols) :initial-contents data)))


(defun part1 (area iterations)
  (dotimes (i iterations)
    (setf area (area-next area)))
  (resource-value area))


(defun area-next (area)
  (let* ((rows (array-dimension area 0))
         (cols (array-dimension area 1))
         (next (make-array (list rows cols))))
    (loop :for r :below rows :do
          (loop for c :below cols :do
                (loop :with open = 0 :and trees = 0 :and lumby = 0
                      :for rr :from (max (1- r) 0) :upto (min (1+ r) (1- rows)) :do
                      (loop :for cc :from (max (1- c) 0) :upto (min (1+ c) (1- cols))
                            :unless (and (= rr r) (= cc c)) :do
                            (ecase (aref area rr cc)
                              (#\. (incf open))
                              (#\| (incf trees))
                              (#\# (incf lumby))))
                      :finally (setf (aref next r c)
                                     (ecase (aref area r c)
                                       (#\. (if (>= trees 3) #\| #\.))
                                       (#\| (if (>= lumby 3) #\# #\|))
                                       (#\# (if (and (>= lumby 1) (>= trees 1))
                                              #\# #\.)))))))
    next))


(defun resource-value (area)
  (loop :for i :below (array-total-size area) :for ch = (row-major-aref area i)
        :count (eq ch #\|) :into trees
        :count (eq ch #\#) :into lumby
        :finally (return (* trees lumby))))


(defun part2 (area)
  (destructuring-bind (cycles-at cycle-size area-new)
      (floyd #'area-next area :test 'equalp)
    (let ((remaining (rem (- 1000000000 cycles-at) cycle-size)))
      (part1 area-new remaining))))


(define-solution (2018 18) (area parse-area)
  (values (part1 area 10) (part2 area)))

(define-test (2018 18) (549936 206304))
