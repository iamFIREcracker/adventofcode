(defpackage :aoc/2019/24 #.cl-user::*aoc-use*)
(in-package :aoc/2019/24)

(defstruct (eris (:constructor make-eris%)
                 (:copier NIL)
                 (:conc-name NIL))
  tiles)

(defun make-eris (data &aux (tiles (make-hash-table)))
  (prog1 (make-eris% :tiles tiles)
    (loop
      :for str :in data
      :for i = 0 :then (1+ i)
      :do (loop
            :for c :across str
            :for j = 0 :then (1+ j)
            :do (hash-table-insert tiles (complex j (- i)) c)))))

(defun copy-eris (e)
  (make-eris% :tiles (copy-hash-table (tiles e))))

(defun bugp (c)
  (eql #\# c))

(defun emptyp (c)
  (eql #\. c))

(defun eris-neighbors (e pos)
  (loop
    :for delta :in (list #C(0 1) #C(1 0) #C(0 -1) #C(-1 0))
    :for pos-next = (+ pos delta)
    :for c = (gethash pos-next (tiles e))
    :when c :collect it))

(defun evolve (curr adj-bugs)
  (if (bugp curr)
    (if (= 1 adj-bugs) #\# #\.)
    (if (or (= 1 adj-bugs) (= 2 adj-bugs)) #\# #\.)))

(defun eris-evolve (e &aux (e-next (copy-eris e)))
  (prog1 e-next
    (loop
      :for pos :being :the :hash-keys :of (tiles e)
      :for c = (gethash pos (tiles e))
      :for adj-bugs = (count-if #'bugp (eris-neighbors e pos))
      :do (hash-table-insert (tiles e-next) pos (evolve c adj-bugs)))))

(defun biodiversity-rating (e)
  (loop
    :with pow = 1
    :for i :from 0 :upto 4
    :summing (loop
               :for j :from 0 :upto 4
               :for pos = (complex j (- i))
               :for c = (gethash pos (tiles e))
               :when (bugp c) :summing pow
               :do (setf pow (* pow 2)))))

(defstruct (recursive-eris (:constructor make-recursive-eris%)
                           (:copier NIL)
                           (:conc-name NIL))
  levels)

(defun make-recursive-eris (data &aux (levels (make-hash-table)))
  (let ((re (make-recursive-eris% :levels levels)))
    (prog1 re
      (hash-table-insert levels 0 (make-eris data))
      (recursive-eris-create-levelf re 1)
      (recursive-eris-create-levelf re -1))))

(defun copy-recursive-eris (re &aux (levels (make-hash-table)))
  (prog1 (make-recursive-eris% :levels levels)
    (dolist (k (hash-table-keys (levels re)))
      (hash-table-insert levels k (copy-eris (gethash k (levels re)))))))

(defun print-recursive-eris (re)
  (loop
    :with min-level = (recursive-eris-min-level re)
    :with max-level = (recursive-eris-max-level re)
    :for level :from min-level :upto max-level
    :for eris = (gethash level (levels re))
    :unless (not (some #'bugp (hash-table-values (tiles eris))))
    :do (format T "Depth ~d:~%" level)
    :and :do (print-hash-table-map (tiles (gethash level (levels re))))
    :do (format T "~%")))

(defun recursive-eris-min-level (re)
  (minimization (hash-table-keys (levels re))))

(defun recursive-eris-max-level (re)
  (maximization (hash-table-keys (levels re))))

(defun recursive-eris-create-levelf (re level &aux (tiles (make-hash-table)))
  (dorange (i 0 5)
    (dorange (j 0 5)
      (hash-table-insert tiles (complex j (- i)) #\.)))
  (hash-table-insert (levels re) level (make-eris% :tiles tiles)))

(defun recursive-eris-neighbors (re level pos)
  (flet ((get-eris (level)
           (or (gethash level (levels re))
               (recursive-eris-create-levelf re level))))
    (loop
      :until (= pos #C(2 -2))
      :with eris = (gethash level (levels re))
      :for delta :in (list #C(0 1) #C(1 0) #C(0 -1) #C(-1 0))
      :for pos-next = (+ pos delta)
      :for i = (imagpart pos-next)
      :for j = (realpart pos-next)
      :append (cond ((> i 0) (let ((eris-above (get-eris (1- level))))
                               (list (gethash #C(2 -1) (tiles eris-above)))))
                    ((> j 4) (let ((eris-above (get-eris (1- level))))
                               (list (gethash #C(3 -2) (tiles eris-above)))))
                    ((< i -4) (let ((eris-above (get-eris (1- level))))
                                (list (gethash #C(2 -3) (tiles eris-above)))))
                    ((< j 0) (let ((eris-above (get-eris (1- level))))
                               (list (gethash #C(1 -2) (tiles eris-above)))))
                    ((= pos-next #C(2 -2)) (cond ((= delta #C(0 1)) (let ((eris-below (get-eris (1+ level))))
                                                                      (list
                                                                        (gethash #C(0 -4) (tiles eris-below))
                                                                        (gethash #C(1 -4) (tiles eris-below))
                                                                        (gethash #C(2 -4) (tiles eris-below))
                                                                        (gethash #C(3 -4) (tiles eris-below))
                                                                        (gethash #C(4 -4) (tiles eris-below)))))
                                                 ((= delta #C(1 0)) (let ((eris-below (get-eris (1+ level))))
                                                                      (list
                                                                        (gethash #C(0  0) (tiles eris-below))
                                                                        (gethash #C(0 -1) (tiles eris-below))
                                                                        (gethash #C(0 -2) (tiles eris-below))
                                                                        (gethash #C(0 -3) (tiles eris-below))
                                                                        (gethash #C(0 -4) (tiles eris-below)))))
                                                 ((= delta #C(0 -1)) (let ((eris-below (get-eris (1+ level))))
                                                                       (list
                                                                         (gethash #C(0 0) (tiles eris-below))
                                                                         (gethash #C(1 0) (tiles eris-below))
                                                                         (gethash #C(2 0) (tiles eris-below))
                                                                         (gethash #C(3 0) (tiles eris-below))
                                                                         (gethash #C(4 0) (tiles eris-below)))))
                                                 ((= delta #C(-1 0)) (let ((eris-below (get-eris (1+ level))))
                                                                       (list
                                                                         (gethash #C(4  0) (tiles eris-below))
                                                                         (gethash #C(4 -1) (tiles eris-below))
                                                                         (gethash #C(4 -2) (tiles eris-below))
                                                                         (gethash #C(4 -3) (tiles eris-below))
                                                                         (gethash #C(4 -4) (tiles eris-below)))))))
                    (T (list (gethash pos-next (tiles eris))))))))

(defun recursive-eris-evolve (re &aux (re-next (copy-recursive-eris re)))
  (prog1 re-next
    (loop
      :for level :in (hash-table-keys (levels re))
      :for e = (gethash level (levels re))
      :for e-next = (gethash level (levels re-next))
      :do (loop
            :for pos :in (hash-table-keys (tiles e))
            :for c = (gethash pos (tiles e))
            :for adj-bugs = (count-if #'bugp (recursive-eris-neighbors re level pos))
            :do (hash-table-insert (tiles e-next) pos (evolve c adj-bugs))))
   (recursive-eris-create-levelf re-next (1- (recursive-eris-min-level re-next)))
   (recursive-eris-create-levelf re-next (1+ (recursive-eris-max-level re-next)))))

(defun recursive-eris-bugs (re)
  (loop
    :for eris :in (hash-table-values (levels re))
    :summing (count-if #'bugp (hash-table-values (tiles eris)))))

(define-problem (2019 24) (data)
  (values
    (destructuring-bind (cycles-at cycle-size e)
        (floyd #'eris-evolve
               (make-eris data)
               :key #'tiles
               :test 'equalp)
      (declare (ignore cycles-at cycle-size))
      (biodiversity-rating e))
    (loop
      :for n :from 0 :upto 200
      :for re = (make-recursive-eris data) :then (recursive-eris-evolve re)
      :finally (return (recursive-eris-bugs re)))))

(1am:test test-2019/24
  (multiple-value-bind (part1 part2) (problem-run)
    (1am:is (= 7543003 part1))
    (1am:is (= 1975 part2))))
