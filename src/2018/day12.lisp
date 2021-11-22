(defpackage :aoc/2018/12 #.cl-user::*aoc-use*)
(in-package :aoc/2018/12)


(defun parse-input (data)
  (list (parse-pots (first data)) (parse-notes (cddr data))))

(defun parse-pots (string)
  (let ((pots (make-hset nil)))
    (loop :for i :from 0
          :for ch :across (subseq string 15)
          :when (eq ch #\#) :do (hset-add i pots))
    pots))

(defun parse-notes (data)
  (let ((notes (make-hset nil :test 'equal)))
    (loop :for string :in data
          :for from = (subseq string 0 5) :for to = (char string 9)
          :when (eq to #\#) :do (hset-add from notes))
    notes))

(defun pots (input) (first input))
(defun notes (input) (second input))


(defun part1 (input iterations)
  (destructuring-bind (pots notes) input
    (dotimes (i iterations)
      (setf pots (pots-next pots notes)))
    (sum-plant-positions pots)))

(defun pots-next (pots notes)
  (let ((next (make-hash-table))
        (min (find-min (hset-values pots)))
        (max (find-max (hset-values pots))))
    (loop :for i :from (- min 4) :to (+ max 4)
          :for key = (surrounding-pots pots i)
          :when (hset-contains-p key notes) :do (hset-add i next))
    next))

(defun surrounding-pots (pots i)
  (let ((string (make-string 5 :initial-element #\.)))
    (loop :for j :from (- i 2) :to (+ i 2)
          :for k :from 0
          :when (hset-contains-p j pots)
          :do (setf (char string k) #\#))
    string))

(defun sum-plant-positions (pots)
  (loop :for i :being :the :hash-keys :of pots :sum i))


(defun part2 (input iterations)
  (flet ((next (pots)
           (pots-next pots (notes input)))
         (key (pots)
           (let ((min (find-min (hset-values pots))))
             (loop :for i :being :the :hash-keys :of pots :sum (- i min)))))
    (destructuring-bind (cycles-at cycle-size pots-new)
        (floyd #'next (pots input) :key #'key)
      (let* ((base (sum-plant-positions pots-new))
             (pots-new-next (pots-next pots-new (notes input)))
             (per-cycle-increment (- (sum-plant-positions pots-new-next) base))
             (cycles (- iterations cycles-at)))
        (assert (= cycle-size 1))
        (+ base (* per-cycle-increment cycles))))))


(define-solution (2018 12) (input parse-input)
  (values (part1 input 20) (part2 input 50000000000)))

(define-test (2018 12) (4386 5450000001166))
