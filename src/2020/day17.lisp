(defpackage :aoc/2020/17 #.cl-user::*aoc-use*
  (:shadow :frequencies))
(in-package :aoc/2020/17)

;; HASH-SET
(defun make-hset (values &key (test 'eql))
  (let ((h (make-hash-table :test test)))
    (dolist (v values)
      (setf (gethash v h) t))
    h))

(defun hset-contains-p (v hset)
  (multiple-value-bind (unused exists-p)
      (gethash v hset)
    (declare (ignore unused))
    exists-p))

(defun hset-values (hset)
  (loop for v being the hash-keys of hset collect v))

(defun hset-size (hset) (hash-table-count hset))

;; GAME OF LIFE
(defparameter *dimensions* 2)
(defparameter *neighbors-deltas* nil)

(defun neighbors-deltas (dimensions)
  (labels ((recur (n)
             (cond ((= n 0) (list nil))
                   (t (loop for d from -1 upto 1 nconc
                            (loop for rest in (recur (1- n))
                                  collect (cons d rest)))))))
    (remove-if (lambda (x) (apply #'= 0 x)) (recur dimensions))))

(defun neighbors (pos)
  (loop for delta in *neighbors-deltas* collect (mapcar #'+ pos delta)))

(defun frequencies (list &key (test 'eql))
  (let ((f (make-hash-table :test test)))
    (dolist (each list)
      (incf (gethash each f 0)))
    (loop for k being the hash-keys of f collect (cons k (gethash k f)))))

(defun next (state)
  (make-hset
    (loop
      for (pos . n) in (frequencies (mapcan #'neighbors (hset-values state)) :test 'equal)
      when (or (= n 3) (and (= n 2) (hset-contains-p pos state)))
      collect pos)
    :test 'equal))

(defun parse-game (rows)
  (let* ((width (length (first rows)))
         (height (length rows))
         (padding (make-list (- *dimensions* 2) :initial-element 0))
         (cells (loop for y upto height for row in rows nconc
                      (loop for x upto width for c across row
                            when (eql c #\#) collect (append
                                                       (list x y)
                                                       padding)))))
    (make-hset cells :test 'equal)))

(defun play (data dimensions)
  (let* ((*dimensions* dimensions)
         (*neighbors-deltas* (neighbors-deltas dimensions))
         (game (parse-game data)))
    (dotimes (n 6 (hset-size game))
      (setf game (next game)))))

(define-solution (2020 17) (data)
  (values (play data 3) (play data 4)))

(define-test (2020 17) (315 1520))
