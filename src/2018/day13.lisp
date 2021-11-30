(defpackage :aoc/2018/13 #.cl-user::*aoc-use*)
(in-package :aoc/2018/13)

(defstruct (cart (:conc-name nil)
                 (:constructor make-cart%))
  pos
  dir
  choices)

(defparameter *choices* (ncycle (list #C(0 1) #C(1 0) #C(0 -1))))

(defun make-cart (pos dir &optional (choices *choices*))
  (make-cart% :pos pos :dir dir :choices choices))


(defstruct (ct-system (:conc-name nil)
                      (:constructor make-ct-system%))
  width
  height
  track
  carts)

(defun make-ct-system (width height track carts)
  (make-ct-system% :width width
                   :height height
                   :track track
                   :carts carts))

(defparameter *up* #C(0 1))
(defparameter *right* #C(1 0))
(defparameter *down* #C(0 -1))
(defparameter *left* #C(-1 0))

(defun parse-system (data)
  (let ((height (length data))
        (width (length (first data)))
        (track (make-hash-table))
        (carts (make-hash-table)))
    (loop :for r :from 0 :for string :in data :do
          (loop :for c :from 0 :for ch :across string
                :for pos = (complex c (- r)) :do
                (ecase ch
                  ((#\/ #\- #\\ #\| #\+) (setf (gethash pos track) ch))
                  (#\> (setf (gethash pos track) #\-
                             (gethash pos carts) (make-cart pos *right*)))
                  (#\< (setf (gethash pos track) #\-
                             (gethash pos carts) (make-cart pos *left*)))
                  (#\^ (setf (gethash pos track) #\|
                             (gethash pos carts) (make-cart pos *up*)))
                  (#\v (setf (gethash pos track) #\|
                             (gethash pos carts) (make-cart pos *down*)))
                  (#\Space nil))))
    (make-ct-system width height track carts)))


(defun part1 (system)
  (loop
    (let ((crashes (system-tick system)))
      (when crashes
        (return (cart-pos->string (first crashes)))))))

(defun system-tick (system &aux collisions)
  (with-slots (carts track) system
    (dolist (cart (sort-carts (hash-table-values carts)) collisions)
      (unless (member cart collisions :test 'equalp)
        (let* ((pos-prev (pos cart))
               (pos (cart-move cart track))
               (collidesp (gethash pos carts)))
          (remhash pos-prev carts)
          (when collidesp
            (pushnew cart collisions)
            (pushnew (gethash pos carts) collisions)
            (remhash pos carts))
          (unless collidesp
            (setf (gethash pos carts) cart)))))))


(defun sort-carts (carts)
  (sort (copy-seq carts) #'complex< :key #'pos))

(defun complex< (c1 c2)
  (or (> (imagpart c1) (imagpart c2))
      (and (= (imagpart c1) (imagpart c2))
           (< (realpart c1) (realpart c2)))))

(defun cart-move (cart track)
  (declare (optimize (speed 3)))
  (with-slots (pos dir choices) cart
    (incf pos dir)
    (let ((ch (gethash pos track)))
      (cond ((and (char= ch #\\) (= dir *up*))
             (setf dir *left*))
            ((and (char= ch #\\) (= dir *right*))
             (setf dir *down*))
            ((and (char= ch #\\) (= dir *down*))
             (setf dir *right*))
            ((and (char= ch #\\) (= dir *left*))
             (setf dir *up*))
            ((and (char= ch #\/) (= dir *up*))
             (setf dir *right*))
            ((and (char= ch #\/) (= dir *right*))
             (setf dir *up*))
            ((and (char= ch #\/) (= dir *down*))
             (setf dir *left*))
            ((and (char= ch #\/) (= dir *left*))
             (setf dir *down*))
            ((and (char= ch #\+))
             (setf dir (* dir (first choices))
                   choices (rest choices)))
            ((find ch "-|") nil)
            (t (error "Unexpected: ~C" ch)))
      pos)))

(defun cart-pos->string (cart &aux (pos (pos cart)))
  (format nil "~D,~D" (realpart pos) (- (imagpart pos))))


(defun part2 (system)
  (loop
    (system-tick system)
    (let ((carts (hash-table-values (carts system))))
      (when (= (length carts) 1)
        (return (cart-pos->string (first carts)))))))


(define-solution (2018 13) (system parse-system)
  (values (part1 system) (part2 system)))

(define-test (2018 13) ("74,87" "29,74"))
