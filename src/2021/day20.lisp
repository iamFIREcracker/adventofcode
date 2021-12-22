(defpackage :aoc/2021/20 #.cl-user::*aoc-use*)
(in-package :aoc/2021/20)


(defun parse-input (data)
  (list (parse-enhancement-algorightm (first data)) (parse-image (cddr data))))

(defun parse-enhancement-algorightm (string)
  (map 'vector (partial-1 #'eql #\#) string))

(defun parse-image (data &aux (image (make-hash-table :test 'equal)))
  (loop for r below (length data)
        for string in data do
        (loop for c below (length string)
              for ch across string
              when (eql ch #\#) do (setf (gethash (list r c) image) t)))
  image)

(defun enhancement-algorithm (input) (first input))
(defun image (input) (second input))


(defun enhance (iterations input &aux (algo (enhancement-algorithm input)) (curr (image input)))
  (dotimes (i iterations)
    (setf curr (enhance-step algo curr (oddp i))))
  (loop for state being the hash-values of curr count state))


(defun enhance-step (algo curr background-lit-p &aux
                          (next (make-hash-table :test 'equal))
                          (row-min (loop for (r _) being the hash-keys of curr minimize r))
                          (row-max (loop for (r _) being the hash-keys of curr maximize r))
                          (col-min (loop for (_ c) being the hash-keys of curr minimize c))
                          (col-max (loop for (_ c) being the hash-keys of curr maximize c)))
  (flet ((block-3x3 (pos)
           (mapcar (partial-1 #'gethash _ curr background-lit-p)
                   (neighbors9 pos))))
    (loop for row from (- row-min 3) to (+ row-max 3) do
          (loop for col from (- col-min 3) to (+ col-max 3)
            for pos = (list row col)
            for i = (as-binary (block-3x3 pos)) do
            (setf (gethash pos next) (aref algo i)))))
  next)


(defparameter *nhood* '((-1 -1) (-1  0) (-1 1)
                        ( 0 -1) ( 0  0) ( 0 1)
                        ( 1 -1) ( 1  0) ( 1 1)))

(defun neighbors9 (pos)
  (loop for d in *nhood* collect (mapcar #'+ pos d)))


(defun as-binary (list &aux (number 0))
  (loop for ch in list for i from 8 downto 0 when ch do
        (setf number (dpb 1 (byte 1 i) number)))
  number)


#+#:excluded (defun print-image (image &aux
                          (row-min (loop for (r _) being the hash-keys of image minimize r))
                          (row-max (loop for (r _) being the hash-keys of image maximize r))
                          (col-min (loop for (_ c) being the hash-keys of image minimize c))
                          (col-max (loop for (_ c) being the hash-keys of image maximize c)))
  (loop for row from (- row-min 2) to (+ row-max 2) do
        (loop for col from (- col-min 2) to (+ col-max 2) do
              (princ (if (gethash (list row col) image) #\# #\.)))
        (terpri))
  (terpri))


(define-solution (2021 20) (input parse-input)
  (values (enhance 2 input) (enhance 50 input)))

(define-test (2021 20) (5503 19156))
