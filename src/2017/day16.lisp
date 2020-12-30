(defpackage :aoc/2017/16 #.cl-user::*aoc-use*)
(in-package :aoc/2017/16)

(defstruct (dancefloor
             (:constructor make-dancefloor%)
             (:copier NIL)
             (:conc-name df-))
  size
  spun
  name-to-position
  position-to-name)

(defun make-dancefloor (&optional (size 16))
  (let ((position-to-name (make-hash-table))
        (name-to-position (make-hash-table)))
    (loop
      :for pos :below size
      :for char :across "abcdefghijklmnopqrstuvwxyz"
      :do (progn
            (hash-table-insert position-to-name pos char)
            (hash-table-insert name-to-position char pos)))
    (make-dancefloor% :size size
                      :spun 0
                      :name-to-position name-to-position
                      :position-to-name position-to-name)))

(defun copy-dancefloor (df)
  (let ((name-to-position (copy-hash-table (df-name-to-position df)))
        (position-to-name (copy-hash-table (df-position-to-name df))))
    (make-dancefloor% :size (df-size df)
                      :spun (df-spun df)
                      :name-to-position name-to-position
                      :position-to-name position-to-name)))

(defun print-dancefloor (df &aux (size (df-size df)))
  (loop
    :for i :below size
    :for pos = (mod (- i (df-spun df)) size)
    :collect (gethash pos (df-position-to-name df)) :into names
    :finally (return (apply #'mkstr names))))

(defun dancefloor-spin (df n)
  (incf (df-spun df) n))

(defun dancefloor-exchange (df pos-a pos-b)
  (let* ((pos-a (mod (- pos-a (df-spun df)) (df-size df)))
         (pos-b (mod (- pos-b (df-spun df)) (df-size df)))
         (name-a (gethash pos-a (df-position-to-name df)))
         (name-b (gethash pos-b (df-position-to-name df))))
    (setf (gethash pos-b (df-position-to-name df)) name-a
          (gethash pos-a (df-position-to-name df)) name-b
          (gethash name-a (df-name-to-position df)) pos-b
          (gethash name-b (df-name-to-position df)) pos-a)))

(defun dancefloor-partner (df name-a name-b)
  (let* ((pos-a (gethash name-a (df-name-to-position df)))
         (pos-b (gethash name-b (df-name-to-position df))))
    (setf (gethash pos-b (df-position-to-name df)) name-a
          (gethash pos-a (df-position-to-name df)) name-b
          (gethash name-a (df-name-to-position df)) pos-b
          (gethash name-b (df-name-to-position df)) pos-a)))

(defun parse-moves (x)
  (labels ((parse-spin (s &aux (n (parse-integer (subseq s 1))))
             (partial-1 #'dancefloor-spin _ n))
           (parse-exchange (s &aux (splits (split-sequence:split-sequence #\/ (subseq s 1))))
             (let ((a (parse-integer (first splits)))
                   (b (parse-integer (second splits))))
               (partial-1 #'dancefloor-exchange _ a b)))
           (parse-partner (s &aux (splits (split-sequence:split-sequence #\/ (subseq s 1))))
             (let ((a (aref (first splits) 0))
                   (b (aref (second splits) 0)))
               (partial-1 #'dancefloor-partner _ a b)))
           (parse-move (s)
             (cond ((position #\s s) (parse-spin s))
                   ((position #\x s) (parse-exchange s))
                   (T (parse-partner s)))))
    (mapcar #'parse-move (split-sequence:split-sequence #\, (first x)))))

(defun do-the-dance (df moves)
  (loop
    :for m :in moves
    :do (funcall m df)
    :finally (return df)))

(define-solution (2017 16) (data parse-moves)
  (values
    (let ((df (make-dancefloor)))
      (print-dancefloor (do-the-dance df data)))
    (let ((df (make-dancefloor)))
      (destructuring-bind (cycles-at cycle-size df)
          (floyd (partial-1 #'do-the-dance _ data) df
                 :copier #'copy-dancefloor
                 :key #'print-dancefloor
                 :test 'string=)
        (let ((remaining (mod (- 1000000000 cycles-at) cycle-size)))
          (dotimes (n remaining (print-dancefloor df))
            (do-the-dance df data)))))))

(define-test (2017 16) ("hmefajngplkidocb" "fbidepghmjklcnoa"))
