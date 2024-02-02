(defpackage :aoc/2023/14 #.cl-user::*aoc-use*)
(in-package :aoc/2023/14)


(defparameter *north* '(-1 0))
(defparameter *east*  '(0 1))
(defparameter *south* '(1 0))
(defparameter *west*  '(0 -1))

(defun parse-input (&optional (strings (aoc::read-problem-input 2023 14)))
  (bnd* ((rows (length strings))
         (cols (length (first strings)))
         (rocks (make-hash-table :test 'equal))
         (walls (make-hash-table :test 'equal)))
    (doseq ((i s) (enumerate strings))
      (doseq ((j ch) (enumerate s))
        (when (char= ch #\O)
          (setf (gethash (list i j) rocks) ch))
        (when (char= ch #\#)
          (setf (gethash (list i j) walls) ch))))
    (list rocks walls rows cols)))


(defun north-first? (pos1 pos2) (< (first pos1) (first pos2)))
(defun west-first? (pos1 pos2) (< (second pos1) (second pos2)))
(defun south-first? (pos1 pos2) (> (first pos1) (first pos2)))
(defun east-first? (pos1 pos2) (> (second pos1) (second pos2)))


(defun move (dir sort-predicate &optional (input (parse-input)))
  (destructuring-bind (rocks walls rows cols) input
    (flet ((outside? (pos)
             (or (< (first pos) 0) (>= (first pos) rows)
                 (< (second pos) 0) (>= (second pos) cols))))
      (dolist (pos (sort (hash-table-keys rocks) sort-predicate))
        (bnd1 (npos (mapcar #'+ pos dir))
          (while (not (or (gethash npos walls)
                          (gethash npos rocks)
                          (outside? npos)))
            (remhash pos rocks)
            (setf (gethash npos rocks) #\O
                  pos npos
                  npos (mapcar #'+ pos dir)))))))
  input)


(defun north-load (input)
  (destructuring-bind (rocks _1 rows _2) input
    (declare (ignore _1 _2))
    (reduce #'+ (hash-table-keys rocks) :key [- rows (car _)])))


(defun cycle (&optional (input (parse-input)))
  (move *north* #'north-first? input)
  (move *west* #'west-first? input)
  (move *south* #'south-first? input)
  (move *east* #'east-first? input))

(defun spin-cycle (n &optional (input (parse-input)))
  (destructuring-bind (cycles-at cycle-size input)
      (floyd #'cycle input
             :copier (lambda (state)
                       (destructuring-bind (rocks walls rows cols) state
                         (list (copy-hash-table rocks)
                               walls
                               rows
                               cols)))
             :key (lambda (state)
                    (destructuring-bind (rocks _ rows cols) state
                      (declare (ignore _))
                      (looping
                        (dorange (i 0 rows)
                          (dorange (j 0 cols)
                            (when (gethash (list i j) rocks)
                              (collect! (list i j))))))))
             :test 'equalp)
    (bnd1 (rem-steps (rem (- n cycles-at) cycle-size))
      (repeat rem-steps
        (cycle input))
      input)))


(define-solution (2023 14) (input parse-input)
  (values (north-load (move *north* #'north-first? input))
          (north-load (spin-cycle 1000000000 input))))

(define-test (2023 14) (109345 112452))
