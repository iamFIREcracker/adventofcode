(defpackage :aoc/2021/13 #.cl-user::*aoc-use*)
(in-package :aoc/2021/13)


(defun parse-input (data &aux
                         (dots (make-hash-table :test 'equal))
                         folds)
  (dolist (s data (list dots (reverse folds)))
    (when-let (point (parse-dot s))
      (setf (gethash point dots) t))
    (when-let (fold (parse-fold s))
      (push fold folds))))
(defun dots (data) (car data))
(defun folds (data) (cadr data))

(defun parse-dot (string)
  (cl-ppcre:register-groups-bind ((#'parse-integer col row))
      ("(\\d+),(\\d+)" string)
    (list row col)))

(defun parse-fold (string)
  (cl-ppcre:register-groups-bind ((#'as-keyword axis) (#'parse-integer n))
      ("fold along (\\w)=(\\d+)" string)
    (list axis n)))


(defun fold (input &aux (curr (dots input)) part1)
  (dolist (f (folds input))
    (let ((next (make-hash-table :test 'equal)))
      (destructuring-bind (axis n) f
        (loop for (row col) being the hash-keys of curr do
              (ecase axis
                (:x (when (> col n) (decf col (* (- col n) 2))))
                (:y (when (> row n) (decf row (* (- row n) 2)))))
              (setf (gethash (list row col) next) t)))
      (setf curr next)
      (unless part1
        (setf part1 (hash-table-count curr)))))
  (values part1 (print-paper curr)))


(defun print-paper (dots &aux
                         (rows (loop for d being the hash-keys of dots maximize (car d)))
                         (cols (loop for d being the hash-keys of dots maximize (cadr d))))
  (with-output-to-string (s)
    (terpri s) ; print an additional newline, so I can better format the expected string
    (dotimes (row (1+ rows))
      (dotimes (col (1+ cols))
        (princ (if (gethash (list row col) dots) #\# #\Space) s))
      (terpri s))))


(define-solution (2021 13) (input parse-input) (fold input))

(define-test (2021 13) (724 "
 ##  ###    ## ###  #### ###  #  # #   
#  # #  #    # #  # #    #  # #  # #   
#    #  #    # ###  ###  #  # #  # #   
#    ###     # #  # #    ###  #  # #   
#  # #    #  # #  # #    # #  #  # #   
 ##  #     ##  ###  #### #  #  ##  ####
"))
