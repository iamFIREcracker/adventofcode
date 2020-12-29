(defpackage :aoc/2017/19 #.cl-user::*aoc-use*)
(in-package :aoc/2017/19)

(defun parse-tube-map (x)
  (let ((entry (position #\| (first x)))
        (height (length x))
        (width (reduce #'max x :key #'length)))
    (values (complex entry 0)
            #C(0 -1)
            (make-array (list height width) :initial-contents x))))

(defun tubes-get (tubes pos)
  (let ((y (- (imagpart pos)))
        (x (realpart pos)))
    (if (array-in-bounds-p tubes y x)
      (aref tubes y x))))

(defun letterp (c &aux (n (char-int c)))
  (and (>= n (char-int #\A))
       (<= n (char-int #\Z))))

(defun straightp (c)
  (member c (list #\| #\-)))

(defun cornerp (c)
  (char= c #\+))

(defun change-direction (tubes curr dir)
  (let ((opts (list (complex-rotate-ccw dir) (complex-rotate-cw dir))))
    (loop
      :for new-dir :in opts
      :for new-pos = (+ curr new-dir)
      :for c = (tubes-get tubes new-pos)
      :when (and c (not (char= c #\Space)))
      :do (return (values new-pos new-dir)))))

(define-solution (2017 19) (data)
  (multiple-value-bind (curr dir tubes) (parse-tube-map data)
    (recursively ((curr curr)
                  (dir dir)
                  letters
                  (steps 0))
      (let ((c (tubes-get tubes curr)))
        (cond ((letterp c) (recur (+ curr dir) dir (cons c letters) (1+ steps)))
              ((straightp c) (recur (+ curr dir) dir letters (1+ steps)))
              ((cornerp c) (multiple-value-bind
                               (curr dir)
                               (change-direction tubes curr dir)
                             (recur curr dir letters (1+ steps))))
              (T (values (apply #'mkstr (reverse letters))
                         steps)))))))

(define-test (2017 19) ("MKXOIHZNBL"  17872))
