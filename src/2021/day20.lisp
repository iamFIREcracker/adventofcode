(defpackage :aoc/2021/20 #.cl-user::*aoc-use*)
(in-package :aoc/2021/20)


(defun parse-input (data)
  (cons (parse-enhancement-algorightm (car data)) (parse-image (cddr data))))

(defun parse-enhancement-algorightm (string)
  (make-array (length string) :element-type 'bit
              :initial-contents (loop for ch across string
                                      collect (as-bit ch))))

(defun parse-image (data &aux (rows (length data)) (cols (length (car data))))
  (make-array (list rows cols) :element-type 'bit
              :initial-contents (loop for string in data collect
                                      (loop for ch across string
                                            collect (as-bit ch)))))

(defun as-bit (ch) (if (eql ch #\#) 1 0))


(defun enhance (iterations input)
  (destructuring-bind (algo . curr) input
    (dotimes (i iterations)
      (setf curr (enhance-step algo curr (if (oddp i) 1 0))))
    (loop for i below (array-total-size curr) count (= (row-major-aref curr i) 1))))


(defun enhance-step (algo curr bgcolor &aux
                          (rows (array-dimension curr 0))
                          (cols (array-dimension curr 1))
                          (next (make-array (list (+ rows 2) (+ cols 2))
                                            :element-type 'bit)))
  (labels ((pixel (i j)
             (if (array-in-bounds-p curr i j) (aref curr i j) bgcolor))
           (index (i j &aux (pos 8) (rez 0))
             (loop for ii from (1- i) upto (1+ i) do
                   (loop for jj from (1- j) upto (1+ j)
                         do (setf rez (dpb (pixel ii jj) (byte 1 pos) rez)
                                  pos (1- pos))))
             rez))
    (dotimes (i (+ rows 2))
      (dotimes (j (+ cols 2))
        (setf (aref next i j) (aref algo (index (1- i) (1- j)))))))
  next)


#+#:excluded (defun print-image (image)
  (destructuring-bind (rows cols) (array-dimensions image)
    (dotimes (row rows)
      (dotimes (col cols)
        (princ (if (= (aref image row col) 1) #\# #\.)))
      (terpri))))


(define-solution (2021 20) (input parse-input)
  (values (enhance 2 input) (enhance 50 input)))

(define-test (2021 20) (5503 19156))
