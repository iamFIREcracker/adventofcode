(defpackage :aoc/2023/01 #.cl-user::*aoc-use*)
(in-package :aoc/2023/01)

(defun char->int (ch) (- (char-code ch) (char-code #\0)))

(defun calibration-value (s)
  (+ (* (char->int (find-if #'digit-char-p s)) 10)
     (char->int (find-if #'digit-char-p s :from-end t))))
#+#:excluded (mapcar #'calibration-value (uiop:read-file-lines #P"src/2023/day01.txt"))
#+#:excluded (reduce #'+ *)

; Note: FORMAT's ~r directive lets you spell out numbers in English
(defparameter *digits-prefixes*  (loop for i from 1 upto 9
                                       collect (cons (format nil "~a" i) i)
                                       collect (cons (format nil "~r" i) i)))

(defun calibration-value-p2 (s)
  (flet ((extract-all-digits ()
           (looping
             (dotimes (start (length s))
               (dolist+ ((prefix . d) *digits-prefixes*)
                 (if (string-starts-with-p prefix (subseq s start))
                   (collect! d)))))))
    (bnd1 (digits (extract-all-digits))
      (+ (* (first digits) 10) (car (last digits))))))
#+#:excluded (calibration-value-p2 "7pqrstsixteen")
#+#:excluded (calibration-value-p2 "eightwo")

(define-solution (2023 01) (strings)
  (values (reduce #'+ (mapcar #'calibration-value strings))
          (reduce #'+ (mapcar #'calibration-value-p2 strings))))

(define-test (2023 01) (54630 54770))
