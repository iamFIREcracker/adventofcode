(defpackage :aoc/2016/08 #.cl-user::*aoc-use*)
(in-package :aoc/2016/08)

(defun parse-draw-rect (string)
  (cl-ppcre:register-groups-bind ((#'parse-integer width height))
      ("rect (\\d+)x(\\d+)" string)
    (list :draw-rect height width)))

(defun parse-rotate-row (string)
  (cl-ppcre:register-groups-bind ((#'parse-integer i shift))
      ("rotate row y=(\\d+) by (\\d+)" string)
    (list :rotate-row i shift)))

(defun parse-rotate-column (string)
  (cl-ppcre:register-groups-bind ((#'parse-integer j shift))
      ("rotate column x=(\\d+) by (\\d+)" string)
    (list :rotate-column j shift)))

(defun parse-single-instruction (string)
  (or (parse-draw-rect string)
      (parse-rotate-row string)
      (parse-rotate-column string)))

(defun parse-instructions (data)
  (mapcar #'parse-single-instruction data))

(defun draw-rect (screen height width)
  (dotimes (i height)
    (dotimes (j width)
      (setf (aref screen i j) #\#))))

(defun rotate-row (screen i shift &aux (n (array-dimension screen 1)))
  (let ((new-values (make-array n :element-type 'character)))
    (loop :for j :below n
          :for k = (mod (+ j shift) n)
          :do (setf (aref new-values k) (aref screen i j)))
    (loop :for v :across new-values
          :for j :from 0
          :do (setf (aref screen i j) v))))

(defun rotate-column (screen j shift &aux (n (array-dimension screen 0)))
  (let ((new-values (make-array n :element-type 'character)))
    (loop :for i :below n
          :for k = (mod (+ i shift) n)
          :do (setf (aref new-values k) (aref screen i j)))
    (loop :for v :across new-values
          :for i :from 0
          :do (setf (aref screen i j) v))))

(defun count-lit-pixels (screen)
  (loop :for i :below (array-total-size screen)
        :count (eql (row-major-aref screen i) #\#)))

(defun print-screen (screen)
  (with-output-to-string (s)
    (terpri s) ; print an additional newline, so I can better format the expected string
    (let ((height (array-dimension screen 0))
          (width (array-dimension screen 1)))
      (dotimes (i height)
        (dotimes (j width)
          (princ (aref screen i j) s))
        (terpri s)))))

(define-solution (2016 8) (instructions parse-instructions)
  (loop :with screen = (make-array (list 6 50) :initial-element #\Space)
        :for (type . args) :in instructions
        :do (case type
              (:draw-rect (apply #'draw-rect screen args))
              (:rotate-row (apply #'rotate-row screen args))
              (:rotate-column (apply #'rotate-column screen args)))
        :finally (return (values (count-lit-pixels screen)
                                 (print-screen screen)))))

(define-test (2016 8) (121  "
###  #  # ###  #  #  ##  ####  ##  ####  ### #    
#  # #  # #  # #  # #  # #    #  # #      #  #    
#  # #  # #  # #  # #    ###  #  # ###    #  #    
###  #  # ###  #  # #    #    #  # #      #  #    
# #  #  # # #  #  # #  # #    #  # #      #  #    
#  #  ##  #  #  ##   ##  ####  ##  ####  ### #### 
"))
