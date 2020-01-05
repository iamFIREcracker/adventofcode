(defpackage :aoc/2019/08 #.cl-user::*aoc-use*)
(in-package :aoc/2019/08)

(defparameter *image-width* 25)
(defparameter *image-height* 6)

(defun read-image (data &aux (str (first data)))
  (let* ((layer-size (* *image-width* *image-height*))
         (num-layers (floor (length str) layer-size)))
    (gathering
      (dotimes (n num-layers)
        (gather (subseq str (* n layer-size) (* (1+ n) layer-size)))))))

(defun solve-part1 (image)
  (let ((best (minimizing image :key (partial-1 #'count #\0))))
    (* (count #\1 best) (count #\2 best))))

(defun reduce-layers (image)
  (gathering
    (dotimes (i 150)
      (loop
        :for layer :in image
        :for pixel = (aref layer i)
        :when (eql #\0 pixel) :return (gather #\Space)
        :when (eql #\1 pixel) :return (gather #\X)))))

(defun print-layer (layer)
  (with-output-to-string (s)
    (format s "~%") ;; add a leading new-line, to make testing easier/nicer
    (loop
      :for pixel :in layer
      :for i = 0 :then (1+ i)
      :do (format s "~a" pixel)
      :when (= 24 (mod i *image-width*)) :do (format s "~&"))))

(defun solve-part2 (image)
  (print-layer
    (loop
      :for i :from 0 :below 150
      :collect (loop
                 :for layer :in image
                 :for pixel = (aref layer i)
                 :when (eql #\0 pixel) :return #\Space
                 :when (eql #\1 pixel) :return #\#))))

(define-problem (2019 8) (image read-image)
  (values
    (solve-part1 image)
    (solve-part2 image)))

(1am:test test-2019/08
  (multiple-value-bind (part1 part2) (problem-run)
    (1am:is (= 2684 part1))
    (1am:is (string=

"
#   # ##  ###  #   ##### 
#   ##  # #  # #   #   # 
 # # #    #  #  # #   #  
  #  # ## ###    #   #   
  #  #  # # #    #  #    
  #   ### #  #   #  #### 
"
part2))))
