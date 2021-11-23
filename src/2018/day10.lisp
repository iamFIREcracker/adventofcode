(defpackage :aoc/2018/10 #.cl-user::*aoc-use*)
(in-package :aoc/2018/10)


(defstruct (star (:conc-name nil)) pos vel)

(defun parse-sky (data)
  (mapcar #'parse-star data))

(defun parse-star (string)
  (destructuring-bind (x y vx vy)
      (mapcar #'parse-integer (cl-ppcre:all-matches-as-strings "-?\\d+" string))
    (make-star :pos (list x y) :vel (list vx vy))))


(defun find-message (input)
  (loop :for time :from 0
        :for sky-prev = input :then sky
        :for sky = input :then (sky-next sky)
        :when (> (sky-area sky) (sky-area sky-prev))
        :return (values (sky-message sky-prev) (1- time))))

(defun sky-next (sky) (mapcar #'star-move sky))

(defun star-move (star)
  (with-slots (pos vel) star
    (make-star :pos (mapcar #'+ pos vel) :vel vel)))

(defun sky-area (sky)
  (destructuring-bind ((x-min x-max) (y-min y-max)) (sky-box sky)
    (* (- x-max x-min) (- y-max y-min))))

(defun sky-message (sky)
  (destructuring-bind ((x-min x-max) (y-min y-max)) (sky-box sky)
    (let ((output (loop :repeat (1+ (- y-max y-min))
                        :collect (make-string (1+ (- x-max x-min)) :initial-element #\Space))))
      (loop :for star :in sky :for (x y) = (pos star)
            :for row = (nth (- y y-min) output)
            :do (setf (aref row (- x x-min)) #\#))
      (with-output-to-string (s)
        (format s "~%~{~a~^~%~}" output)))))

(defun sky-box (sky)
  (loop :for star :in sky :for (x y) = (pos star)
        :minimize x :into x-min
        :maximize x :into x-max
        :minimize y :into y-min
        :maximize y :into y-max
        :finally (return (list (list x-min x-max) (list y-min y-max)))))


(define-solution (2018 10) (sky parse-sky)
  (find-message sky))

(define-test (2018 10) ("
#####   #    #  #####      ###   ####   #       #####   ######
#    #  #    #  #    #      #   #    #  #       #    #  #     
#    #  #    #  #    #      #   #       #       #    #  #     
#    #  #    #  #    #      #   #       #       #    #  #     
#####   ######  #####       #   #       #       #####   ##### 
#    #  #    #  #           #   #  ###  #       #       #     
#    #  #    #  #           #   #    #  #       #       #     
#    #  #    #  #       #   #   #    #  #       #       #     
#    #  #    #  #       #   #   #   ##  #       #       #     
#####   #    #  #        ###     ### #  ######  #       ######" 10831))
