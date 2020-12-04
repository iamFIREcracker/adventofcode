(defpackage :aoc/2018/11 #.cl-user::*aoc-use*)
(in-package :aoc/2018/11)

(defun power-level (x y serial-number)
  (let ((rack-id (+ x 10)))
    (->< rack-id
      (* >< y)
      (+ >< serial-number)
      (* >< rack-id)
      (mod (floor >< 100) 10)
      (- >< 5))))

(defun power-grid (size serial-number)
  (let* ((grid (make-array `(,size ,size))))
    (dotimes (y size)
      (dotimes (x size)
        (setf (aref grid y x) (power-level (1+ x) (1+ y) serial-number))))
    grid))

(defun max-square-of-energy (st square-size &aux best-pos best-value)
  (loop
    :with grid-size = (array-dimension st 0)
    :for top :from 0 :below (- grid-size (1- square-size))
    :do (loop
          :for left :from 0 :below (- grid-size (1- square-size))
          :for bottom = (+ top (1- square-size))
          :for right = (+ left (1- square-size))
          :for power = (st-area-of st top left bottom right)
          :do (when (or (not best-value) (> power best-value))
                (setf best-pos (list left top)
                      best-value power))))
  (values best-pos best-value))

(define-solution (2018 11) (data read-integer)
  (let* ((grid (power-grid 300 data))
         (st (make-summedarea-table grid)))
    (values
      (destructuring-bind (x y) (max-square-of-energy st 3)
        (mkstrc (1+ x) (1+ y)))
      (loop
        :with best-pos
        :with best-value
        :with best-size
        :for size :from 1 :to 300
        :do (multiple-value-bind (pos value) (max-square-of-energy st size)
              (when (or (not best-value) (> value best-value))
                (setf best-pos pos
                      best-value value
                      best-size size)))
        :finally (return (destructuring-bind (x y) best-pos
                           (mkstrc (1+ x) (1+ y) best-size)))))))

(define-test (2018 11) ("21,41" "227,199,19"))
