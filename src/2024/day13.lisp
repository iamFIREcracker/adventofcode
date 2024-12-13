(defpackage :aoc/2024/13 #.cl-user::*aoc-use*)
(in-package :aoc/2024/13)

(defun parse-input (&optional (strings (uiop:read-file-lines #P"src/2024/day13.txt")))
  (looping
    (doseq (configuration (split-sequence:split-sequence "" strings :test 'equal))
     (collect! (mapcar #'extract-positive-integers configuration)))))


(defun solve (a b price)
  (destructuring-bind (ax ay) a
    (destructuring-bind (bx by) b
      (destructuring-bind (px py) price
        ;; Px = n × Ax + m × Bx
        ;; Py = n × Ay + m × By
        (let1 m (/ (- (* py ax) (* px ay))
                   (- (* by ax) (* bx ay)))
          (let1 n (/ (- px (* m bx))
                     ax)
            (if (and (integerp m) (integerp n))
                (+ (* n 3) m))))))))



(define-solution (2024 13) (input parse-input)
  (values (looping
            (doseq ((a b prize) input)
              (awhen (solve a b prize)
                (sum! it))))
          (looping
            (doseq ((a b (px py)) input)
              (awhen (solve a b (list (+ px 10000000000000) (+ py 10000000000000)))
                (sum! it))))))

(define-test (2024 13) (39290 73458657399094))
