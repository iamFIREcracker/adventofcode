(defpackage :aoc/2024/13 #.cl-user::*aoc-use*)
(in-package :aoc/2024/13)

#;
(sb-ext:gc :full t)
(defun parse-input (&optional (strings (uiop:read-file-lines #P"src/2024/day13.txt")))
  (looping
    (doseq (configuration (split-sequence:split-sequence "" strings :test 'equal))
     (collect! (mapcar #'extract-positive-integers configuration)))))
#+#:excluded (parse-input)

(defun move-straight (pos dir) (mapcar #'+ pos dir))
(defun overshoot? (curr target)
  (or (> (car curr) (car target))
      (> (cadr curr) (cadr target))))

(defun minimize-presses (a b prize &optional (start (list 0 0)))
  (a* start
      :goal-state prize :test 'equal
      :neighbors (fn (curr)
                   (looping
                     (doseqs ((dir (list a b))
                              (cost (list 3 1)))
                       (let1 next (move-straight curr dir)
                         (unless (overshoot? next prize)
                           (collect! (cons next cost)))))))
      :heuristic [manhattan-distance _ prize]))

(looping
  (doseq ((a b prize) (parse-input))
    (awhen (search-cost (minimize-presses a b prize))
      (sum! it))))
39290!

;; bruteforce
(defun solve (a b price)
  (destructuring-bind (ax ay) a
    (destructuring-bind (bx by) b
      (destructuring-bind (px py) price
        (looping
          (dorangei (n 0 100)
            (dorangei (m 0 100)
              (when (and (= (+ (* ax n) (* bx m)) px)
                         (= (+ (* ay n) (* by m)) py))
                (minimize! (+ (* n 3) m))))))))))
(looping
  (doseq ((a b prize) (parse-input))
    (awhen (solve a b prize)
      (sum! it))))

;; Math
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
(assert
  (= (looping
       (doseq ((a b prize) (parse-input))
         (awhen (solve a b prize)
           (sum! it))))
     39290))

(looping
  (doseq ((a b (px py)) (parse-input))
    (awhen (solve a b (list (+ px 10000000000000) (+ py 10000000000000)))
      (sum! it))))
73458657399094!
