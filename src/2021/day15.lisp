(defpackage :aoc/2021/15 #.cl-user::*aoc-use*)
(in-package :aoc/2021/15)


(defun parse-risk-levels (data &aux (levels (make-hash-table :test 'equal)))
  (loop for r below (length data)
        for string in data do
        (loop for c below (length string)
              for ch across string do
              (setf (gethash (list r c) levels)
                    (- (char-code ch) (char-code #\0)))))
  levels)


(defun lowest-risk (levels &aux
                           (row-max (loop for (r _) being the hash-keys of levels maximize r))
                           (col-max (loop for (_ c) being the hash-keys of levels maximize c))
                           (end (list row-max col-max)))
  (search-cost (a* '(0 0)
                   :goal-state end
                   :test 'equal
                   :neighbors (partial-1 #'neighbors levels)
                   :heuristic (partial-1 #'manhattan-distance end))))


(defun neighbors (levels p)
  (loop for n in (adjacents p)
        for level = (gethash n levels)
        when level collect (cons n level)))


(defun massage (levels &aux
                       (rows (1+ (loop for (r _) being the hash-keys of levels maximize r)))
                       (cols (1+ (loop for (_ c) being the hash-keys of levels maximize c)))
                       (rez (make-hash-table :test 'equal)))
  ;; Extend the map -- horizontally first
  (loop for (r c) being the hash-keys of levels using (hash-value risk) do
        (loop for i below 5
              for c1 = (+ c (* cols i))
              for risk1 = (+ risk (* 1 i)) do
              (setf (gethash (list r c1) rez) (if (> risk1 9) (- risk1 9) risk1))))
  ;; Then vertically
  (setf levels (copy-hash-table rez)) ;; copy because we are changing it as we scan it
  (loop for (r c) being the hash-keys of levels using (hash-value risk) do
        (loop for i below 5
              for r1 = (+ r (* rows i))
              for risk1 = (+ risk (* 1 i)) do
              (setf (gethash (list r1 c) rez) (if (> risk1 9) (- risk1 9) risk1))))
  rez)


(define-solution (2021 15) (risk-levels parse-risk-levels)
  (values (lowest-risk risk-levels) (lowest-risk (massage risk-levels))))

(define-test (2021 15) (745 3002))
