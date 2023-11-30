(defpackage :aoc/2021/12 #.cl-user::*aoc-use*)
(in-package :aoc/2021/12)


(defun parse-map (data &aux (map (make-hash-table)))
  (dolist (s data map)
    (destructuring-bind (from . to) (parse-line s)
      (push to (gethash from map))
      (push from (gethash to map)))))

(defun parse-line (string)
  (cl-ppcre:register-groups-bind ((#'make-keyword from to))
      ("(\\w+)-(\\w+)" string)
    (cons from to)))


(defun walk (map &optional (visitablep #'visitablep))
  (looping
    (labels ((recur (path)
               (cond ((eq (car path) :|end|) (count! t))
                     (t (loop for next in (gethash (car path) map)
                              when (funcall visitablep path next) do
                              (recur (cons next path)))))))
      (recur '(:|start|)))))

(defun visitablep (path next) (or (big-cave-p next) (not (member next path))))
(defun big-cave-p (cave) (not (small-cave-p cave)))
(defun small-cave-p (cave) (loop for ch across (string cave) thereis (lower-case-p ch)))


(defun part2-visitable-p (path next)
  (cond ((big-cave-p next) t)
        ((eq next :|start|) nil)
        (t (<= (loop for p on (cons next path) for (c) = p
                     when (small-cave-p c) count (> (count c p) 1))
               1))))


(define-solution (2021 12) (map parse-map)
  (values (walk map) (walk map #'part2-visitable-p)))

(define-test (2021 12) (3000 74222))
