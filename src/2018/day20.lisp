(defpackage :aoc/2018/20 #.cl-user::*aoc-use*)
(in-package :aoc/2018/20)

(defun parse-doors (data &aux (string (first data)) (doors (make-hset nil)))
  (loop :with pos = 0 :with pos-stack = (list pos)
        :for ch :across string
        :do (ecase ch
              ((#\^) nil)
              ((#\$) (return doors))
              ((#\() (push pos pos-stack))
              ((#\)) (setf pos (pop pos-stack)))
              ((#\|) (setf pos (first pos-stack)))
              ((#\N) (hset-add (+ pos (complex 0 1)) doors) (setf pos (+ pos (complex 0 2))))
              ((#\E) (hset-add (+ pos (complex 1 0)) doors) (setf pos (+ pos (complex 2 0))))
              ((#\S) (hset-add (- pos (complex 0 1)) doors) (setf pos (- pos (complex 0 2))))
              ((#\W) (hset-add (- pos (complex 1 0)) doors) (setf pos (- pos (complex 2 0))))))
  doors)


(defparameter *deltas* '(#C(0 1) #C(1 0) #C(0 -1) #C(-1 0)))

(defun neighbors (doors pos)
  (loop :for d :in *deltas*
        :for door-pos = (+ pos d)
        :when (hset-contains-p door-pos doors)
        :collect (+ pos d d)))

(define-solution (2018 20) (doors parse-doors)
  (let ((cost-so-far (nth-value 3 (bfs 0 :neighbors (partial-1 #'neighbors doors)))))
    (values
      (reduce #'max (hash-table-values cost-so-far))
      (count-if (partial-1 #'>= _ 1000) (hash-table-values cost-so-far)))))

(define-test (2018 20) (3835 8520))

(test-run)
