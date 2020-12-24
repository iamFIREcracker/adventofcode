(defpackage :aoc/2020/24 #.cl-user::*aoc-use*)
(in-package :aoc/2020/24)

(defparameter *steps*
  '(("nw"  #c(-1/2 3/4))
    ("ne" #c(1/2 3/4))
    ("e"  #c(1 0))
    ("se" #c(1/2 -3/4))
    ("sw" #c(-1/2 -3/4))
    ("w"  #c(-1 0))))

(defun parse-step (string)
  (second (assoc string *steps* :test #'string=)))

(defun parse-instruction (string)
  (remove nil (mapcar #'parse-step
                     (cl-ppcre:split "(ne|nw|se|sw|w|e)" string
                                     :with-registers-p t))))

(defun parse-instructions (data)
  (mapcar #'parse-instruction data))

(defun create-floor (instructions)
  (loop with flipped = (make-hset '())
        for steps in instructions for delta = (reduce #'+ steps)
        if (hset-contains-p delta flipped) do (hset-rem delta flipped)
        else do (hset-add delta flipped)
        finally (return flipped)))

(defparameter *neighbors-deltas*
  '(#c(-1/2 3/4) #c(1/2 3/4) #c(1 0) #c(1/2 -3/4) #c(-1/2 -3/4) #c(-1 0)))

(defun neighbors (pos)
  (loop for delta in *neighbors-deltas* collect (+ pos delta)))

(defun should-be-flipped-p (pos n state)
  (or (= n 2) (and (= n 1) (hset-contains-p pos state))))

(define-solution (2020 24) (instructions parse-instructions)
  (let ((floor (create-floor instructions)))
    (values (hset-size floor)
            (dotimes (n 100 (hset-size floor))
              (setf floor
                    (gol:next floor :neighbors #'neighbors
                              :should-be-alive-p #'should-be-flipped-p))))))

(define-test (2020 24) (275 3537))
