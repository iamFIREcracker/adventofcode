(defpackage :aoc/2015/14 #.cl-user::*aoc-use*)
(in-package :aoc/2015/14)

(defun parse-reindeer (string)
  (cl-ppcre:register-groups-bind (name (#'parse-integer speed run-for rest-for))
      ("(\\w+) can fly (\\d+) km/s for (\\d+) seconds, but then must rest for (\\d+) seconds." string)
    (list name speed run-for rest-for)))

(defun parse-reindeers (lines) (mapcar #'parse-reindeer lines))

(defun traveled-distance (time reindeer)
  (destructuring-bind (speed run-for rest-for) (rest reindeer)
    (let* ((period (+ run-for rest-for))
           (cycles (floor time period))
           (remaining (mod time period)))
      (* speed (+ (* cycles run-for) (min remaining run-for))))))

(defun part1 (reindeers)
  (reduce #'max reindeers :key (partial-1 #'traveled-distance 2503)))

(defun part2 (reindeers)
  (loop repeat 2503
        with scores = (make-list (length reindeers) :initial-element 0)
        with distances = (make-list (length reindeers) :initial-element 0)
        with run-fors = (make-list (length reindeers) :initial-element 0)
        with rest-fors = (make-list (length reindeers) :initial-element 0)
        do (loop for reindeer in reindeers for index from 0
                 for (speed run-for rest-for) = (cdr reindeer) ; skip name
                 if (< (nth index run-fors) run-for)
                    do (incf (nth index distances) speed)
                       (incf (nth index run-fors))
                 else if (< (nth index rest-fors) rest-for)
                    do (incf (nth index rest-fors))
                 if (= (nth index rest-fors) rest-for)
                    do (setf (nth index run-fors) 0
                             (nth index rest-fors) 0))
        do (loop with furthest = (reduce #'max distances)
                 for d in distances for index from 0
                 when (= d furthest) do (incf (nth index scores)))
        finally (return (reduce #'max scores))))

(define-solution (2015 14) (reindeers parse-reindeers)
  (values (part1 reindeers) (part2 reindeers)))

(define-test (2015 14) (2660 1256))
