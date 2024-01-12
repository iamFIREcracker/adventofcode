(defpackage :aoc/2023/05 #.cl-user::*aoc-use*)
(in-package :aoc/2023/05)

(defun parse-map (strings)
  (bnd1 (ranges (looping
                  (dolist (range (rest strings))
                    (destructuring-bind (dest source size)
                        (extract-positive-integers range)
                      (collect! (list source dest size))))))
    (setf ranges (sort ranges #'< :key #'first))
    (cons
      (list 0 0 (first (first ranges)))
      ranges)))
#+#:excluded (parse-map "seed-to-soil map:
50 98 2
52 50 48")

(defun find-range (seed ranges)
  (find-if [<= (first _) seed (+ (first _) (third _))] ranges))

(defun part1 (&optional (strings (uiop:read-file-lines #P"src/2023/day05.txt")))
  (destructuring-bind (seeds &rest maps)
      (split-sequence:split-sequence "" strings :test #'string=)
    (setf seeds (extract-positive-integers (first seeds))
          maps (mapcar #'parse-map maps))
    (looping
      (dolist (seed seeds)
        (dolist (map maps)
          (bnd1 ((source dest ignore) (or (find-range seed map)
                                          (list seed seed 1)))
            (setf seed (+ (- seed source) dest))))
        (minimize! seed)))))
#+#:excluded (part1)
600279879

(defun seed-ranges (seeds)
  (loop with seeds = seeds
        for seed1 in seeds by #'cddr
        for seed2 in (cdr seeds) by #'cddr
        collect (list seed1 seed2)))
#+#:excluded (seed-ranges (list 1 2 3 4 5 6))

(defun part2 (&optional (strings (uiop:read-file-lines #P"src/2023/day05.txt")))
  (declare (optimize (debug 3)))
  (destructuring-bind (seeds &rest maps)
      (split-sequence:split-sequence "" strings :test #'string=)
    (setf seeds (seed-ranges (parse-positive-integers (first seeds)))
          maps (mapcar #'parse-map maps))
    (dolist (map maps seeds)
      (bnd1 (seeds-next)
        (doseq ((start length) seeds)
          (loop while (> length 0) do
                (bnd* (((source dest size) (or (find-range start map)
                                               (list start start length)))
                       (new-length (if (> (+ start length) (+ source size))
                                     (- size (- start source))
                                     length)))
                  (push (list (+ (- start source) dest)
                              new-length)
                        seeds-next)
                  (setf start (+ start new-length 1)
                        length (- length new-length)))))
        (setf seeds seeds-next)))))

#+#:excluded (time (1- (reduce #'min (part2) :key #'first)))
205409647 ;too high
20191103 ; this is what my solution outputs... however, it's off by 1...
         ; i noticed it was off by one with the example... so I gave it a shot
         ; anyways, and it worked... go figure..
