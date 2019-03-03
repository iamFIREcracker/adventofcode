(defpackage :aoc/2017/15 #.cl-user::*aoc-use*)
(in-package :aoc/2017/15)

(defun parse-generators (x)
  (flet ((parse-generator (s &aux (splits (split-sequence:split-sequence #\Space s)))
           (parse-integer (fifth splits))))
    (mapcar #'parse-generator x)))

(define-problem (2017 15) (data parse-generators)
  (labels ((next-a (curr)
             (mod (* curr 16807) 2147483647))
           (next-b (curr)
             (mod (* curr 48271) 2147483647))
           (next-a-part2 (curr &aux (next (next-a curr)))
             (if (dividesp 4 next)
               next
               (next-a-part2 next)))
           (next-b-part2 (curr &aux (next (next-b curr)))
             (if (dividesp 8 next)
               next
               (next-b-part2 next)))
           (numbers-match-p (a b)
             (= (logand a 65535) (logand b 65535))))
    (values
      (loop
        :with (gen-a gen-b) = data
        :for n :below 40000000
        :for a = (next-a gen-a) :then (next-a a)
        :for b = (next-b gen-b) :then (next-b b)
        :count (numbers-match-p a b))
      (loop
        :with (gen-a gen-b) = data
        :for n :below 5000000
        :for a = (next-a-part2 gen-a) :then (next-a-part2 a)
        :for b = (next-b-part2 gen-b) :then (next-b-part2 b)
        :count (numbers-match-p a b)))))

(1am:test test-2017/15
  (multiple-value-bind (part1 part2) (problem-run)
    (1am:is (= 592 part1))
    (1am:is (= 320 part2))))
