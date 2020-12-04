(defpackage :aoc/2016/05 #.cl-user::*aoc-use*)
(in-package :aoc/2016/05)

(defun goodp (md5-hash)
  (and (zerop (aref md5-hash 0))
       (zerop (aref md5-hash 1))
       (zerop (ash (aref md5-hash 2) -4))))

(defun int-hex (n)
  (write-to-string n :base 16))

(defun destruct-hash (md5-hash)
  (let ((valid (goodp md5-hash))
        (part1-digit (aref (string-downcase (int-hex (aref md5-hash 2))) 0))
        (part2-offset (aref md5-hash 2))
        (part2-digit (aref (string-downcase (int-hex (ash (aref md5-hash 3) -4))) 0)))
    (list valid part1-digit part2-offset part2-digit)))

(define-solution (2016 5) (door-id first)
  (loop :with part1 = (make-array 8 :element-type 'character :adjustable t :fill-pointer 0)
        :with part2 = (make-string 8 :initial-element #\_)
        :with remaining = 8
        :for index :from 0
        :for md5-hash = (md5:md5sum-string (mkstr door-id index))
        :for (valid part1-digit part2-offset part2-digit) = (destruct-hash md5-hash)
        :when valid :do (progn
                          (when (< (fill-pointer part1) 8)
                            (vector-push part1-digit part1))
                          (when (and (< part2-offset 8)
                                     (eql (aref part2 part2-offset) #\_))
                            (setf (aref part2 part2-offset) part2-digit
                                  remaining (1- remaining))))
        :when (zerop remaining) :return (values part1 part2)))

(define-test (2016 5) ("2414bc77" "437e60fc"))
