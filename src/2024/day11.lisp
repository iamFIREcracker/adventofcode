(defpackage :aoc/2024/11 #.cl-user::*aoc-use*)
(in-package :aoc/2024/11)


#;

If the stone is engraved with the number 0, it is replaced by a stone engraved with the number 1.
If the stone is engraved with a number that has an even number of digits, it is replaced by two stones. The left half of the digits are engraved on the new left stone, and the right half of the digits are engraved on the new right stone. (The new numbers don't keep extra leading zeroes: 1000 would become stones 10 and 0.)
If none of the other rules apply, the stone is replaced by a new stone; the old stone's number multiplied by 2024 is engraved on the new stone.

(defun parse-input (&optional (s (first (uiop:read-file-lines #P"src/2024/day11.txt"))))
  (extract-positive-integers s))
#+#:excluded (parse-input)

(defun transform (stone)
  (cond ((= stone 0) (list 1))
        ((evenp (length (spr stone))) (let1 s (spr stone)
                                        (list (parse-integer (subseq s 0 (/ (length s) 2)))
                                              (parse-integer (subseq s (/ (length s) 2))))))
        (t (list (* stone 2024)))))
#+#:excluded (transform 0)
#+#:excluded (transform 1)
#+#:excluded (transform 10)
#+#:excluded (transform 99)
#+#:excluded (transform 999)

(let1 stones (parse-input)
  (repeat 25
    (setf stones (mapcan #'transform stones)))
  (length stones))
231278!


(let1 input (parse-input)
  (let1 stones (copy-seq input)
    (repeat 25
      (setf stones (mapcan #'transform stones)))
    (prog1-let (res (length stones))
      (retriable
        (find-pattern stones)))))

(defun find-pattern (stones)
  (let1 res (length stones)
    (dorangei (end 100 (/ res 2))
      (let1 pattern (subseq stones 0 end)
        (let1 start2 end
          (let1 pos (search pattern stones :start2 start2)
            (while pos
              (dbg pattern 'matched-at pos (subseq stones pos (+ pos end)) start2 pos)
              ; (break)
              (setf start2 (+ pos end)
                    pos (search pattern stones :start2 start2)))))))))

(defvar *memo* (make-hash-table :test 'equal))

(defun count-stones (stone blinks)
  (memoizing (*memo* stone blinks)
    (cond ((= blinks 0) 1)
          ((= stone 0) (count-stones 1 (1- blinks)))
          ((evenp (length (spr stone))) (let1 s (spr stone)
                                          (+ (count-stones (parse-integer (subseq s 0 (/ (length s) 2))) (1- blinks))
                                             (count-stones (parse-integer (subseq s (/ (length s) 2))) (1- blinks)))))
          (t (count-stones (* stone 2024) (1- blinks))))))
(count-stones 10 1)

(let1 stones (parse-input)
  (reduce '+ stones :key [count-stones _ 75]))
44972143432199 ; too low
274229228071551!
