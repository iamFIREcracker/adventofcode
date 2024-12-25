(defpackage :aoc/2024/25 #.cl-user::*aoc-use*)
(in-package :aoc/2024/25)

(defun parse-input (&optional (strings (uiop:read-file-lines #P"src/2024/day25.txt")))
  (let (locks keys)
    (let1 blocks (split-sequence:split-sequence "" strings :test 'equal)
      (dolist (block blocks)
        (if (char= (char (nth 0 block) 0) #\#)
            (push block locks)
            (push block keys))))
    (list locks keys)))
#+#:excluded (parse-input)


;; I originally went on and converted each block into an array of numbers
;; (representing the height of each pin / hole); then, to solve the problem,
;; for each pair I would check and confirm that the sum the heights for each
;; column was never greater than 5.
;; Turns out there is a simpler approach: for each position in the block,
;; make sure both lock and keys have a # in it, as that would mean the two
;; would overlap.  Slower, for sure, but easier to relate to.
(defun fit? (block1 block2)
  (looping
    (dolists ((row1 block1)
              (row2 block2))
      (doseqs ((ch1 row1)
               (ch2 row2))
        (never! (char= ch1 ch2 #\#))))))


(define-solution (2024 25) (input parse-input)
  (destructuring-bind (locks keys) input
    (looping
      (dolist (l locks)
        (dolist (k keys)
          (count! (fit? l k)))))))

(define-test (2024 25) (2978))
