(defpackage :aoc/2022/05 #.cl-user::*aoc-use*)
(in-package :aoc/2022/05)


(defun parse-input (&optional (strings (uiop:read-file-lines #P"src/2022/day05.txt")))
  (labels ((transpose (m) (loop for i below (length (car m)) collect (col i m)))
           (col (j m) (loop for row in m collect (nth j row))))
    (destructuring-bind (stacks moves)
        (split-sequence:split-sequence "" strings :test #'string=)
      (list
        (let* ((m (length (car stacks)))
               (array (make-array (1+ (ceiling (/ m 4))) :initial-element nil)))
          (loop for stack in (cdr (transpose (mapcar [coerce _ 'list] stacks))) by #'cddddr
                for i from 1 do
                (setf (aref array i) (loop for ch in stack when (alpha-char-p ch) collect ch)))
          array)
        (mapcar [extract-positive-integers _] moves)))))

(defun top-crates (stacks)
  (format nil "~{~A~}" (map 'list #'car (subseq stacks 1))))

(defun part1 ()
  (destructuring-bind (stacks moves) (parse-input)
   (loop for (n from to) in moves do
         (loop repeat n do (push (pop (aref stacks from)) (aref stacks to)))
         finally (return (top-crates stacks)))))

(defun part2 ()
  (destructuring-bind (stacks moves) (parse-input)
    (loop for (n from to) in moves do
          (loop for a in (reverse (loop repeat n collect (pop (aref stacks from))))
                do (push a (aref stacks to)))
          finally (return (top-crates stacks)))))

(defun solution-run ()
  (values (part1) (part2)))

(define-test (2022 05) ("SVFDLGLWV" "DCVTCVPCL"))

;;; Version 1 (this):
;;; - Add parsing of the input stacks (it's still a mess, but at least it's not hard-coded anymore)
;;; - Nicely format the solutions
;;; - [link](https://topaz.github.io/paste/#XQAAAQCRBQAAAAAAAAAUGQimgx+p6PZhRh7uIO5BVNPIdKEN9R6L6scYP/cAtE1OY4+7AE4L6pGtFZ0RIzO5WPZjKg+dcdnDKddy2wTvGcEDnWUCw+TILaxPE8C/EiWaJ8D9I1bC0kscXyao6sgG5W0yDHTIQl97/tdfx0s2I/Xv0aBROjThGCyDoEs90OvQDI5fMmq5Ya8ZfCCpGF1J4B2Dx1RDjbSeXbx0EeAuh00GZC69J5SiUZz31Be/eJUfmkGXtVmtcO3LhEVksElx1rmv6NbS2g3iHp9EcihkLyNElWYQJ/lFKrFVCPXT5xQRfQze706ngs7Sh7uzHqmpB7YVxXUZI3YBaFPzPH8c/H20zb2HNpDwMVN8E3zFjAC8vAVhMzrcLSIWJRzWoqTx1qR4LrbyMwY/zPDY4aCWm7L1HktWW2fbKhVPPWG82TBMmvqMJTHpiSUKSZsYPxgzSHiUdIkCCCIh8ukw4hzF5Naehp3dXhToWEh5m6ycY/7QE3TsY18hftbmHGnLIxi3Vbmsm13l3GUspDTBl2swpsN5IT9cvlV9+EBRZuaNpcmv12S7QlwnV+sDrfJfj7lzUI8F6YHCmOfT3d5JmfMCExklYY+qh0pWYzDN7qMv3EeVbmw0Lsuoa6UKNVwXzZQbb6dW3Nn4CHltUmTldHm4UZs2LoTlew5WmM7ds+UcAoaSCaBt+bSdNJ88Y2gSEnDENM4blkKjf/rJ8qzbez+KOenhcjurf+/vFT8BmoOubw/epNxzEfFZLJ+DYKHhtjNV/drt99kDyXpF+py8rNhLfxS+4cajV0mfutSKTpBslUn/0pFdKA==)
;;; Version 0:
;;; - Hard-coded input (for the stacks; the moves were getting parsed just fine)
;;; - Part 2 solution is kind of inefficient, but whatever (pop all the crates to move first, reverse the list, then push them back in, one at a time)
;;; - Answers not nicely formatted (i.e. `(NIL S V F D L G L W V)` instead of just `SVFDLGLWV`)
;;; - [link](https://topaz.github.io/paste/#XQAAAQAfBwAAAAAAAAAUGQimgulVkMqJePdJ12sdB5fH9BnGD8S3Wer97rPrXwApTz7e2XAzCjOIR8KKTxc9GYxewNwO4Mm0PEq9ROyOuqExEKfNBfAgJEWIE5t+g//RgdEwPWdUBI6B6RnvKBYWT9XAfWZ9qLmowFNmx9AgamGUXRNkyrQ8DPHlKrw4DG8JBhVrrOb4/GzkhC/AWj1Lf95WRBx/ZxeqT7a2T6f/+4EGALSqKMbUf3AQNKYs3XfTgCqcw+pbmRkT77XQMnNkKa0CkCPin2Q7gkuOjs4PrFGqXsZHxHYgTOVV+iaA/tHCf2O5eVKhIOsJgMpr0SzeUn3Q9yM+vJMuOv/AcYCgA9O4EayASzkPJrnq6w67JaxptkKfBpM29OEKrEjU3KllW47w835Wop5MLXwrYH+PRRhZPds88YqQ9NlrUKjVFFNA+0gpgRS4e4UAH+umfYjUpfJM76aTdpwyHfLmqGFTWbj3DkSpfdqnajaJfuoHo2oMfCFqdaaJYLoYqaIohBl8UGxEogxTOvcwbzt06gqAfDLjw2JVnCavRuNQCU/1GDYKgBK6nDopoR4vJFDDtHV5dJk8fgR36a/fd2f6NvKdFhVzmL/NNjzpiLbOahPktTGL9bgxPCKE1tp/AD39lUnxankn1cHtTnggof01LcrL+QsBwWqf1Kj6+L1GGromoTYsWygbPP60wyI=)
