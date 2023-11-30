(defpackage :aoc/2022/25 #.cl-user::*aoc-use*)
(in-package :aoc/2022/25)


(defun snafu->decimal (s)
  (loop for m = 1 then (* m 5)
        for c across s for n = (ecase c
                                 (#\0 0)
                                 (#\1 1)
                                 (#\2 2)
                                 (#\= -2)
                                 (#\- -1))
        for sum = n then (+ (* sum 5) n)
        finally (return sum)))
#+#:excluded (snafu->decimal "1121-1110-1=0")

(defun snafus (&optional (file #P"src/2022/day25.txt"))
  (mapcar #'snafu->decimal (uiop:read-file-lines file)))
#+#:excluded (snafus)

(defun number->snafu (n)
  (coerce
    (reverse
      (loop with carry = 0 with digits = (digits n 5) with ch
            for d = (pop digits) for m = (+ (or d 0) carry)
            do (pr d carry '-> m)
            do (if (= m 5)
                 (setf carry 1 ch #\0)
                 (if (> m 2)
                   (setf carry 1 ch (if (= m 3) #\= #\-))
                   (setf carry 0 ch (code-char (+ (char-code #\0) m)))))
            collect ch
            until (and (zerop carry) (not digits))))
    'string))

#; Scratch
(number->snafu 314159265)
(reduce #'+ (snafus))
(number->snafu *)
(print-snafu *)

(snafu->decimal "1=-0-2")
(snafu->decimal  "12111")
(snafu->decimal  "    =")
(snafu+ (parse-snafu "22")
        (parse-snafu "22"))
(print-snafu *)

(+ (snafu->decimal "22")
   (snafu->decimal "22"))

(truncate 2022 125)
