(defpackage :aoc/2024/25 #.cl-user::*aoc-use*)
(in-package :aoc/2024/25)

#;


(defun parse-input (&optional (strings (uiop:read-file-lines #P"src/2024/day25.txt")))
  (let (locks keys)
    (let1 blocks (split-sequence:split-sequence "" strings :test 'equal)
      (dolist (strings blocks)
        (let1 whatever (list -1 -1 -1 -1 -1)
          (doeseq (i s strings)
            (doeseq (j ch s)
              (if (char= ch #\#)
                  (incf (nth j whatever)))))
          (if (char= (char (nth 0 strings) 0) #\#)
              (push whatever locks)
              (push whatever keys)) )))
    (list locks keys)))
#+#:excluded (parse-input)

(defun good (x1 x2)
  (looping
    (dolists ((xx1 x1)
              (xx2 x2))
      (always! (<= (+ xx1 xx2) 5)))))

(destructuring-bind (locks keys) (parse-input)
  (dbgl locks keys)
  (looping
    (dolist (l locks)
      (dolist (k keys)
        (count! (good l k))))))
