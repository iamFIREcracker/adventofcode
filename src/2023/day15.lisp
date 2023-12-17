(defpackage :aoc/2023/15 #.cl-user::*aoc-use*)
(in-package :aoc/2023/15)

(defun part1 (&optional (strings (uiop:read-file-lines #P"src/2023/day15.txt")))
  (bnd1 (s (first strings))
    (bnd1 (parts (split-sequence:split-sequence #\, s))
      (looping
        (dolist (each parts)
          (bnd1 (value 0)
            (doseq (ch each)
              (setf value (rem (* (+ (char-code ch) value) 17) 256)))
            (sum! value)))))))
#+#:excluded (part1)


(defun hash (s)
  (bnd1 (value 0)
    (doseq (ch s)
      (setf value (rem (* (+ (char-code ch) value) 17) 256)))
    value))
#+#:excluded (hash "rn")

(defun part1 (&optional (strings (uiop:read-file-lines #P"src/2023/day15.txt"))
                        &aux (s (first strings)))
  (looping
    (dolist (part (split-sequence:split-sequence #\, s))
      (sum! (hash part)))))
#+#:excluded (part1)
; 510013

(defun focusing-power (boxes)
  (looping
    (dorange (i 0 (length boxes))
      (bnd1 (box (aref boxes i))
        (dolist+ ((j (label . num)) (enumerate (reverse box)))
          (sum! (* (1+ i) (1+ j) num)))))))

(defun part2 (&optional (strings (uiop:read-file-lines #P"src/2023/day15.txt"))
                        &aux (s (first strings)))
  (bnd1 (boxes (make-array 256 :initial-element nil))
    (dolist (part (split-sequence:split-sequence #\, s))
      (cl-ppcre:register-groups-bind (label op num)
          ("(\\w+)(-|=)(\\d*)" part)
        (bnd1 (i (hash label))
          (if (string= op "-")
            (setf (aref boxes i) (delete-if [string= (car _) label] (aref boxes i)))
            (aif (assoc label (aref boxes i) :test #'string=)
              (setf (cdr it) (parse-integer num))
              (push (cons label (parse-integer num)) (aref boxes i)))))))
    (focusing-power boxes)))
#+#:excluded (part2)
; 268497
