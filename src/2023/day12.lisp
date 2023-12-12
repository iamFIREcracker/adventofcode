(defpackage :aoc/2023/12 #.cl-user::*aoc-use*)
(in-package :aoc/2023/12)

'(&optional (strings (uiop:read-file-lines #P"src/2023/day12.txt")))
'(declare (optimize (debug 3)))

(defun parse-condition-record (s)
  (destructuring-bind (springs groups) (split-sequence:split-sequence #\Space s)
    (list springs (extract-positive-integers groups))))
#+#:excluded (parse-condition-record (first (uiop:read-file-lines #P"src/2023/day12.txt")))

(defun valid? (springs expected-groups)
  (bnd1 (actual-groups (mapcar #'length (cl-ppcre:all-matches-as-strings "#+" springs)))
    (equal expected-groups actual-groups)))
(cl-ppcre:all-matches-as-strings "#+" "?###???????? 3,2,1")
#+#:excluded (valid? "#.#.###" '(1 1 3))
#+#:excluded (valid? "#.#.###" '(1 2 3))
#+#:excluded (valid? "#.#" '(1 2 3))

(defun count-valid-arrangements (s)
  (destructuring-bind (springs groups) (parse-condition-record s)
    (looping
      (labels ((recur ()
                 (pr springs groups)
                 (unless (count! (valid? springs groups))
                   (awhen (position #\? springs)
                     (setf (aref springs it) #\.)
                     (recur)
                     (setf (aref springs it) #\# )
                     (recur)
                     (setf (aref springs it) #\?)))))
        (recur)))))


; (count-valid-arrangements "#.#.### 1,1,3")
; (count-valid-arrangements "?.?.?## 1,1,3")
; (count-valid-arrangements "???.### 1,1,3")
; (count-valid-arrangements ".??..??...?##. 1,1,3")
; (count-valid-arrangements "?#?#?#?#?#?#?#? 1,3,1,6")
; (count-valid-arrangements "????.#...#... 4,1,1")
; (count-valid-arrangements "????.######..#####. 1,6,5")
; (count-valid-arrangements "?###???????? 3,2,1")

; 2398 too low
; 74921 too high
; (time (reduce #'+ (uiop:read-file-lines #P"src/2023/day12.txt") :key #'count-valid-arrangements))
; Evaluation took:
;   16.560 seconds of real time
;   16.618706 seconds of total run time (16.567646 user, 0.051060 system)
;   [ Real times consist of 0.260 seconds GC time, and 16.300 seconds non-GC time. ]
;   [ Run times consist of 0.235 seconds GC time, and 16.384 seconds non-GC time. ]
;   100.36% CPU
;   31,407,189,773 processor cycles
;   4,065,966,464 bytes consed
;
; 7460

(defmacro memoizing ((ht &rest key-parts) &body body)
  (with-gensyms (memo key)
    `(let ((,memo ,ht)
           (,key (list ,@key-parts)))
       (multiple-value-bind (res res?) (gethash ,key ,memo)
         (if res?
           res
           (setf (gethash ,key ,memo)
                 (block memo
                        ,@body)))))))

(defun count-valid-arrangements (s)
  (destructuring-bind (springs groups) (parse-condition-record s)
    (bnd1 (memo (make-hash-table :test 'equal))
      (labels ((recur (springs current remaining)
                 (memoizing (memo springs current remaining)
                   (bnd1 (ch (car springs))
                     (cond ((and (null springs) (plusp current) (not remaining)) 0)
                           ((and (null springs) (plusp current))
                            (if (= (car remaining) current)
                              (recur springs 0 (cdr remaining))
                              0))
                           ((and (null springs) (zerop current) (not remaining) 1))
                           ((and (null springs) (zerop current) remaining 0))
                           ((and (plusp current) (not remaining)) 0)
                           ((and (char= ch #\.) (plusp current))
                            (if (= (car remaining) current)
                              (recur (cdr springs) 0 (cdr remaining))
                              0))
                           ((and (char= ch #\.) (zerop current))
                            (recur (cdr springs) 0 remaining))
                           ((char= ch #\# ) (recur (cdr springs) (1+ current) remaining))
                           ((and (char= ch #\?))
                            (+ (recur (cons #\. (cdr springs)) current remaining)
                               (recur (cons #\# (cdr springs)) current remaining)))
                           (t (error "WTF?!")))))))
        (recur (coerce springs 'list) 0 groups)))))

(defun massage-input (s)
  (destructuring-bind (springs groups) (split-sequence:split-sequence #\Space s)
    (with-output-to-string (*standard-output*)
      (dotimes (n 5)
        (unless (zerop n)
          (princ #\?))
        (princ springs))
      (princ #\Space)
      (dotimes (n 5)
        (unless (zerop n)
          (princ #\,))
        (princ groups)))))
#+#:excluded (massage-input ".# 1")

; (time (reduce #'+ (uiop:read-file-lines #P"src/2023/day12.txt") :key [count-valid-arrangements (massage-input _)]))
; Evaluation took:
;   27.109 seconds of real time
;   27.115514 seconds of total run time (26.935509 user, 0.180005 system)
;   [ Real times consist of 0.110 seconds GC time, and 26.999 seconds non-GC time. ]
;   [ Run times consist of 0.100 seconds GC time, and 27.016 seconds non-GC time. ]
;   100.03% CPU
;   51,393,183,099 processor cycles
;   898,910,656 bytes consed
;
; 6720660274964
