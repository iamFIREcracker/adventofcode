(defpackage :aoc/2023/22 #.cl-user::*aoc-use*)
(in-package :aoc/2023/22)


(defvar *name* #\A)
(defun brick (s)
  (prog1 (append (extract-positive-integers s) (list (as-keyword *name*)))
    (setf *name* (code-char (1+ (char-code *name*))))))
#+#:excluded (brick "1,0,1~1,2,1")

(defun parse-input (&optional (strings (uiop:read-file-lines #P"src/2023/day22.txt")))
  (setf *name* #\A)
  (mapcar #'brick strings))
#+#:excluded (parse-input)


(defun index-by-z (&optional (bricks (parse-input)))
  (bnd1 (index (make-hash-table :test 'eql))
    (dolist (b bricks)
      (dorangei (z (third b) (sixth b))
        (push b (gethash z index))))
    index))
#+#:excluded (index-by-z)


(defun can-move-down? (b index)
  (and (not (on-the-ground? b))
       (not (blocked? b index))))


(defun on-the-ground? (b) (= (third b) 1)) ; TODO should we check SIXTH too?


(defun move-down (b)
  (destructuring-bind (x1 y1 z1 x2 y2 z2) (butlast b)
    (list* x1 y1 (1- z1) x2 y2 (1- z2) (last b))))


(defun ranges-overlap? (s1 e1 s2 e2)
  ; (assert (and (<= s1 e1) (<= s2 e2)))
  (and (<= s1 e2) (>= e1 s2)))

(defun bricks-overlap? (b1 b2)
  (and (ranges-overlap? (first b1) (fourth b1) (first b2) (fourth b2))
       (ranges-overlap? (second b1) (fifth b1) (second b2) (fifth b2))
       (ranges-overlap? (third b1) (sixth b1) (third b2) (sixth b2))))

(defun blocked? (b index)
  (bnd1 (b- (move-down b))
    (dorangei (z (third b-) (sixth b-))
      (dolist (b1 (gethash z index))
        (unless (equal b b1)
          (when (bricks-overlap? b- b1)
            (return-from blocked? b1)))))))


(defun free-fall (&optional (bricks (parse-input)))
  (bnd* ((index (index-by-z bricks))
         (continue? t))
    (while continue?
      (setf continue? nil)
      (dolist (z (sort (hash-table-keys index) #'<))
        (dolist (b (gethash z index))
          (while (can-move-down? b index)
            (removef (gethash (sixth b) index) b :test #'equal)
            (decf (sixth b))
            (decf (third b))
            (push b (gethash (third b) index))
            (setf continue? t)))))
    index))
#+#:excluded (free-fall)
#+#:excluded (hash-table-alist *)


(defun disintegrateable? (b index)
  (setf index (copy-hash-table index))
  (dorangei (z (third b) (sixth b))
    (removef (gethash z index) b :test #'equal))
  (dorangei (z (third b) (1+ (sixth b))) ; 1+ because we want to process anything above
    (dolist (b1 (gethash z index))
      (assert (not (equal b1 b)))
      (when (can-move-down? b1 index)
        (return-from disintegrateable? nil))))
  t)

(defun part1 (&optional (bricks (parse-input)))
  (bnd1 (index (free-fall bricks))
    (count-if [disintegrateable? _ index] bricks)))
#+#:excluded (part1)
; 482 too low -- also same answer for someone else
; 1428 too high
; 1429 too high -- lol
; 1412
; 485


(defun disintegrate (b index)
  (setf index (copy-hash-table index))
  (bnd1 (fallen nil)
    (labels ((recur (b)
               (dorangei (z (third b) (sixth b))
                 (removef (gethash z index) b :test #'equal))
               (dorangei (z (third b) (1+ (sixth b))) ; 1+ because we want to process anything above
                 (dolist (b1 (gethash z index))
                   (assert (not (equal b1 b)))
                   (when (can-move-down? b1 index)
                     (push b1 fallen)
                     (recur b1))))))
      (recur b)
      (length (remove-duplicates fallen :test #'equal)))))


(defun part2 (&optional (bricks (parse-input)))
  (bnd1 (index (free-fall bricks))
    (reduce #'+ bricks :key [disintegrate _ index])))
#+#:excluded (part2)
; 324269 too high
; 74594
