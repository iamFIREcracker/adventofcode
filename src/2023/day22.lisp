(defpackage :aoc/2023/22 #.cl-user::*aoc-use*)
(in-package :aoc/2023/22)


(defun brick (s) (extract-positive-integers s))
(defaccessor x1 (b) (accesses (nth 0 b)))
(defaccessor y1 (b) (accesses (nth 1 b)))
(defaccessor z1 (b) (accesses (nth 2 b)))
(defaccessor x2 (b) (accesses (nth 3 b)))
(defaccessor y2 (b) (accesses (nth 4 b)))
(defaccessor z2 (b) (accesses (nth 5 b)))

(defparameter *debug-names* nil)

(defun parse-input (&optional (strings (aoc::read-problem-input 2023 22)))
  (bnd1 (bricks (mapcar #'brick strings))
    (if-not *debug-names*
      bricks
      (bnd1 (name #\A)
        (looping
          (doseq (b bricks)
            (collect! (append b (list (as-keyword name))) )
            (setf name (code-char (1+ (char-code name))))))))))


(defun index-by-z (&optional (bricks (parse-input)))
  (bnd1 (index (make-hash-table :test 'eql))
    (dolist (b bricks)
      (add-to-index index b))
    index))

(defun add-to-index (index b)
  (dorangei (z (z1 b) (z2 b))
    (push b (gethash z index)))
  index)

(defun remove-from-index (index b)
  (dorangei (z (z1 b) (z2 b))
    (removef (gethash z index) b :test #'equal))
  index)


(defun can-move-down? (b index)
  (and (not (on-the-ground? b))
       (not (blocked? b index))))


(defun on-the-ground? (b) (= (z1 b) 1)) ; TODO should we check Z2 too?


(defun blocked? (b index)
  (bnd1 (b- (move-down b))
    (dorangei (z (z1 b-) (z2 b-))
      (dolist (b1 (gethash z index))
        (unless (equal b b1)
          (when (bricks-overlap? b- b1)
            (return-from blocked? b1)))))))


(defun move-down (b) (move-down! (copy-seq b)))

(defun move-down! (b)
  (prog1 b
    (decf (z1 b))
    (decf (z2 b))))

(defun bricks-overlap? (b1 b2)
  (and (ranges-overlap? (x1 b1) (x2 b1) (x1 b2) (x2 b2))
       (ranges-overlap? (y1 b1) (y2 b1) (y1 b2) (y2 b2))
       (ranges-overlap? (z1 b1) (z2 b1) (z1 b2) (z2 b2))))

(defun ranges-overlap? (s1 e1 s2 e2)
  (and (<= s1 e2) (>= e1 s2)))


(defun free-fall (&optional (bricks (parse-input)))
  (bnd* ((index (index-by-z bricks))
         (continue? t))
    (while continue?
      (setf continue? nil)
      (dolist (z (sort (hash-table-keys index) #'<))
        (dolist (b (gethash z index))
          (while (can-move-down? b index)
            (remove-from-index index b)
            (add-to-index index (move-down! b))
            (setf continue? t)))))
    index))


(defun safe-to-disintegrate? (b index)
  (setf index (copy-hash-table index))
  (remove-from-index index b)
  (not (bricks-free-to-move index (z1 b) (1+ (z2 b)))))  ; 1+ because we want to process anything above the removed brick

(defun bricks-free-to-move (index z1 z2)
  (looping
    (dorangei (z z1 z2)
      (dolist (b1 (gethash z index))
        (when (can-move-down? b1 index)
          (adjoin! b1 :test #'equal))))))


(defun disintegrate-and-count-fallen (index b)
  (setf index (copy-hash-table index))
  (length
    (looping
      (recursively ((b b))
        (remove-from-index index b)
        (dolist (b1 (bricks-free-to-move index (z1 b) (1+ (z2 b)))) ; 1+ because we want to process anything above the removed brick
          (adjoin! b1)
          (recur b1))))))


(define-solution (2023 22) (bricks parse-input)
  (bnd1 (index (free-fall bricks))
    (values (count-if [safe-to-disintegrate? _ index] bricks)
            (reduce #'+ bricks :key [disintegrate-and-count-fallen index _]))))

(define-test (2023 22) (485 74594))
