(defpackage :aoc/2024/21 #.cl-user::*aoc-use*)
(in-package :aoc/2024/21)

(defun parse-input (&optional (strings (uiop:read-file-lines #P"src/2024/day21.txt")))
  strings)
#+#:excluded (parse-input)


(defparameter *num-keypad* (make-array (list 4 3) :initial-contents '("789"
                                                                      "456"
                                                                      "123"
                                                                      " 0A")))
(defparameter *dir-keypad* (make-array (list 2 3) :initial-contents '(" ^A"
                                                                      "<v>")))

(defun coords-for (keypad char)
  (destructuring-bind (rows cols) (array-dimensions keypad)
    (dotimes (i rows)
      (dotimes (j cols)
        (if (char= (aref keypad i j) char)
            (return-from coords-for (list i j)))))))

(defun button-at (keypad row col)
  (when (array-in-bounds-p keypad row col)
    (let1 b (aref keypad row col)
      (when (char/= b #\Space)
        b))))

(defun move-to (keypad start end)
  (let ((hq (make-hq :key #'car))
        (seen (make-hash-table :test 'equal))
        best)
    (hq-insert hq (list 0 start nil))
    (looping
      (until (hq-empty-p hq)
        (destructuring-bind (cost pos path) (hq-pop hq)
          (when (equal pos end)
            (if (null best)
                (setf best cost))
            (when (= cost best)
              (collect! (coerce (reverse path) 'string))))
          (when (<= cost (gethash pos seen most-positive-fixnum))
            (setf (gethash pos seen) cost)
            (destructuring-bind (i j) pos
              (doseq ((di dj ch) '((-1 0 #\^) (0 1 #\>) (1 0 #\v) (0 -1 #\<)))
                (let ((ni (+ i di)) (nj (+ j dj)))
                  (when (button-at keypad ni nj)
                    (assert (char/= (aref keypad ni nj) #\Space))
                    (hq-insert hq (list (1+ cost) (list ni nj) (cons ch path)))))))))))))


(defun robot (keypad) (list keypad (coords-for keypad #\A)))
(defaccessor keypad (robot) (accesses (car robot)))
(defaccessor pos (robot) (accesses (cadr robot)))

(defun min-steps-for-code (code robots)
  (let1 memo (make-hash-table :test 'equal)
    (recursively ((code code)
                  (robots robots))
      (memoizing (memo code robots)
        (cond ((null robots) (length code))
              (t (destructuring-bind (robot . robots) robots
                   (looping
                     (doseq (ch code)
                       (sum!
                         (let1 codes1 (move-to (keypad robot) (pos robot) (coords-for (keypad robot) ch))
                           (setf (pos robot) (coords-for (keypad robot) ch))
                           (looping
                             (dolist (code1 codes1)
                               (minimize! (recur (spr code1 #\A) robots)))))))))))))))

(defun complexity (code robots)
  (* (min-steps-for-code code robots)
     (~> code extract-integers first)))


(define-solution (2024 21) (strings)
  (flet ((mkrobots (n)
           (list* (robot *num-keypad*)
                  (looping (repeat n (collect! (robot *dir-keypad*)))))))
    (values (let1 rbts (mkrobots 2)
              (reduce #'+ strings :key [complexity _ rbts]))
            (let1 rbts (mkrobots 25)
              (reduce #'+ strings :key [complexity _ rbts])))))

(define-test (2024 21) (176870 223902935165512))
