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
#+#:excluded (coords-for *num-keypad* #\A)
#+#:excluded (coords-for *num-keypad* #\0)
#+#:excluded (coords-for *num-keypad* #\1)

(defun button-at (keypad row col)
  (when (array-in-bounds-p keypad row col)
    (let1 b (aref keypad row col)
      (when (char/= b #\Space)
        b))))
#+#:excluded (button-at *num-keypad* 3 2)
#+#:excluded (button-at *num-keypad* 3 1)
#+#:excluded (button-at *num-keypad* 3 0)
#+#:excluded (button-at *num-keypad* 4 0)

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
#+#:excluded (move-to *num-keypad* (coords-for *num-keypad* #\A) (coords-for *num-keypad* #\A))
#+#:excluded (move-to *num-keypad* (coords-for *num-keypad* #\A) (coords-for *num-keypad* #\0))
#+#:excluded (move-to *num-keypad* (coords-for *num-keypad* #\A) (coords-for *num-keypad* #\2))

(defun robot (keypad) (list keypad (coords-for keypad #\A)))
(defaccessor keypad (robot) (accesses (car robot)))
(defaccessor pos (robot) (accesses (cadr robot)))

(defun type-code (robot code)
  (let1 keypad (keypad robot)
    (recursively ((code (coerce code 'list)))
      (cond ((zerop (length code)) (list ""))
            (t (let1 to-ch (move-to keypad (pos robot) (coords-for keypad (car code)))
                 (assert to-ch () "A path to ~A should exist" (car code))
                 (setf (pos robot) (coords-for keypad (car code)))
                 (let1 from-ch-to-end (recur (cdr code))
                   (looping
                     (doseq (seq1 to-ch)
                       (doseq (seq2 from-ch-to-end)
                         (collect! (spr seq1 #\A seq2))))))))))))

; robots always seem to start and end at A, so we might as well recreate them every time
(defun shortest-path-to-code (code3)
  (~> (looping
        (dolist (code2 (type-code (robot *num-keypad*) code3))
          (dolist (code1 (type-code (robot *dir-keypad*) code2))
            (dolist (code (type-code (robot *dir-keypad*) code1))
              (collect! code)))))
      (sort ~ #'< :key #'length)
      first
      length))
#+#:excluded (shortest-path-to-code "029A")
#+#:excluded (shortest-path-to-code "0")

(defun complexity (code3)
  (* (dbg (shortest-path-to-code code3))
     (dbg (first (extract-integers code3)))))
#+#:excluded (complexity "029A")
#+#:excluded (complexity "980A")
#+#:excluded (complexity "179A")
#+#:excluded (complexity "456A")
#+#:excluded (complexity "379A")

#;
#+#:excluded (reduce #'+ (parse-input) :key #'complexity)
176870

(defun type-code (code &rest robots)
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
#+#:excluded (type-code "029A" (robot *num-keypad*) (robot *dir-keypad*) (robot *dir-keypad*))

(defun complexity (code robots)
  (* (dbg (apply 'type-code code robots))
     (dbg (first (extract-integers code)))))

(let1 robots (list* (robot *num-keypad*)
                    (looping (repeat 2 (collect! (robot *dir-keypad*)))))
  (reduce #'+ (parse-input) :key [complexity _ robots]))

(let1 robots (list* (robot *num-keypad*)
                    (looping (repeat 25 (collect! (robot *dir-keypad*)))))
  (reduce #'+ (parse-input) :key [complexity _ robots]))
