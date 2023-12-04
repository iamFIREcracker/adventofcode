(defpackage :aoc/2023/03 #.cl-user::*aoc-use*)
(in-package :aoc/2023/03)

(defun parse-line-numbers (s &key (start 0))
  (aif (position-if #'digit-char-p s :start start)
    (bnd1 ((values number end) (parse-integer s :start it :junk-allowed t))
      (cons (list number it (1- end))
            (parse-line-numbers s :start end)))))
#+#:excluded (parse-line-numbers "467..114..")
#+#:excluded (parse-line-numbers "46+..-11..")

(defun parse-all-numbers (&optional (strings (uiop:read-file-lines #P"src/2023/day03.txt")))
  (bnd1 (map (make-hash-table :test 'equal))
    (loop for s in strings for i from 0 do
          (loop for (n start end) in (parse-line-numbers s) do
                (loop for j from start upto end do
                      (setf (gethash (list i j) map) (list n start end)))))
    map))
#+#:excluded (parse-all-numbers)

(defun symbol-char-p (ch)
  (and (not (digit-char-p ch))
       (not (char= ch #\.))))
#+#:excluded (symbol-char-p #\.)
#+#:excluded (symbol-char-p #\1)
#+#:excluded (symbol-char-p #\@)

(defun surrounding-numbers (map i j)
  (remove-duplicates
    (loop for ii from (1- i) upto (1+ i) append
          (loop for jj from (1- j) upto (1+ j)
                for found = (gethash (list ii jj) map)
                when found collect (destructuring-bind (num start end) found
                                     ;; Note: I did not add the current line
                                     ;; number, i.e., `ii`, as part of the
                                     ;; returned value; without that,
                                     ;; REMOVE-DUPLICATES might treat
                                     ;; the the same number on the the same
                                     ;; column but on different line, as
                                     ;; duplicate, and fuck up with the
                                     ;; result sum...
                                     (list num ii start end))))
    :test #'equal))

(defun part-numbers (&optional (strings (uiop:read-file-lines #P"src/2023/day03.txt")))
  (bnd* ((numbers-map (parse-all-numbers strings)))
    (remove-duplicates
      (loop for s in strings for i from 0 append
            (loop for ch across s for j from 0
                  when (symbol-char-p ch)
                  append (surrounding-numbers numbers-map i j)))
      :test #'equal)))

(reduce #'+ (part-numbers) :key #'first)
509115


(defun gear-ratios (&optional (strings (uiop:read-file-lines #P"src/2023/day03.txt")))
  (bnd* ((numbers-map (parse-all-numbers strings)))
    (loop for s in strings for i from 0 append
          (loop for ch across s for j from 0
                when (and (char= ch #\*)
                          (= (length (surrounding-numbers numbers-map i j)) 2))
                collect (reduce #'* (surrounding-numbers numbers-map i j)
                                :key #'first) ))))
(reduce #'+ (gear-ratios))
75220503
