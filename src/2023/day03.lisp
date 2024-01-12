(defpackage :aoc/2023/03 #.cl-user::*aoc-use*)
(in-package :aoc/2023/03)


(defun parse-line-numbers (s &key (start 0))
  (aif (position-if #'digit-char-p s :start start)
    (bnd1 ((values number end) (parse-integer s :start it :junk-allowed t))
      (cons (list number it (1- end))
            (parse-line-numbers s :start end)))))

(defun parse-all-numbers (&optional (strings (uiop:read-file-lines #P"src/2023/day03.txt")))
  (bnd1 (map (make-hash-table :test 'equal))
    (doseq ((i s) (enumerate strings))
      (doseq ((n start end) (parse-line-numbers s))
        (dorangei (j start end)
          (setf (gethash (list i j) map) (list n i start end)))))
    map))


(defun symbol-char-p (ch)
  (and (not (digit-char-p ch))
       (not (char= ch #\.))))

(defun surrounding-numbers (map i j)
  (remove-duplicates
    (looping
      (dorangei (ii (1- i) (1+ i))
        (dorangei (jj (1- j) (1+ j))
          (awhen (gethash (list ii jj) map)
            (collect! it)))))
    :test #'equal))

(defun part-numbers (&optional (strings (uiop:read-file-lines #P"src/2023/day03.txt")))
  (bnd1 (numbers-map (parse-all-numbers strings))
    (remove-duplicates
      (looping
        (doseq ((i s) (enumerate strings))
          (doseq ((j ch) (enumerate s))
            (when (symbol-char-p ch)
              (append! (surrounding-numbers numbers-map i j))))))
      :test #'equal)))


(defun gear-char-p (ch) (char= ch #\*))

(defun gears (&optional (strings (uiop:read-file-lines #P"src/2023/day03.txt")))
  (bnd* ((numbers-map (parse-all-numbers strings)))
    (looping
      (doseq ((i s) (enumerate strings))
        (doseq ((j ch) (enumerate s))
          (when (gear-char-p ch)
            (bnd1 (numbers (surrounding-numbers numbers-map i j))
              (when (= (length numbers) 2)
                (collect! numbers)))))))))

(defun gear-ratio (numbers)
  (destructuring-bind (n1 n2) numbers
    (* (first n1) (first n2))))


(define-solution (2023 03) (strings)
  (values (reduce #'+ (part-numbers strings) :key #'first)
          (reduce #'+ (gears strings) :key #'gear-ratio) ))

(define-test (2023 03) (509115 75220503))
