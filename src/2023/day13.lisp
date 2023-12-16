(defpackage :aoc/2023/13 #.cl-user::*aoc-use*)
(in-package :aoc/2023/13)


(defun parse-input (&optional (strings (uiop:read-file-lines #P"src/2023/day13.txt")))
  (split-sequence:split-sequence "" strings :test 'string=))

(defun find-reflections (pattern)
  (bnd* ((rows (length pattern))
         (cols (length (first pattern))))
    (flet ((row (i j1 j2)
             (looping
               (dorangei (j j1 j2)
                 (collect! (aref (nth i pattern) j)))))
           (col (j i1 i2)
             (looping
               (dorangei (i i1 i2)
                 (collect! (aref (nth i pattern) j))))))
      (list
        ;; Vertical reflections
        (position-if (lambda (j)
                       (every (lambda (i)
                                (every #'char=
                                       (reverse (row i 0 j))
                                       (row i (1+ j) (1- cols))))
                              (iota rows)) )
                     (iota (1- cols)))
        ;; Horizontal reflections
        (position-if (lambda (i)
                       (every (lambda (j)
                                (every #'char=
                                       (reverse (col j 0 i))
                                       (col j (1+ i) (1- rows))))
                              (iota cols)) )
                     (iota (1- rows)))))))

(defun summary (pattern)
  (destructuring-bind (col row) (find-reflections pattern)
    (+ (if col (1+ col) 0)
       (* (if row (1+ row) 0) 100))))

#+#:excluded (mapcar #'find-reflections (parse-input))
#+#:excluded (reduce #'+ (parse-input) :key #'summary)

(defun find-reflection-lines (pattern)
  (append (find-vertical-reflection-lines pattern)
          (find-horizontal-reflection-lines pattern)))

(defun find-vertical-reflection-lines (pattern)
  (bnd* ((rows (length pattern))
         (cols (length (first pattern))))
    (flet ((row (i j1 j2)
             (looping
               (dorangei (j j1 j2)
                 (collect! (aref (nth i pattern) j)))))
           (col (j i1 i2)
             (looping
               (dorangei (i i1 i2)
                 (collect! (aref (nth i pattern) j))))))
      (looping
        (dorange (j 0 (1- cols))
          (when (every (lambda (i)
                         (every #'char=
                                (reverse (row i 0 j))
                                (row i (1+ j) (1- cols))))
                       (iota rows))
            (collect! (list :v j))))))))

(defun find-horizontal-reflection-lines (pattern)
  (bnd* ((rows (length pattern))
         (cols (length (first pattern))))
    (flet ((row (i j1 j2)
             (looping
               (dorangei (j j1 j2)
                 (collect! (aref (nth i pattern) j)))))
           (col (j i1 i2)
             (looping
               (dorangei (i i1 i2)
                 (collect! (aref (nth i pattern) j))))))
      (looping
        (dorange (i 0 (1- rows))
          (when (every (lambda (j)
                         (every #'char=
                                (reverse (col j 0 i))
                                (col j (1+ i) (1- rows))))
                       (iota cols))
            (collect! (list :h i))))))))

(defun summary (pattern)
  (looping
    (dolist+ ((dir val) (find-reflection-lines pattern))
      (incf val)
      (sum! (ecase dir (:v val) (:h (* val 100)))))))

(reduce #'+ (parse-input) :key #'summary)

(defun fix-smudge (pattern &aux (pattern (copy-seq pattern)))
  (bnd* ((rows (length pattern))
         (cols (length (first pattern)))
         ((old-line) (find-reflection-lines pattern)))
    (dorange (i 0 rows)
      (dorange (j 0 cols)
        (bnd1 (old (aref (nth i pattern) j))
          (when (char/= old #\.)
            (setf (aref (nth i pattern) j) #\.)
            (dolist+ (new-line (find-reflection-lines pattern))
              (when (not (equal old-line new-line))
                (return-from fix-smudge new-line))))
          (when (char/= old #\# )
            (setf (aref (nth i pattern) j) #\# )
            (dolist+ (new-line (find-reflection-lines pattern))
              (when (not (equal old-line new-line))
                (return-from fix-smudge new-line))))
          (setf (aref (nth i pattern) j) old) )))))


(defun summary (reflection-line)
  (destructuring-bind (dir val) reflection-line
    (incf val)
    (ecase dir (:v val) (:h (* val 100)))))
(mapcar #'fix-smudge (parse-input))
(reduce #'+ * :key #'summary)

; can there be different reflection lines?
; can there 

