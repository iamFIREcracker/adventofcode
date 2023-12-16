(defpackage :aoc/2023/13 #.cl-user::*aoc-use*)
(in-package :aoc/2023/13)


(defun parse-input (&optional (strings (uiop:read-file-lines #P"src/2023/day13.txt")))
  (split-sequence:split-sequence "" strings :test 'string=))


(defun find-reflection (pattern &optional with-smudge?)
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
      ;; Vertical reflections
      (dorange (j 0 (1- cols))
        (bnd1 (diff 0)
          (dorange (i 0 rows)
            (dolists ((ch1 (reverse (row i 0 j)))
                      (ch2 (row i (1+ j) (1- cols))))
              (when (char/= ch1 ch2)
                (incf diff))))
          ;; When there is a smudge only one character along the reflection
          ;; line will be off; also, there can only be one smudge per pattern,
          ;; so once we found a reflection with only one character off, there
          ;; we have found our reflection line once the smudge was fixed.
          (when (= diff (if with-smudge? 1 0))
            (return-from find-reflection (list :v j)))))
      ;; Horizontal reflections
      (dorange (i 0 (1- rows))
        (bnd1 (diff 0)
          (dorange (j 0 cols)
            (dolists ((ch1 (reverse (col j 0 i)))
                      (ch2 (col j (1+ i) (1- rows))))
              (when (char/= ch1 ch2)
                (incf diff))))
          (when (= diff (if with-smudge? 1 0))
            (return-from find-reflection (list :h i))))))))


(defun summary (reflection-line)
  (destructuring-bind (dir val) reflection-line
    (ecase dir
      (:v (1+ val))
      (:h (* (1+ val) 100)))))

(define-solution (2023 13) (input parse-input)
  (values (reduce #'+ (mapcar #'find-reflection input) :key #'summary)
          (reduce #'+ (mapcar [find-reflection _ t] input) :key #'summary)))

(define-test (2023 13) (29846 25401))
