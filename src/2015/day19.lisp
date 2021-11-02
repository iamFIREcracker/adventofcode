(defpackage :aoc/2015/19 #.cl-user::*aoc-use*)
(in-package :aoc/2015/19)

(defun parse-replacement (string &aux (parts (cl-ppcre:split " " string)))
  (cons (first parts) (third parts)))

(defun parse-replacements (lines) (mapcar #'parse-replacement lines))

(defun parse-input (lines &aux (pos (position "" lines :test #'string=)))
  (cons (parse-replacements (subseq lines 0 pos))
        (nth (1+ pos) lines)))

(defun mreplace (molecule from to pos)
  (format nil "~A~A~A"
          (subseq molecule 0 pos)
          to
          (subseq molecule (+ pos (length from)))))

(defun part1 (input)
  (destructuring-bind (replacements . molecule) input
      (length
        (remove-duplicates
          (loop for (from . to) in replacements append
                (loop for start in (cl-ppcre:all-matches from molecule) by #'cddr
                      collect (mreplace molecule from to start)))
          :test #'string=))))

(defun number-of-matches (molecule replacement &aux (to (cdr replacement)))
  (length (cl-ppcre:all-matches to molecule)))

(defun part2 (input)
  (destructuring-bind (replacements . molecule) input
    (labels ((recur (molecule steps)
               (cond ((string= molecule "e") (return-from part2 steps))
                     (t
                       (setf replacements
                             (sort (copy-seq replacements) #'<
                                   :key (partial-1 #'number-of-matches molecule)))
                       (loop for (from . to) in replacements
                             for matches = (cl-ppcre:all-matches to molecule)
                             do (loop for start in matches by #'cddr
                                      do (recur (mreplace molecule to from start)
                                                (1+ steps))))))))
      (recur molecule 0))))

(define-solution (2015 19) (input parse-input)
  (values (part1 input) (part2 input)))

(define-test (2015 19) (576 207))
