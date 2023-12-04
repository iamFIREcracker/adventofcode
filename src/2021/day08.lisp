(defpackage :aoc/2021/08 #.cl-user::*aoc-use*)
(in-package :aoc/2021/08)


(defun parse-entries (data)
  (loop for string in data
        for parts = (cl-ppcre:all-matches-as-strings "[a-z]+" string)
        collect (cons (subseq parts 0 10) (subseq parts 10))))
(defun inputs (entry) (car entry))
(defun outputs (entry) (cdr entry))


(defun part1 (entries)
  (loop for e in entries sum
        (loop for d in (outputs e) count (member (length d) '(2 4 3 7)))))


(defun decode (mapping signals &aux (rez 0))
  (dolist (s signals)
    (let ((d (signal->digit mapping s)))
      (setf rez (+ (* rez 10) d))))
  rez)


(defparameter *digits->segments* '((0 . #b1110111)
                                   (1 . #b0100100)
                                   (2 . #b1011101)
                                   (3 . #b1101101)
                                   (4 . #b0101110)
                                   (5 . #b1101011)
                                   (6 . #b1111011)
                                   (7 . #b0100101)
                                   (8 . #b1111111)
                                   (9 . #b1101111)))

(defun signal->digit (mapping s &aux (segments-mask 0))
  (doseq (ch s)
    (let ((i (position ch mapping)))
      (setf segments-mask (dpb 1 (byte 1 i) segments-mask))))
  (car (rassoc segments-mask *digits->segments*)))


(defun part2 (entries)
  (loop for e in entries
        for m = (find-mapping (inputs e))
        sum (decode m (outputs e))))

(defun find-mapping (signals)
  (labels ((recur (curr remaining)
             (cond ((loop for x in curr always (= (length x) 1))
                    (return-from find-mapping (mapcar #'car curr)))
                   ((loop for x in curr thereis (zerop (length x))) nil)
                   ((null remaining) nil)
                   (t (let ((s (first remaining)))
                        (dolist (d (possible-digits s))
                          (let ((segs (cdr (assoc d *digits->segments*))))
                            (recur
                              (loop for c in curr for i below 7
                                    collect (if (= (ldb (byte 1 i) segs) 1)
                                              (intersection c (coerce s 'list))
                                              (set-difference c (coerce s 'list))))
                              (rest remaining)))))))))
    (recur
      (loop repeat 7 collect (coerce "abcdefg" 'list))
      (sort (copy-seq signals) #'possible-digits<))))


(defparameter *length->digits* '((2 1)
                                 (3 7)
                                 (4 4)
                                 (5 2 3 5)
                                 (6 0 6 9)
                                 (7 8)))

(defun possible-digits (s) (cdr (assoc (length s) *length->digits*)))

(defun possible-digits< (s1 s2)
  (< (length (possible-digits s1)) (length (possible-digits s2))))


#+#:alternate-solution-with-bruteforce (defun part2 (entries)
  (let ((all-mappings (all-permutations (coerce "abcdefg" 'list)))
       (sum 0))
   (dolist (e entries)
     (dolist (m all-mappings)
       (when (every (partial-1 #'signal->digit m) (inputs e))
         (return (incf sum (decode m (outputs e)))))))
   sum))


(define-solution (2021 08) (entries parse-entries)
  (values (part1 entries) (part2 entries)))

(define-test (2021 08) (330 1010472))
