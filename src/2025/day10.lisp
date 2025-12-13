(defpackage :aoc/2025/10 #.cl-user::*aoc-use*)
(in-package :aoc/2025/10)

(defun read-input (&optional (strings (uiop:read-file-lines #P"src/2025/day10.txt")))
  (looping
    (dolist (s strings)
      (destructuring-bind (indicator wirings joltage)
                          (split-sequence:split-sequence-if [find _ "]{"] s)
        (collect! (list (map 'list [char= #\# _] (subseq indicator 1))
                        (looping
                          (dolist (s2 (split-sequence:split-sequence #\Space (string-trim " " wirings)))
                            (collect! (extract-integers s2))))
                        (extract-integers joltage)))))))
#+#:excluded (read-input)

(defun press (state wiring)
  (prog1-let state2 (copy-seq state)
    (dolist (i wiring)
      (setf (nth i state2) (not (nth i state2))))))

(defun configure (problem)
  (destructuring-bind (goal wirings) (butlast problem)
    (search-cost
      (bfs (make-list (length goal))
           :test 'equal
           :goal-state goal
           :neighbors (fn (state)
                        (looping
                          (dolist (w wirings)
                            (collect! (press state w)))))))))
#+#:excluded (configure (car (read-input)))
#+#:excluded (reduce #'+ (read-input) :key #'configure)
; 432

(defun best-index (state)
  (looping
    (doseq ((i c) (enumerate state))
      (when (plusp c)
        (minimize! i :key (constantly c))))))

(defun wirings-for-counter (i wirings)
  "Returns all the wirins that touch counter `i`"
  (keep-if [member i _] wirings))

(defun unpress (state wiring)
  (prog1-let state2 (copy-seq state)
    (dolist (i wiring)
      (decf (nth i state2)))))

(defun configure2 (problem)
  (dbgl problem)
  (destructuring-bind (wirings goal) (cdr problem)
    (let1 memo (make-hash-table :test 'equal)
      (recursively ((state goal))
        (memoizing (memo state)
          (dbgl state)
          (break)
          (cond ((every 'zerop state) 0)
                ((some 'minusp state) nil)
                (t (let1 i (best-index state)
                     (when i
                       (looping
                         (dolist (w (wirings-for-counter i wirings))
                           (aif (recur (unpress state w))
                                (minimize! (1+ it))))))))))))))

#+#:excluded (configure2 (first (read-input)))
#+#:excluded (configure2 (second (read-input)))
#+#:excluded (configure2 (third (read-input)))
#+#:excluded (configure2 (fourth (read-input)))
#+#:excluded (reduce #'+ (read-input) :key #'configure2)
; 17995 too low
; 18011


#+#:excluded (ql:quickload :linear-programming)
#+#:excluded (ql:quickload :linear-programming-glpk)
(defun var-name (j)
  (symb (code-char (+ (char-code #\A) j))))

(defun configure2 (problem)
  (destructuring-bind (wirings goal) (cdr problem)
    (dbgl wirings goal)
    (let ((objective-exp (list 'min (list* '+
                                           (looping
                                             (dotimes (j (length wirings))
                                               (collect! (var-name j)))))))
          (constraints (looping
                         (doseq ((i c) (enumerate goal))
                           (collect! (list '=
                                           (list* '+
                                                  (looping
                                                    (doseq ((j w) (enumerate wirings))
                                                      (when (member i w)
                                                        (collect! (var-name j))))))
                                           c)))
                         #+#:excluded (doseq ((j w) (enumerate wirings))
                                        (collect! (list '>= (var-name j) 0)))
                         (collect! (list* 'integer (looping
                                                     (dotimes (j (length wirings))
                                                       (collect! (var-name j)))))))))
      #+#:excluded (dbgl objective-exp constraints)
      (setf linear-programming:*solver* linear-programming-glpk:glpk-solver)
      #+#:excluded (setf linear-programming:*solver* linear-programming::'simplex-solver)


      (let1 lin-prob (linear-programming:parse-linear-problem objective-exp
                                                              constraints)
        (let1 solution (linear-programming:solve-problem lin-prob)
          (linear-programming:solution-objective-value solution))))))


#;

       b0  b1    b2  b3    b4    b5     c0 c1 c2, c3
[.##.] (3) (1,3) (2) (2,3) (0,2) (0,1) {3, 5, 4,  7}

b4 + b5 = 3
b1 + b5 = 5
b2 + b3 + b4 = 4
b0 + b1 + b3 = 7

e + f = 3
b + f = 5
c + d + e = 4
a + b + d = 7


