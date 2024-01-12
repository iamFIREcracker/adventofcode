(defpackage :aoc/2022/11 #.cl-user::*aoc-use*)
(in-package :aoc/2022/11)


#+#:excluded (&optional (program (uiop:read-file-forms #P"src/2022/day11.txt")))

; (clockin 739)
; (clockout 909)
; (clockin 958)
; (idea repetitions w/ deltas)
; (clockout 1000)
; (clockin 1042)
; (idea lcm of all the divisors)


(defstruct (monkey (:conc-name nil))
  items
  operation
  div-by
  throw-if-div
  throw-if-not-div)

(defun parse-number (s) (car (parse-numbers s)))
(defun parse-numbers (s) (extract-positive-integers s))

(defun parse-operation (s)
  (bnd1 (old (symb "_"))
    (flet ((make-operand (s)
             (if (string= s "old") old (parse-integer s))))
      (cl-ppcre:register-groups-bind ((#'symb rator) (#'make-operand rand))
          ("new = old (.+) (.+)" s)
        (eval `(lambda (,old)
                 (,rator ,old ,rand)))))))
(parse-operation "  Operation: new = old + 7")
(parse-operation "  Operation: new = old * old")

(defun parse-div-by (s) (parse-number s))
(parse-div-by "  Test: divisible by 2")

(defun parse-throw-if-div (s) (parse-number s))
(parse-throw-if-div "    If true: throw to monkey 5")

(defun parse-throw-if-div (s) (parse-number s))
(parse-throw-if-div "    If true: throw to monkey 5")



; (make-array 7 :initial-contents (list
;                                   (make-monkey '(89  73 66 57 64 80)
;                                                [* _ 3]
;                                                [if (dividesp _ 13) 6 2])
;                                   (make-monkey '(83 78 81 55 81 59 69)
;                                                [+ _ 1]
;                                                [if (dividesp _ 3) 7 4])
;                                   (make-monkey '(76 91 58 85)
;                                                [* _ 13]
;                                                [if (dividesp _ 7) 1 4])
;                                   (make-monkey '(71  72  74 76 68)
;                                                [* _ _]
;                                                [if (dividesp _ 2) 1 4]) ))


(defun parse-monkeys (&optional (strings (uiop:read-file-lines #P"src/2022/day11.txt")))
  (let* ((groups (split-sequence:split-sequence "" strings :test #'string=))
         (monkeys (make-array (length groups))))
    (dolist (g groups)
      (setf (aref monkeys (parse-number (pop g)))
            (make-monkey :items (loop with q = (make-queue)
                                      for e in (parse-numbers (pop g)) do (enqueue e q)
                                      finally (return q))
                         :operation (parse-operation (pop g))
                         :div-by (parse-number (pop g))
                         :throw-if-div (parse-number (pop g))
                         :throw-if-not-div (parse-number (pop g)))))
    monkeys))

#+#:excluded (defun solve (&optional (monkeys (parse-monkeys #+#:excluded (uiop:read-file-lines #P"scratch.txt"))))
               (bnd1 (counts (make-array (length monkeys) :initial-element 0))
                 (loop repeat 20 do
                       (loop for m across monkeys for i from 0 do
                             (loop until (queue-empty-p (items m)) for wl = (dequeue (items m)) do
                                   (incf (aref counts i))
                                   (bnd1 nwl
                                     (setf nwl (funcall (operation m) wl))
                                     (setf nwl (floor nwl 3))
                                     (if (dividesp (div-by m) nwl)
                                       (enqueue nwl (items (aref monkeys (throw-if-div m))))
                                       (enqueue nwl (items (aref monkeys (throw-if-not-div m)))))))))
                 (reduce #'* (sort counts #'>) :end 2)))


#+#:excluded (solve)

;; I don't know how to use divides anymore (flipped the order of its operands)

(defun solve (&optional (monkeys (parse-monkeys #+#:excluded (uiop:read-file-lines #P"scratch.txt"))))
  (let ((counts (make-array (length monkeys) :initial-element 0))
        (lcm (reduce #'lcm (map 'list #'div-by monkeys))))
    (loop repeat 10000 for turn from 0 do
          (loop for m across monkeys for i from 0 do
                (loop until (queue-empty-p (items m)) for wl = (dequeue (items m)) do
                      (incf (aref counts i))
                      (bnd1 nwl
                        (setf nwl (rem (funcall (operation m) wl) lcm))
                        #+#:excluded (setf nwl (floor nwl 3))
                        (if (dividesp (div-by m) nwl)
                          (enqueue nwl (items (aref monkeys (throw-if-div m))))
                          (enqueue nwl (items (aref monkeys (throw-if-not-div m)))))))))
    (reduce #'* (sort counts #'>) :end 2)))

#+#:excluded (solve)

;;;

(defstruct (monkey (:conc-name nil))
  items
  operation
  div-by
  throw-if-div
  throw-if-not-div)

(defun parse-number (s) (car (parse-numbers s)))
(defun parse-numbers (s) (extract-positive-integers s))

(defun parse-operation (s)
  (bnd1 (old (symb "_"))
    (flet ((make-operand (s)
             (if (string= s "old") old (parse-integer s))))
      (cl-ppcre:register-groups-bind ((#'symb rator) (#'make-operand rand))
          ("new = old (.+) (.+)" s)
        (eval `(lambda (,old)
                 (,rator ,old ,rand)))))))
#+#:excluded (parse-operation "  Operation: new = old + 7")
#+#:excluded (parse-operation "  Operation: new = old * old")


(defun parse-monkeys (&optional (strings (uiop:read-file-lines #P"src/2022/day11.txt")))
  (let* ((groups (split-sequence:split-sequence "" strings :test #'string=))
         (monkeys (make-array (length groups))))
    (dolist (g groups)
      (setf (aref monkeys (parse-number (pop g)))
            (make-monkey :items (loop with q = (make-queue)
                                      for e in (parse-numbers (pop g)) do (enqueue e q)
                                      finally (return q))
                         :operation (parse-operation (pop g))
                         :div-by (parse-number (pop g))
                         :throw-if-div (parse-number (pop g))
                         :throw-if-not-div (parse-number (pop g)))))
    monkeys))

(defun solve (turns &optional (monkeys (parse-monkeys)) part2?)
  (let ((counts (make-array (length monkeys) :initial-element 0))
        (lcm (reduce #'lcm (map 'list #'div-by monkeys))))
    (loop repeat turns for turn from 0 do
          (loop for m across monkeys for i from 0 do
                (loop until (queue-empty-p (items m)) for wl = (dequeue (items m)) do
                      (incf (aref counts i))
                      (bnd1 nwl
                        (setf nwl (rem (funcall (operation m) wl) lcm))
                        (unless part2?
                          (setf nwl (floor nwl 3)))
                        (if (dividesp (div-by m) nwl)
                          (enqueue nwl (items (aref monkeys (throw-if-div m))))
                          (enqueue nwl (items (aref monkeys (throw-if-not-div m)))))))))
    (reduce #'* (sort counts #'>) :end 2)))

(define-solution (2022 11) (strings)
  (values (solve 20    (parse-monkeys strings))
          (solve 10000 (parse-monkeys strings) t)))

(define-test (2022 11) (119715 18085004878))
