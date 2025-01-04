(defpackage :aoc/2024/17 #.cl-user::*aoc-use*)
(in-package :aoc/2024/17)

(defun parse-input (&optional (strings (uiop:read-file-lines #P"src/2024/day17.txt")))
  (destructuring-bind (registers program) (split-sequence:split-sequence "" strings :test 'equal)
    (list (mapcan #'extract-positive-integers registers)
          (coerce (extract-positive-integers ~program.first) 'vector))))
#+#:excluded (parse-input)

(defaccessor a (r) (accesses ~r.car))
(defaccessor b (r) (accesses ~r.cadr))
(defaccessor c (r) (accesses ~r.caddr))


(defun combo (regs rand)
  (ecase rand
    ((0 1 2 3) rand)
    (4 ~regs.a)
    (5 ~regs.b)
    (6 ~regs.c)
    (7 (error "Unused"))))

(defun run (&optional (input (parse-input)))
  (destructuring-bind (regs program) input
    (let1 ip 0
      (labels ((next () (prog1 (aref program ip) (incf ip))))
        (format nil "~{~A~^,~}"
                (looping
                  (while (array-in-bounds-p program ip)
                    (let ((instr (next))
                          (rand (next)))
                      (ecase instr
                        (0 (setf ~regs.a (floor ~regs.a (expt 2 (combo regs rand)))))
                        (1 (setf ~regs.b (logxor ~regs.b rand)))
                        (2 (setf ~regs.b (mod (combo regs rand) 8)))
                        (3 (if (/= ~regs.a 0) (setf ip rand)))
                        (4 (setf ~regs.b (logxor ~regs.b ~regs.c)))
                        (5 (collect! (mod (combo regs rand) 8)))
                        (6 (setf ~regs.b (floor ~regs.a (expt 2 (combo regs rand)))))
                        (7 (setf ~regs.c (floor ~regs.a (expt 2 (combo regs rand))))) ))))))))  )


;; By inspecting the input one could see that the program is just a plain loop,
;; repeating the same instructions over and over again; the last instruction is
;; usually a jnz/3, and before that you will usually find a out/5.  Letting the
;; program run you will notice two more things: that reg B and C always get
;; reset somewhere at the beginning of each iteration (which means we can
;; define what out/5 will output as a function of reg A at the beginning of the
;; iteration); second, that reg A keeps on getting reduced via adv/0.
;;
;; For example, if we make X the content of reg A at the beginning of each
;; iteration, my program will out/5 the result of the following expression:
;;
;;     (MOD
;;      (LOGXOR (LOGXOR (LOGXOR (MOD X 8) 1) 4)
;;              (FLOOR X (EXPT 2 (LOGXOR (MOD X 8) 1))))
;;      8)
;;
;; Moreover, thanks to adv/0, the program will keep on reducing reg A using the
;; following expression:
;;
;;     (FLOOR X (EXPT 2 3))
;;
;; How is this useful?  Well, we need to find the initial value of reg A such
;; that the program will output itself.  Since all that matters is the value of
;; reg A at the beginning of each iteration, we can start from the end, and
;; figure out which value of reg A will generate the last operand; then, by
;; reversing the effect of adv/0, we can try to figure out which value of reg
;; A at the second last iteration will cause out/5 to output the last opcode;
;; then, move on to the second last operand, until we get to the first opcode.
;; 
;; For example: my program ends with "3,0".  If we want the last iteration to
;; output 0, we need to find a value of reg A, X, that satisfies:
;;
;;     (= (MOD
;;         (LOGXOR (LOGXOR (LOGXOR (MOD X 8) 1) 4)
;;                 (FLOOR X (EXPT 2 (LOGXOR (MOD X 8) 1))))
;;         8)
;;        0) ; last operand of my program
;;
;; Now, we know the program should halt, which implies reg A should to be 0 at
;; the end of the iteration; reg A gets updated via (FLOOR X (EXPT 2 3)), so if
;; we reverse that, we will find ourselves to check all the values such that:
;;
;;     (= (FLOOR X (EXPT 2 3))
;;        0) ; so the program halts
;;
;; This means reg A, i.e., X, at the beginning of the last iteration could
;; only assume one of the following values: 0, 1, 2, 3, 4, 5, 6, 7.  Which of these
;; will cause out/5 to output 0? 5!
;;
;; Moving on, if we want the second last iteration to output 3 we need to find
;; X such that:
;;
;;     (= (MOD
;;         (LOGXOR (LOGXOR (LOGXOR (MOD X 8) 1) 4)
;;                 (FLOOR X (EXPT 2 (LOGXOR (MOD X 8) 1))))
;;         8)
;;        3) ; last opcode of my program
;; 
;; In this case, all the possible values of reg A to check are all the values
;; such that:
;;
;;     (= (FLOOR X (EXPT 2 3))
;;        5) ; so it will out/5 the right thing during the next iteration
;;
;; Or, more explicitly: 40, 41, 42, 43, 44, 45, 46, 47.

(defun find-program-specifics (&optional (input (parse-input)))
  (destructuring-bind (regs program) input
    (let1 ip 0
      (setf ~regs.a 'x)
      (labels ((next () (prog1 (aref program ip) (incf ip))))
        (let (out-expr adv-expr)
          (while (array-in-bounds-p program ip)
            (let ((instr (next))
                  (rand (next)))
              (ecase instr
                (0 (setf adv-expr `(floor ,~regs.a (expt 2 ,(combo regs rand)))))
                (1 (setf ~regs.b `(logxor ,~regs.b ,rand)))
                (2 (setf ~regs.b `(mod ,(combo regs rand) 8)))
                (3 :noop)
                (4 (setf ~regs.b `(logxor ,~regs.b ,~regs.c)))
                (5 (setf out-expr `(mod ,(combo regs rand) 8)))
                (6 (setf ~regs.b `(floor ,~regs.a (expt 2 ,(combo regs rand)))))
                (7 (setf ~regs.c `(floor ,~regs.a (expt 2 ,(combo regs rand))))))))
          (values out-expr adv-expr))))))
#+#:excluded (find-program-specifics)


(defun find-reg-a-for-quine (&optional (input (parse-input)) &aux (program ~input.second))
  (multiple-value-bind (out-expr adv-expr) (find-program-specifics input)
    (assert (equal adv-expr '(floor x (expt 2 3))))
    (flet ((candidates (x) (iota 8 :start (* x 8))))
      (let1 out-fn (eval `(lambda (x) ,out-expr))
        (let1 rev-program (~> program (coerce ~ 'list) reverse)
          (looping
            (labels ((recur (remaining x)
                       (cond ((null remaining) (minimize! (floor x 8)))
                             (t (if (= (funcall out-fn x) ~remaining.car)
                                    (dolist (x1 (candidates x))
                                      (recur ~remaining.cdr x1)))))))
              (dolist (x (candidates 0))
                (recur rev-program x)))))))))
#+#:excluded (find-reg-a-for-quine)


(define-solution (2024 17) (input parse-input)
  (values (run input)
          (find-reg-a-for-quine input)))

(define-test (2024 17) ("6,1,6,4,2,4,7,3,5" 202975183645226))
