(defpackage :aoc/2024/17 #.cl-user::*aoc-use*)
(in-package :aoc/2024/17)

#;
(sb-ext:gc :full t)

(defun parse-input (&optional (strings (uiop:read-file-lines #P"src/2024/day17.txt")))
  (destructuring-bind (registers program) (split-sequence:split-sequence "" strings :test 'equal)
    (list (mapcan #'extract-positive-integers registers)
          (coerce (extract-positive-integers (first program)) 'vector))))
#+#:excluded (parse-input)
(defaccessor a (r) (accesses (car r)))
(defaccessor b (r) (accesses (cadr r)))
(defaccessor c (r) (accesses (caddr r)))

; (defun literal (regs rand)
;   (declare (ignore regs))
;   rand)

(defun combo (regs rand)
  (ecase rand
    ((0 1 2 3) rand)
    (4 (a regs))
    (5 (b regs))
    (6 (c regs))
    (7 (error "Unused"))))

(destructuring-bind (regs program) (parse-input)
  (let1 ip 0
    (labels ((next () (prog1 (aref program ip) (incf ip))))
      (format nil "~{~A~^,~}"
              (looping
                (while (array-in-bounds-p program ip)
                  (let ((instr (next))
                        (rand (next)))
                    (ecase instr
                      (0 (setf (a regs) (floor (a regs) (expt 2 (combo regs rand)))))
                      (1 (setf (b regs) (logxor (b regs) rand)))
                      (2 (setf (b regs) (mod (combo regs rand) 8)))
                      (3 (if (/= (a regs) 0) (setf ip rand)))
                      (4 (setf (b regs) (logxor (b regs) (c regs))))
                      (5 (collect! (mod (combo regs rand) 8)))
                      (6 (setf (b regs) (floor (a regs) (expt 2 (combo regs rand)))))
                      (7 (setf (c regs) (floor (a regs) (expt 2 (combo regs rand))))) ))))))))
6,1,6,4,2,4,7,3,5

(destructuring-bind (regs program) (parse-input)
  (let1 ip 0
    (setf (a regs) 'x)
    (labels ((next () (prog1 (aref program ip) (incf ip))))
      (format nil "~{~A~^,~}"
              (looping
                (let1 outd 0
                  (while (and (array-in-bounds-p program ip)
                              (< outd (length program)))
                    (let ((instr (next))
                          (rand (next)))
                      (ecase instr
                        (0 (setf (a regs) `(floor ,(a regs) (expt 2 ,(combo regs rand)))))
                        (1 (setf (b regs) `(logxor ,(b regs) ,rand)))
                        (2 (setf (b regs) `(mod ,(combo regs rand) 8)))
                        (3 (dbgl (a regs)) (setf ip 0 (a regs) 'x) (continuable (break)))
                        (4 (setf (b regs) `(logxor ,(b regs) ,(c regs))))
                        (5 (incf outd) (collect! (dbg `(mod ,(combo regs rand) 8))))
                        (6 (setf (b regs) `(floor ,(a regs) (expt 2 ,(combo regs rand)))))
                        (7 (setf (c regs) `(floor ,(a regs) (expt 2 ,(combo regs rand)))))
                        )))))))))

; (MOD
;  (LOGXOR (LOGXOR (LOGXOR (MOD X 8) 1) 4)
;          (FLOOR X (EXPT 2 (LOGXOR (MOD X 8) 1))))
;  8)

; (A REGS) (FLOOR X (EXPT 2 3))

(defun solves? (target x)
  (= (mod
       (logxor (logxor (logxor (mod x 8) 1) 4)
               (floor x (expt 2 (logxor (mod x 8) 1))))
       8)
     target))

(destructuring-bind (regs program) (parse-input)
  (zapf program [reverse (coerce _ 'list)])
  (looping
    (labels ((recur (program x)
               (cond ((null program) (minimize! (floor x 8)))
                     (t (if (solves? (car program) x)
                            (dorangei (x1 (* x 8) (+ (* x 8) 7))
                              (recur (cdr program) x1))))
                     (t (error "NEVER")))))
      (dolist (x (iota 8))
        (recur program x)))))
1623801469161855; too high
1623801469161808; too high
202975183645226
