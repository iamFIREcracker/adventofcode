(defpackage :aoc/2021/24 #.cl-user::*aoc-use*)
(in-package :aoc/2021/24)


(defun read-instructions (lines)
  (loop for string in lines collect
        (loop with start = 0 and value-read while (< start (length string))
              collect (setf (values value-read start)
                            (read-from-string string nil nil :start start)))))


(define-modify-macro execf (other op) exec)
(defun exec (rand1 rand2 rator)
  (aif (simplify rator rand1 rand2)
    it
    (list rator rand1 rand2)))


(defun read-monad (lines &aux
                         (instructions (read-instructions lines))
                         (vars (make-array 4 :initial-element 0)))
  (labels ((offset (v) (ecase v (w 0) (x 1) (y 2) (z 3)))
           (value (v) (if (numberp v) v (aref vars (offset v))))
           (compile-into-lambda (e) (compile nil `(lambda (? z) ,e))))
    (uiop:while-collecting (add)
      (loop for (cmd a b) in instructions do
        (ecase cmd
          (inp (unless (eql (aref vars (offset 'z)) 0)
                 (add (compile-into-lambda (aref vars (offset 'z)))))
               (setf (aref vars 0) 'w (aref vars 1) 'x
                     (aref vars 2) 'y (aref vars 3) 'z)
               (setf (aref vars (offset a)) '?))
          (add (execf (aref vars (offset a)) (value b) '+))
          (mul (execf (aref vars (offset a)) (value b) '*))
          (div (execf (aref vars (offset a)) (value b) 'truncate))
          (mod (execf (aref vars (offset a)) (value b) 'mod))
          (eql (execf (aref vars (offset a)) (value b) 'b=))))
      ;; Make sure to pick up all the 14 ALU stages
      (add (compile-into-lambda (aref vars (offset 'z)))))))

(defun b= (n1 n2) (if (= n1 n2) 1 0))


(defun simplify (rator rand1 rand2)
  (case rator
    (+ (cond ((eql rand1 0) rand2)))
    (* (cond ((eql rand2 0) 0)))
    (truncate (cond ((eql rand2 1) rand1)))))


(defun first-valid-model (expressions &optional
                                      (input (iota 9 :start 9 :step -1))
                                      (z 0)
                                      (dp (make-hash-table))
                                      &aux
                                      (remaining (length expressions))
                                      (key (+ (* z 100) remaining)))
  (multiple-value-bind (model foundp) (gethash key dp)
    (if foundp
      model
      (setf (gethash key dp)
            (cond ((and (= remaining z 0)) 0)
                  ((= remaining 0) nil)
                  (t (loop for w in input for z-next = (funcall (car expressions) w z) do
                           (when-let (model (first-valid-model (rest expressions) input z-next dp))
                             (return (+ (* (expt 10 (1- remaining)) w) model))))))))))


(define-solution (2021 24) (monad read-monad)
  (values (first-valid-model monad) (first-valid-model monad (iota 9 :start 1))))

(define-test (2021 24) (93959993429899 11815671117121))
