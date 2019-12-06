(defpackage :aoc/2019/05 #.cl-user::*aoc-use*)
(in-package :aoc/2019/05)

(defun read-program (data &aux (str (first data)))
  "XXX borrowed from day2"
  (let* ((list (split-sequence:split-sequence #\, str))
         (list (mapcar #'parse-integer list)))
    (make-array (length list) :initial-contents list)))

(defvar *input*)
(defvar *output*)

(defun immediatep (meta n)
  (let* ((mask (expt 10 (1- n)))
         (bit (mod (floor meta mask) 10)))
    (= bit 1)))

(defun write-value (memory position value)
  (setf (aref memory position) value))

(defun get-value-immediate (memory pos)
  (aref memory pos))

(defun get-value-position (memory pos)
  (aref memory (aref memory pos)))

(defun get-value (memory pos &optional immediatep)
  (if immediatep
    (get-value-immediate memory pos)
    (get-value-position memory pos)))

(defun op-add (meta)
  (lambda (memory ip)
    (let* ((left (get-value memory (+ ip 1) (immediatep meta 1)))
           (right (get-value memory (+ ip 2) (immediatep meta 2)))
           (value (+ left right)) 
           (pos (get-value-immediate memory (+ ip 3))))
      (write-value memory pos value)
      (+ ip 4))))

(defun op-mul (meta)
  (lambda (memory ip)
    (let* ((left (get-value memory (+ ip 1) (immediatep meta 1)))
           (right (get-value memory (+ ip 2) (immediatep meta 2)))
           (value (* left right)) 
           (pos (get-value-immediate memory (+ ip 3))))
      (write-value memory pos value)
      (+ ip 4))))

(defun op-in ()
  (lambda (memory ip)
    (let ((value (pop *input*))
          (pos (get-value-immediate memory (+ ip 1))))
      (write-value memory pos value)
      (+ ip 2))))

(defun op-out (meta)
  (lambda (memory ip)
    (let* ((value (get-value memory (+ ip 1) (immediatep meta 1))))
      (push value *output*)
      (+ ip 2))))

(defun op-jump-if-true (meta)
  (lambda (memory ip)
    (let* ((left (get-value memory (+ ip 1) (immediatep meta 1)))
           (right (get-value memory (+ ip 2) (immediatep meta 2))))
      (if (not (zerop left))
        right
        (+ ip 3)))))

(defun op-jump-if-false (meta)
  (lambda (memory ip)
    (let* ((left (get-value memory (+ ip 1) (immediatep meta 1)))
           (right (get-value memory (+ ip 2) (immediatep meta 2))))
      (if (zerop left)
        right
        (+ ip 3)))))

(defun op-lt (meta)
  (lambda (memory ip)
    (let* ((left (get-value memory (+ ip 1) (immediatep meta 1)))
           (right (get-value memory (+ ip 2) (immediatep meta 2)))
           (pos (get-value-immediate memory (+ ip 3))))
      (write-value memory pos (if (< left right) 1 0))
      (+ ip 4))))

(defun op-eq (meta)
  (lambda (memory ip)
    (let* ((left (get-value memory (+ ip 1) (immediatep meta 1)))
           (right (get-value memory (+ ip 2) (immediatep meta 2)))
           (pos (get-value-immediate memory (+ ip 3))))
      (write-value memory pos (if (= left right) 1 0))
      (+ ip 4))))

(defun parse-op (memory ip &aux (value (get-value-immediate memory ip)))
  (multiple-value-bind (meta code) (floor value 100)
    (case code
      (1 (op-add meta))
      (2 (op-mul meta))
      (3 (op-in))
      (4 (op-out meta))
      (5 (op-jump-if-true meta))
      (6 (op-jump-if-false meta))
      (7 (op-lt meta))
      (8 (op-eq meta))
      (99 'exit))))

(defun program-run (data input &aux (memory (read-program data)))
  (loop
    :initially (setf *input* (list input)
                     *output* NIL)
    :for ip = 0 :then (funcall op memory ip)
    :for op = (parse-op memory ip)
    :until (eql op 'exit)
    :finally (return (first *output*))))

(define-problem (2019 5) (data)
  (values
    (program-run data 1)
    (program-run data 5)))

(1am:test test-2019/05
  (multiple-value-bind (part1 part2) (problem-run)
    (1am:is (= 9938601 part1))
    (1am:is (= 4283952 part2))))
