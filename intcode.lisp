(defpackage :intcode
  (:use :cl :pmdb :aoc)
  (:export
    :read-program
    :make-program
    :program-memory
    :program-in
    :program-run))
(in-package :intcode)

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
  (lambda (memory ip in out)
    (declare (ignore in out))
    (let* ((left (get-value memory (+ ip 1) (immediatep meta 1)))
           (right (get-value memory (+ ip 2) (immediatep meta 2)))
           (value (+ left right))
           (pos (get-value-immediate memory (+ ip 3))))
      (write-value memory pos value)
      (+ ip 4))))

(defun op-mul (meta)
  (lambda (memory ip in out)
    (declare (ignore in out))
    (let* ((left (get-value memory (+ ip 1) (immediatep meta 1)))
           (right (get-value memory (+ ip 2) (immediatep meta 2)))
           (value (* left right))
           (pos (get-value-immediate memory (+ ip 3))))
      (write-value memory pos value)
      (+ ip 4))))

(defun op-in ()
  (lambda (memory ip in out)
    (declare (ignore out))
    (if (queue-empty-p in)
      'wait
      (let ((value (dequeue in))
            (pos (get-value-immediate memory (+ ip 1))))
        (write-value memory pos value)
        (+ ip 2)))))

(defun op-out (meta)
  (lambda (memory ip in out)
    (declare (ignore in))
    (let* ((value (get-value memory (+ ip 1) (immediatep meta 1))))
      (enqueue value out)
      (+ ip 2))))

(defun op-jump-if-true (meta)
  (lambda (memory ip in out)
    (declare (ignore in out))
    (let* ((left (get-value memory (+ ip 1) (immediatep meta 1)))
           (right (get-value memory (+ ip 2) (immediatep meta 2))))
      (if (not (zerop left))
        right
        (+ ip 3)))))

(defun op-jump-if-false (meta)
  (lambda (memory ip in out)
    (declare (ignore in out))
    (let* ((left (get-value memory (+ ip 1) (immediatep meta 1)))
           (right (get-value memory (+ ip 2) (immediatep meta 2))))
      (if (zerop left)
        right
        (+ ip 3)))))

(defun op-lt (meta)
  (lambda (memory ip in out)
    (declare (ignore in out))
    (let* ((left (get-value memory (+ ip 1) (immediatep meta 1)))
           (right (get-value memory (+ ip 2) (immediatep meta 2)))
           (pos (get-value-immediate memory (+ ip 3))))
      (write-value memory pos (if (< left right) 1 0))
      (+ ip 4))))

(defun op-eq (meta)
  (lambda (memory ip in out)
    (declare (ignore in out))
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

(defstruct (program (:constructor make-program%))
  memory
  in
  out
  ip)

(defun make-program (memory &optional (in (make-queue)) (out (make-queue)))
  (make-program% :memory memory
                 :in in 
                 :out out
                 :ip 0))

(defun program-next (p)
  "Runs the next instruction, and then return the its op-code"
  (let ((op (parse-op (program-memory p) (program-ip p))))
    (if (eql op 'exit)
      op
      (let ((next-ip (funcall
                       op
                       (program-memory p)
                       (program-ip p)
                       (program-in p)
                       (program-out p))))
        (if (eql next-ip 'wait)
          'wait
          (prog1 'running
            (setf (program-ip p) next-ip)))))))

(defun program-run (p)
  (loop
    :for status = (program-next p)
    :until (eql status 'exit)
    :when (eql status 'wait) :return T))

(defun read-program (data &aux (str (first data)))
  (let* ((list (split-sequence:split-sequence #\, str))
         (list (mapcar #'parse-integer list)))
    (make-program (make-array (length list) :initial-contents list))))
