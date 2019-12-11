(defpackage :intcode
  (:use :cl :pmdb :aoc)
  (:export
    :read-program
    :make-program
    :program-memory
    :program-in
    :program-run
    :program-rb))
(in-package :intcode)

(defparameter *relative-base-index* -1)

(defun mode (meta n)
  (let* ((mask (expt 10 (1- n)))
         (bit (mod (floor meta mask) 10)))
    bit))

(defun write-value-relative (memory pos value)
  (let* ((offset (gethash *relative-base-index* memory 0)))
    (setf (gethash (+ offset pos) memory) value)))

(defun write-value-position (memory pos value)
  (setf (gethash pos memory) value))

(defun write-value (memory pos value mode)
  (ecase mode
    (2 (write-value-relative memory pos value))
    (0 (write-value-position memory pos value))))

(defun get-value-relative (memory pos)
  (let* ((offset (gethash *relative-base-index* memory 0)))
    (gethash (+ offset (gethash pos memory 0)) memory 0)))

(defun get-value-immediate (memory pos)
  (gethash pos memory 0))

(defun get-value-position (memory pos)
  (gethash (gethash pos memory 0) memory 0))

(defun get-value (memory pos mode)
  (ecase mode
    (2 (get-value-relative memory pos))
    (1 (get-value-immediate memory pos))
    (0 (get-value-position memory pos))))

(defun op-add (meta)
  (lambda (memory ip in out)
    (declare (ignore in out))
    (let* ((left (get-value memory (+ ip 1) (mode meta 1)))
           (right (get-value memory (+ ip 2) (mode meta 2)))
           (value (+ left right))
           (pos (get-value-immediate memory (+ ip 3))))
      (write-value memory pos value (mode meta 3))
      (+ ip 4))))

(defun op-mul (meta)
  (lambda (memory ip in out)
    (declare (ignore in out))
    (let* ((left (get-value memory (+ ip 1) (mode meta 1)))
           (right (get-value memory (+ ip 2) (mode meta 2)))
           (value (* left right))
           (pos (get-value-immediate memory (+ ip 3))))
      (write-value memory pos value (mode meta 3))
      (+ ip 4))))

(defun op-in (meta)
  (lambda (memory ip in out)
    (declare (ignore out))
    (if (queue-empty-p in)
      'wait
      (let ((value (dequeue in))
            (pos (get-value-immediate memory (+ ip 1))))
        (write-value memory pos value (mode meta 1))
        (+ ip 2)))))

(defun op-out (meta)
  (lambda (memory ip in out)
    (declare (ignore in))
    (let* ((value (get-value memory (+ ip 1) (mode meta 1))))
      (enqueue value out)
      (+ ip 2))))

(defun op-jump-if-true (meta)
  (lambda (memory ip in out)
    (declare (ignore in out))
    (let* ((left (get-value memory (+ ip 1) (mode meta 1)))
           (right (get-value memory (+ ip 2) (mode meta 2))))
      (if (not (zerop left))
        right
        (+ ip 3)))))

(defun op-jump-if-false (meta)
  (lambda (memory ip in out)
    (declare (ignore in out))
    (let* ((left (get-value memory (+ ip 1) (mode meta 1)))
           (right (get-value memory (+ ip 2) (mode meta 2))))
      (if (zerop left)
        right
        (+ ip 3)))))

(defun op-lt (meta)
  (lambda (memory ip in out)
    (declare (ignore in out))
    (let* ((left (get-value memory (+ ip 1) (mode meta 1)))
           (right (get-value memory (+ ip 2) (mode meta 2)))
           (pos (get-value-immediate memory (+ ip 3))))
      (write-value memory pos (if (< left right) 1 0) (mode meta 3))
      (+ ip 4))))

(defun op-eq (meta)
  (lambda (memory ip in out)
    (declare (ignore in out))
    (let* ((left (get-value memory (+ ip 1) (mode meta 1)))
           (right (get-value memory (+ ip 2) (mode meta 2)))
           (pos (get-value-immediate memory (+ ip 3))))
      (write-value memory pos (if (= left right) 1 0) (mode meta 3))
      (+ ip 4))))

(defun op-rb-add (meta)
  (lambda (memory ip in out)
    (declare (ignore in out))
    (let* ((left (get-value memory (+ ip 1) (mode meta 1)))
           (right (get-value-immediate memory *relative-base-index*))
           (value (+ left right))
           (pos *relative-base-index*))
      (write-value memory pos value (mode meta 3))
      (+ ip 2))))

(defun parse-op (memory ip &aux (value (get-value-immediate memory ip)))
  (multiple-value-bind (meta code) (floor value 100)
    (case code
      (1 (op-add meta))
      (2 (op-mul meta))
      (3 (op-in meta))
      (4 (op-out meta))
      (5 (op-jump-if-true meta))
      (6 (op-jump-if-false meta))
      (7 (op-lt meta))
      (8 (op-eq meta))
      (9 (op-rb-add meta))
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

(defun program-rb (p)
  (gethash *relative-base-index* (program-memory p) 0))

(defun read-program (data &aux (str (first data)))
  (let* ((list (split-sequence:split-sequence #\, str))
         (list (mapcar #'parse-integer list)))
    (loop
      :with memory = (make-hash-table)
      :for instruction :in list
      :for ip = 0 :then (+ ip 1)
      :do (hash-table-insert memory ip instruction)
      :finally (return (make-program memory)))))
    
