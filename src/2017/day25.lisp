(defpackage :aoc/2017/25 #.cl-user::*aoc-use*)
(in-package :aoc/2017/25)

(defstruct (state
             (:conc-name st-))
  name
  if-zero-value
  if-zero-dir
  if-zero-next
  if-one-value
  if-one-dir
  if-one-next)

(defstruct (turing-machine
             (:constructor make-turing-machine%)
             (:conc-name tm-))
  (tape (make-hash-table))
  (cursor 0)
  states
  curr-state)

(defun make-turing-machine (states-list curr-state &aux (states (make-hash-table)))
  (dolist (s states-list)
    (hash-table-insert states (st-name s) s))
  (make-turing-machine% :states states
                        :curr-state curr-state))

(defun turing-machine-advancef (tm)
  (let* ((state (gethash (tm-curr-state tm) (tm-states tm)))
         (curr-value (gethash (tm-cursor tm) (tm-tape tm) 0))
         (next-value (if (zerop curr-value) (st-if-zero-value state) (st-if-one-value state)))
         (next-dir (if (zerop curr-value) (st-if-zero-dir state) (st-if-one-dir state)))
         (next-state (if (zerop curr-value) (st-if-zero-next state) (st-if-one-next state))))
    (setf (gethash (tm-cursor tm) (tm-tape tm)) next-value
          (tm-cursor tm) (+ (tm-cursor tm) next-dir)
          (tm-curr-state tm) next-state)))

(defun read-turing-machine-state (list)
  (flet ((read-init-state (s &aux (splits (split-sequence:split-sequence #\Space s)))
           (aref (fourth splits) 0))
         (read-steps (s &aux (splits (split-sequence:split-sequence #\Space s)))
           (parse-integer (sixth splits)))
         (read-state-name (s &aux (splits (split-sequence:split-sequence #\Space s)))
           (aref (third splits) 0))
         (read-value-to-write (s &aux (splits (split-sequence:split-sequence #\Space (subseq s 4))))
           (parse-integer (fifth splits) :junk-allowed T))
         (read-dir (s &aux (splits (split-sequence:split-sequence #\Space (subseq s 4))))
           (if (string= (seventh splits) "right.") 1 -1))
         (read-next (s &aux (splits (split-sequence:split-sequence #\Space (subseq s 4))))
           (aref (fifth splits) 0)))
    (let ((init-state (read-init-state (pop list)))
          (steps (read-steps (pop list))))
      (loop
        :while list
        :initially (pop list)
        :for name = (read-state-name (pop list))
        :for skip-0 = (pop list)
        :for if-zero-value = (read-value-to-write (pop list))
        :for if-zero-dir = (read-dir (pop list))
        :for if-zero-next = (read-next (pop list))
        :for skip-1 = (pop list)
        :for if-one-value = (read-value-to-write (pop list))
        :for if-one-dir = (read-dir (pop list))
        :for if-one-next = (read-next (pop list))
        :for skip-2 = (pop list)
        :collect (make-state :name name
                             :if-zero-value if-zero-value
                             :if-zero-dir if-zero-dir
                             :if-zero-next if-zero-next
                             :if-one-value if-one-value
                             :if-one-dir if-one-dir
                             :if-one-next if-one-next) :into states
        :finally (return (values init-state steps states))))))

(define-solution (2017 25) (data)
  (multiple-value-bind (init-state steps states)
      (read-turing-machine-state data)
    (loop
      :with tm = (make-turing-machine states init-state)
      :repeat steps
      :do (turing-machine-advancef tm)
      :finally (return (count 1 (hash-table-values (tm-tape tm)))))))

(define-test (2017 25) (5593))
