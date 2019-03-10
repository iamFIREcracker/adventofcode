(defpackage :aoc/2017/18 #.cl-user::*aoc-use*
  (:export
    :ip-pos
    :*registers*
    :reg-name-to-pos
    :value-or-reg-content
    :i-set
    :i-mul
    :*instructions-by-name*
    :make-program
    :program-registers
    :program-next-instruction-name
    :program-exec-next-instruction
    :parse-instructions))
(in-package :aoc/2017/18)

(defconstant ip-pos 26)

(defparameter *part2* NIL)
(defparameter *programs* NIL)
(defparameter *id* NIL)
(defparameter *registers* NIL)
(defparameter *shared* NIL)

(defun reg-name-to-pos (name)
  (- (char-int name) (char-int #\a)))

(defun value-or-reg-content (y)
  (if (numberp y)
    y
    (aref *registers* (reg-name-to-pos y))))

(defun i-snd (x)
  (if (not *part2*)
    (setf *shared* (value-or-reg-content x))
    (let* ((queue-pos (mod (1+ *id*) (length *shared*)))
           (queue (aref *shared* queue-pos)))
      (setf (aref *shared* queue-pos) (nconc queue (list (value-or-reg-content x)))))))

(defun i-set (x y &aux (pos (reg-name-to-pos x)))
  (setf (aref *registers* pos) (value-or-reg-content y)))

(defun i-add (x y &aux (pos (reg-name-to-pos x)))
  (incf (aref *registers* pos) (value-or-reg-content y)))

(defun i-mul (x y &aux (pos (reg-name-to-pos x)))
  (setf (aref *registers* pos) (* (aref *registers* pos)
                                (value-or-reg-content y))))

(defun i-mod (x y &aux (pos (reg-name-to-pos x)))
  (setf (aref *registers* pos) (mod (aref *registers* pos)
                                  (value-or-reg-content y))))

(defun i-rcv (x &aux (pos (reg-name-to-pos x)))
  (if (not *part2*)
    (when (plusp (value-or-reg-content x))
      *shared*)
    (setf (aref *registers* pos) (pop (aref *shared* *id*)))))

(defun i-jgz (x y)
  (when (plusp (value-or-reg-content x))
    (incf (aref *registers* ip-pos) (1- (value-or-reg-content y)))))

(defparameter *instructions-by-name* NIL)
(defun instruction-by-name (name)
  (second (assoc name *instructions-by-name* :test 'equal)))

(defstruct (instruction
             (:copier NIL)
             (:conc-name NIL))
  name
  args
  fun)

(defun parse-instructions (x)
  (labels ((parse-value (s &aux (n (parse-integer s :junk-allowed T)))
             (if n n (aref s 0)))
           (parse-instruction (s &aux (splits (split-sequence:split-sequence #\Space s)))
             (let ((name (first splits))
                   (args (mapcar #'parse-value (subseq splits 1))))
               (make-instruction :name name
                                 :args args
                                 :fun (instruction-by-name name)))))
    (mapcar #'parse-instruction x)))

(defstruct (program
             (:constructor make-program%)
             (:copier NIL))
  id
  registers
  instructions)

(defun make-program (id instructions)
  (let ((registers (make-array 27 :initial-element 0)))
    (setf (aref registers (reg-name-to-pos #\p)) id)
    (make-program% :id id
                   :registers registers
                   :instructions instructions)))

(defun program-next-instruction-name (p)
  (let* ((ip (aref (program-registers p) ip-pos))
         (instruction (nth ip (program-instructions p))))
    (and instruction (name instruction))))

(defun program-exec-next-instruction (p)
  (let* ((ip (aref (program-registers p) ip-pos))
         (instruction (nth ip (program-instructions p))))
    (prog1
      (apply (fun instruction) (args instruction))
      (incf (aref (program-registers p) ip-pos)))))

(defun solve-part1 (data)
  (loop
    :with *part2*
    :with current = (make-program 0 data)
    :with *registers* = (program-registers current)
    :for name = (program-next-instruction-name current)
    :for ret = (program-exec-next-instruction current)
    :when (string= "rcv" name) :return ret))

(defun solve-part2 (data)
  (labels ((rcv-queue-not-empty-p (p)
             (aref *shared* (program-id p)))
           (program-can-execute-p (p)
             (let ((next (program-next-instruction-name p)))
               (or (not (string= "rcv" next)) (rcv-queue-not-empty-p p))))
           (deadlockp (programs)
             (not (some #'program-can-execute-p programs))))
    (loop
      :with *part2* = T
      :with *programs* = (make-array 2 :initial-contents (list 
                                                           (make-program 0 data)
                                                           (make-program 1 data)))
      :with *shared* = (make-array 2 :initial-element NIL)
      :with current = (aref *programs* 0)
      :with *id* = (program-id current)
      :with *registers* = (program-registers current)
      :until (deadlockp *programs*)
      :counting (and (= 1 *id*) (string= "snd" (program-next-instruction-name current)))
      :do (if (program-can-execute-p current)
            (program-exec-next-instruction current)
            (setf current (aref *programs* (mod (1+ *id*) (length *programs*)))
                  *id* (program-id current)
                  *registers* (program-registers current))))))

(define-problem (2017 18) (data parse-instructions)
  (setf *instructions-by-name* `(("snd" ,#'i-snd)
                                 ("set" ,#'i-set)
                                 ("add" ,#'i-add)
                                 ("mul" ,#'i-mul)
                                 ("mod" ,#'i-mod)
                                 ("rcv" ,#'i-rcv)
                                 ("jgz" ,#'i-jgz)))
  (values
    (solve-part1 data)
    (solve-part2 data)))

(1am:test test-2017/18
  (multiple-value-bind (part1 part2) (problem-run)
    (1am:is (= 3423 part1))
    (1am:is (= 7493 part2))))
