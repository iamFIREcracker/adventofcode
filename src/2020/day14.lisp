(defpackage :aoc/2020/14 #.cl-user::*aoc-use*)
(in-package :aoc/2020/14)

(defun parse-mask (string)
  (cl-ppcre:register-groups-bind (mask)
      ("mask = ([10X]+)" string)
    (let ((zeros 0) (ones 0))
      (loop for c across (reverse mask) for index from 0
            for byte = (byte 1 index)
            when (char= c #\0) do (setf zeros (dpb 1 byte zeros))
            when (char= c #\1) do (setf ones (dpb 1 byte ones)))
      (list :mask (cons zeros ones)))))

(defun zeros (mask) (car mask))
(defun ones (mask) (cdr mask))

(defun parse-mem (string)
  (cl-ppcre:register-groups-bind ((#'parse-integer address value))
      ("mem\\[(\\d+)\\] = (\\d+)" string)
    (list :mem (cons address value))))

(defun address (mem) (car mem))
(defun value (mem) (cdr mem))

(defun parse-line (string)
  (or (parse-mask string) (parse-mem string)))

(defun parse-instructions (data)
  (mapcar #'parse-line data))

(defun value-mask (value mask)
  (logior (logand value (lognot (zeros mask)))
          (ones mask)))

(defun part1 (instructions)
  (loop with mask with mem = (make-hash-table)
        for (type instr) in instructions
        if (eql type :mask) do (setf mask instr)
        else do (setf (gethash (address instr) mem)
                      (value-mask (value instr) mask))
        finally (return (reduce #'+ (hash-table-values mem)))))

(defun masked-addresses (addr mask &aux (dp (list 0)))
  (dotimes (index 36 dp)
    (loop with prev = dp initially (setf dp nil) for number in prev
          for byte = (byte 1 index) do
          (cond ((logbitp index (zeros mask))
                 (push (dpb (ldb byte addr) byte number) dp))
                ((logbitp index (ones mask))
                 (push (dpb 1 byte number) dp))
                (t (push (dpb 0 byte number) dp)
                   (push (dpb 1 byte number) dp))))))

(defun part2 (instructions)
  (loop with mask with mem = (make-hash-table)
        for (type instr) in instructions
        if (eql type :mask) do (setf mask instr)
        else do (loop for addr in (masked-addresses (address instr) mask)
                      do (setf (gethash addr mem) (value instr)))
        finally (return (reduce #'+ (hash-table-values mem)))))

(define-solution (2020 14) (instructions parse-instructions)
  (values (part1 instructions) (part2 instructions)))

(define-test (2020 14) (10050490168421 2173858456958))
