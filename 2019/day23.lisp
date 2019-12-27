(defpackage :aoc/2019/23 #.cl-user::*aoc-use*)
(in-package :aoc/2019/23)

(defun make-network (program &aux (nics (make-array 50)))
  (prog1 nics
    (loop
      :for n :from 0 :upto 49
      :for nic = (intcode:make-program (copy-hash-table (intcode:program-memory program)))
      :do (enqueue n (intcode:program-in nic))
      :do (setf (aref nics n) nic))))

(defun solve-part1 (nics)
  (loop
    :for i = 0 :then (mod (1+ i) (length nics))
    :for nic = (aref nics i)
    :for out = (intcode:program-out nic)
    :for in = (intcode:program-in nic)
    :do (intcode:program-run nic)
    :do (enqueue -1 in)
    :do (loop
          :until (queue-empty-p out)
          :for address = (dequeue out)
          :for x = (dequeue out)
          :for y = (dequeue out)
          :if (= address 255) :do (return-from solve-part1 y)
          :else :do (progn
                      (enqueue x (intcode:program-in (aref nics address)))
                      (enqueue y (intcode:program-in (aref nics address)))))))

(defun network-idlep (nics)
  (loop
    :for nic :across nics
    :for out = (intcode:program-out nic)
    :for in = (intcode:program-in nic)
    :always (and (queue-empty-p out))))

(defun solve-part2 (nics)
  (loop
    :with inactive
    :with nat-x
    :with nat-y
    :with nat-y-prev
    :for i = 0 :then (mod (1+ i) (length nics))
    :for nic = (aref nics i)
    :for out = (intcode:program-out nic)
    :for in = (intcode:program-in nic)
    :do (intcode:program-run nic)
    :do (enqueue -1 in)
    :if (queue-empty-p out) :do (setf inactive (union inactive (list i)))
    :else :do (loop
                :for address = (dequeue out)
                :for x = (dequeue out)
                :for y = (dequeue out)
                :if (= address 255) :do (setf nat-x x
                                              nat-y y)
                :else :do (progn
                            (enqueue x (intcode:program-in (aref nics address)))
                            (enqueue y (intcode:program-in (aref nics address)))
                            (setf inactive (remove address inactive)))
                :until (queue-empty-p out))
    :when (= (length nics) (length inactive))
    :do (progn
          (when (and nat-y (eql nat-y nat-y-prev))
            (return-from solve-part2 nat-y))
          (when nat-y
            (enqueue nat-x (intcode:program-in (aref nics 0)))
            (enqueue nat-y (intcode:program-in (aref nics 0))))
          (setf i -1
                nat-y-prev nat-y
                inactive NIL))))

(define-problem (2019 23) (program intcode:read-program)
  (values
    (solve-part1 (make-network program))
    (solve-part2 (make-network program))))

(1am:test test-2019/23
  (multiple-value-bind (part1 part2) (problem-run)
    (1am:is (= 16685 part1))
    (1am:is (= 11048 part2))))
