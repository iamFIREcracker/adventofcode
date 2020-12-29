(defpackage :aoc/2019/07 #.cl-user::*aoc-use*)
(in-package :aoc/2019/07)

(defstruct (acs (:constructor make-acs%))
  amps
  in
  out)

(defun make-acs (program phases &key feedback-loop-p)
  (let* ((in (make-queue))
         (a-to-b (make-queue))
         (b-to-c (make-queue))
         (c-to-d (make-queue))
         (d-to-e (make-queue))
         (out (if feedback-loop-p in (make-queue)))
         (amps (list (intcode:make-program (copy-hash-table (intcode:program-memory program)) in a-to-b)
                     (intcode:make-program (copy-hash-table (intcode:program-memory program)) a-to-b b-to-c)
                     (intcode:make-program (copy-hash-table (intcode:program-memory program)) b-to-c c-to-d)
                     (intcode:make-program (copy-hash-table (intcode:program-memory program)) c-to-d d-to-e)
                     (intcode:make-program (copy-hash-table (intcode:program-memory program)) d-to-e out))))
    (dotimes (i 5)
      (enqueue (nth i phases) (intcode:program-in (nth i amps))))
    (make-acs% :amps amps :in in :out out)))

(defun acs-run (acs input)
  (loop
    :initially (enqueue input (acs-in acs))
    :while (acs-amps acs)
    :for amp = (pop (acs-amps acs))
    :for still-running = (intcode:program-run amp)
    :when still-running :do (nconc (acs-amps acs) (list amp))
    :finally (return (dequeue (acs-out acs)))))

(define-solution (2019 7) (program intcode:read-program)
  (values
    (reduce
      #'max (all-permutations (list 0 1 2 3 4))
      :key (partial-1 #'acs-run (make-acs program _) 0))
    (reduce
      #'max (all-permutations (list 5 6 7 8 9))
      :key (partial-1 #'acs-run (make-acs program _ :feedback-loop-p t) 0))))

(define-test (2019 7) (45730 5406484))
