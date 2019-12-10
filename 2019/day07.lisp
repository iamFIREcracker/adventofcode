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
         (amps (list (intcode:make-program (copy-seq (intcode:program-memory program)) in a-to-b)
                     (intcode:make-program (copy-seq (intcode:program-memory program)) a-to-b b-to-c)
                     (intcode:make-program (copy-seq (intcode:program-memory program)) b-to-c c-to-d)
                     (intcode:make-program (copy-seq (intcode:program-memory program)) c-to-d d-to-e)
                     (intcode:make-program (copy-seq (intcode:program-memory program)) d-to-e out))))
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

(define-problem (2019 7) (program intcode:read-program)
  (values
    (loop
      :for phases :in (all-permutations (list 0 1 2 3 4))
      :for acs = (make-acs program phases)
      :maximizing (acs-run acs 0))
    (loop
      :for phases :in (all-permutations (list 5 6 7 8 9))
      :for acs = (make-acs program phases :feedback-loop-p T)
      :maximizing (acs-run acs 0))))

(1am:test test-2019/07
  (multiple-value-bind (part1 part2) (problem-run)
    (1am:is (= 45730 part1))
    (1am:is (= 5406484 part2))))
