(defpackage :aoc/2019/17 #.cl-user::*aoc-use*)
(in-package :aoc/2019/17)

(defstruct (robot (:constructor make-robot%))
  program
  in
  out)

(defun make-robot (program)
  (let* ((program (intcode:make-program (copy-hash-table (intcode:program-memory program)))))
    (make-robot% :program program
                 :in (intcode:program-in program)
                 :out (intcode:program-out program))))

(defun print-map (h)
  (print-hash-table-map h (lambda (value &optional)
                            (if (null value) #\Space value))))

(defun explore (robot)
  (loop
    :with map = (make-hash-table)
    :with program = (robot-program robot)
    :with i = 0
    :for j = 0 :then (1+ j)
    :for pos = (complex j (- i))
    :for running = (intcode:program-run program)
    :for code = (dequeue (robot-out robot))
    :for char = (code-char code)
    :when (eql char #\NewLine) :do (setf i (1+ i)
                                         j -1)
    :when (not (eql char #\Newline)) :do (hash-table-insert map pos char)
    :until (queue-empty-p (robot-out robot))
    :finally (return map)))

(defun explore-1 (robot)
  (loop
    :with program = (robot-program robot)
    :with last
    :until (queue-empty-p (robot-out robot))
    :do (progn
          (intcode:program-run program)
          (setf last (dequeue (robot-out robot))))
    :finally (return last)))

(defun neighbors (pos)
  (loop
    :for delta :in (list #C(0 1) #C(1 0) #C(0 -1) #C(-1 0))
    :collecting (+ pos delta)))

(defun scaffoldp (pos map &aux (char (gethash pos map)))
  (member char (list #\# #\^ #\> #\v #\<)))

(defun intersectionp (pos map)
  (loop
    :for neighbor :in (neighbors pos)
    :always (scaffoldp neighbor map)))

(defun input-main-movement-routine-1()
  (list #\A #\, #\A #\, #\B #\, #\B #\, #\C #\, #\B #\, #\C #\, #\B #\, #\C #\, #\A #\Newline))

(defun input-movement-function-1()
  (list #\L #\, #\1 #\0 #\, #\L #\, #\1 #\0 #\, #\R #\, #\6 #\Newline))

(defun input-movement-function-2()
  (list #\R #\, #\1 #\2 #\, #\L #\, #\1 #\2 #\, #\L #\, #\1 #\2 #\Newline))

(defun input-movement-function-3()
  (list #\L #\, #\6 #\, #\L #\, #\1 #\0 #\, #\R #\, #\1 #\2 #\, #\R #\, #\1 #\2 #\Newline))

(defun input-video-feed ()
  (list #\n #\Newline))

(defun robot-clean (robot)
  (let* ((program (robot-program robot))
         (memory (intcode:program-memory program)))
    (hash-table-insert memory 0 2)
    (dolist (char (input-main-movement-routine-1))
      (enqueue (char-code char) (robot-in robot)))
    (dolist (char (input-movement-function-1))
      (enqueue (char-code char) (robot-in robot)))
    (dolist (char (input-movement-function-2))
      (enqueue (char-code char) (robot-in robot)))
    (dolist (char (input-movement-function-3))
      (enqueue (char-code char) (robot-in robot)))
    (dolist (char (input-video-feed))
      (enqueue (char-code char) (robot-in robot)))
    (intcode:program-run (robot-program robot))
    (explore-1 robot)))

(define-solution (2019 17) (program intcode:read-program)
  (values
    (let* ((robot (make-robot program))
           (map (explore robot)))
      (summation
        (gathering
          (dolist (k (hash-table-keys map))
            (when (and (scaffoldp k map) (intersectionp k map))
              (gather (* (realpart k) (- (imagpart k)))))))))
    (let* ((robot (make-robot program)))
      (robot-clean robot))))

(define-test (2019 17) (11140 1113108))
