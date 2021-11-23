(defpackage :aoc/2018/07 #.cl-user::*aoc-use*)
(in-package :aoc/2018/07)


(defun parse-instructions (data)
  (let (rez)
    (loop :for string :in data
          :for (step . dep) = (parse-instruction string)
          :for step-entry = (assoc step rez)
          :for dep-entry = (assoc dep rez)
          :if step-entry :do (push dep (cdr step-entry))
          :else :do (push (cons step (list dep)) rez)
          :unless dep-entry :do (push (cons dep nil) rez))
    rez))

(defun parse-instruction (string)
  (cl-ppcre:register-groups-bind ((#'parse-char dep step))
      ("Step (\\w) must be finished before step (\\w) can begin." string)
    (cons step dep)))


(defun part1 (instructions &aux completed)
  (loop
    (if (null instructions)
      (return (format nil "~{~C~}" (reverse completed)))
      (destructuring-bind ((step) . rest) (sort-instructions instructions)
        (setf completed (cons step completed)
              instructions (remove-dependency rest step))))))

(defun sort-instructions (instructions)
  (sort (copy-seq instructions) #'instruction<))

(defun instruction< (i1 i2)
  (destructuring-bind (step1 . deps1) i1
    (destructuring-bind (step2 . deps2) i2
      (or (< (length deps1) (length deps2))
          (and (= (length deps1) (length deps2))
               (char< step1 step2))))))

(defun remove-dependency (instructions dependency)
  (loop :for (step . deps) :in instructions
        :collect (cons step (remove dependency deps))))


(defstruct (workers (:conc-name nil)
                    (:constructor make-workers%))
  busy-for
  busy-with)

(defun make-workers (n)
  (make-workers% :busy-for (make-array n :initial-element 0)
                 :busy-with (make-array n :initial-element nil)))


(defparameter *workers-count* 5)

(defun part2 (instructions)
  (let ((workers (make-workers *workers-count*))
        (time 0))
    (loop
      (setf instructions (workers-tick workers instructions))
      (if (every #'null (busy-with workers))
        (return time)
        (incf time)))))

(defun workers-tick (workers instructions)
  (with-slots (busy-for busy-with) workers
    (flet ((try-release-workers ()
             (loop :for i :from 0
                   :for left :across busy-for
                   :for step :across busy-with
                   :if (> left 1) :do (decf (aref busy-for i))
                   :else :collect i :and :do
                   (setf instructions (remove-dependency instructions step)
                         (aref busy-for i) 0
                         (aref busy-with i) nil)))
           (try-assign-work (available)
             (loop :while (and available instructions) :do
                   (let ((i (first available)))
                     (setf instructions (sort-instructions instructions)
                           (aref busy-for i) 0
                           (aref busy-with i) nil)
                     (destructuring-bind ((step . deps) . rest) instructions
                       (if (> (length deps) 0)
                         (return)
                         (setf (aref busy-for i) (step-time step)
                               (aref busy-with i) step
                               available (rest available)
                               instructions rest)))))))
      (let ((available (try-release-workers)))
        (try-assign-work available)
        instructions))))

(defparameter *step-duration* 60)

(defun step-time (step)
  (+ *step-duration* (1+ (- (char-code step) (char-code #\A)))))


(define-solution (2018 7) (instructions parse-instructions)
  (values (part1 instructions) (part2 instructions)))

(define-test (2018 7) ("GRTAHKLQVYWXMUBCZPIJFEDNSO" 1115))
