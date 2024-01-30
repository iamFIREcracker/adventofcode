(defpackage :aoc/2023/20 #.cl-user::*aoc-use*)
(in-package :aoc/2023/20)


(defun button () (list :name :button :outputs '(:broadcaster)))

(defun parse-module (s)
  (destructuring-bind (name destinations) (cl-ppcre:split " -> " s)
    (bnd* ((type nil))
      (when (find (char name 0) "%&")
        (setf type (symb (char name 0))
              name (subseq- name 1)))
      (bnd1 (outputs (cl-ppcre:split ", " destinations))
        (list :name (as-keyword name)
              :type type
              :inputs nil
              :state :low
              :outputs (mapcar #'as-keyword outputs))))))
(defun name (m) (getf m :name))

(defun parse-input (&optional (strings (aoc::read-problem-input 2023 20)))
  (bnd* ((modules (cons (button) (mapcar #'parse-module strings))))
    (doseq (m modules)
      (dolist (n (getf m :outputs))
        (bnd* ((o (find n modules :key #'name)))
          (setf (getf (getf o :inputs) (name m)) :low))))
    modules))

(defun low? (i) (eq i :low))
(defun high? (i) (eq i :high))
(defun flip (i) (if (low? i) :high :low))


(defun change-input (m from pulse-type)
  (setf (getf (getf m :inputs) from) pulse-type)
  (bnd1 (new (%new-state m pulse-type))
    (if new (setf (getf m :state) new))
    new))

(defun %new-state (m last-pulse-type)
  (case (getf m :type)
    (% (if (low? last-pulse-type) (flip (getf m :state))))
    (& (if (every #'high? (plist-values (getf m :inputs))) :low :high))
    (otherwise last-pulse-type)))

(define-condition pulse ()
  ((type :initarg :type)
   (from :initarg :from)
   (to :initarg :to)))

(defun push-button (&optional (modules (parse-input)))
  (bnd* ((pulses (list :low 0 :high 0))
         (q (make-queue)))
    (enqueue '(:low :elves :button) q)
    (while-not (queue-empty-p q)
      (destructuring-bind (pulse-type from to) (dequeue q)
        (signal 'pulse :type pulse-type :from from :to to)
        (bnd1 (m (find to modules :key #'name))
          (awhen (change-input m from pulse-type)
            (dolist (n (getf m :outputs))
              (incf (getf pulses it))
              (enqueue (list it (name m) n) q))))))
    (plist-values pulses)))


(defun warm-up (&optional (modules (parse-input)))
  (bnd1 (pulses (list :low 0 :high 0))
    (repeat 1000
      (destructuring-bind (low high) (push-button modules)
        (incf (getf pulses :low) low)
        (incf (getf pulses :high) high)))
    (reduce #'* (plist-values pulses))))


;; By inspecting my input I realized that:
;;
;; - Only one module outputs into :RX (:BB in my case, a conjuction module)
;; - There are 4 modules who output into :BB (:CT, KP, KS, and :XC, all
;;   conjuctions)
;;
;; :BB is a conjuction module, and will output a low pulse when all its inputs
;; are :high.
;;
;; So what I did was monitoring the network and keep track of the number of
;; times I need to push the button to get any of the :BB's input to generate
;; a high pulse.  Assume some periodicity, et voila`!
(defun turn-on-engine (&optional (strings (aoc::read-problem-input 2023 20)))
  (apply #'lcm
         (looping
           (bnd1 (modules (parse-input strings))
             (destructuring-bind (to) (find-that-outputs-into :rx modules)
               (dolist (from (find-that-outputs-into (name to) modules))
                 (collect! (push-until :high
                                       (name from)
                                       (name to)
                                       (parse-input strings)))))))))

(defun find-that-outputs-into (name &optional (modules (parse-input)))
  (keep-if [member name (getf _ :outputs)] modules))


(defun push-until (expected-pulse-type expected-from expected-to
                                       &optional (modules (parse-input)))
  (bnd1 (pushes 0)
    (handler-bind ((pulse
                     (lambda (c)
                       (with-slots (type from to) c
                         (when (and (eq expected-pulse-type type)
                                    (eq expected-from from)
                                    (eq expected-to to))
                           (return-from push-until pushes))))))
      (while t
        (incf pushes)
        (push-button modules)))))


(define-solution (2023 20) (strings)
  (values (warm-up (parse-input strings))
          (turn-on-engine strings)))

(define-test (2023 20) (883726240 211712400442661))
