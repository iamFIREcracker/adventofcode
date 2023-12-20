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

(defun parse-input (&optional (strings (uiop:read-file-lines #P"src/2023/day20.txt")))
  (bnd* ((modules (cons (button) (mapcar #'parse-module strings))))
    (dolist+ (m modules)
      (dolist (n (getf m :outputs))
        (bnd* ((o (find n modules :key #'name)))
          (setf (getf (getf o :inputs) (name m)) :low))))
    modules))
#+#:excluded (parse-input)

(defun low? (i) (eq i :low))
(defun high? (i) (eq i :high))
(defun flip (i) (if (low? i) :high :low))

(defun plist-keys (plist) (loop for k in plist by #'cddr collect k))
(defun plist-values (plist) (loop for v in (cdr plist) by #'cddr collect v))

(defun change-input (m from pulse)
  (setf (getf (getf m :inputs) from) pulse)
  (bnd1 (new (%new-state m  pulse))
    (if new (setf (getf m :state) new))
    new))

(defun %new-state (m last-pulse)
  (case (getf m :type)
    (% (if (low? last-pulse) (flip (getf m :state))))
    (& (if (every #'high? (plist-values (getf m :inputs))) :low :high))
    (otherwise last-pulse)))

; (untrace %new-state)

(defun warm-up (&optional (modules (parse-input)))
  (bnd1 (pulses (list :low 0 :high 0))
    (labels ((push-button ()
               (bnd1 (q (list (list :low :elves :button)))
                 (while q
                   (setf q
                         (looping
                           (dolist+ ((pulse from to) q)
                             (bnd* ((m (find to modules :key #'name)))
                               (awhen (change-input m from pulse)
                                 (dolist (n (getf m :outputs))
                                   (incf (getf pulses it))
                                   ; (pr (name m) it n)
                                   (collect! (list it (name m) n))))))))))))

      (repeat 1000
        (push-button)))
    pulses))
(reduce #'* (plist-values (warm-up)))
; 95113840 nope
; 86755932 nope
; 883726240


(defun push-until (expected-pulse expected-from expected-to
                                  &optional (modules (parse-input)))
  (bnd* ((pushes 0))
    (labels ((push-button ()
               (bnd1 (q (list (list :low :elves :button)))
                 (while q
                   (setf q
                         (looping
                           (dolist+ ((pulse from to) q)
                             (when (and (eq expected-pulse pulse)
                                        (eq expected-from from)
                                        (eq expected-to to))
                               (return-from push-until pushes))
                             (bnd* ((m (find to modules :key #'name)))
                               (awhen (change-input m from pulse)
                                 (dolist (n (getf m :outputs))
                                   (collect! (list it (name m) n))))))))))))

      (while t
        (incf pushes)
        (push-button)))))
#+#:excluded (push-until :low :fr)
#+#:excluded (push-until :low :ms)
#+#:excluded (push-until :low :rx)


(lcm
  (push-until :high :xc :bb)
  (push-until :high :ks :bb)
  (push-until :high :kp :bb)
  (push-until :high :ct :bb))
