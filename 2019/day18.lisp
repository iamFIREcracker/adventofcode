(defpackage :aoc/2019/18 #.cl-user::*aoc-use*)
(in-package :aoc/2019/18)

(defun read-map (data &aux (map (make-hash-table)))
  (prog1 map
    (loop
      :for str :in data
      :for i = 0 :then (1- i)
      :do (loop
            :for c :across str
            :for j = 0 :then (1+ j)
            :for pos = (complex j i)
            :do (hash-table-insert map pos c)))))

(defstruct (state (:conc-name s-))
  pos
  keys)

(defun keyp (c)
  (lower-case-p c))

(defun doorp (c)
  (upper-case-p c))

(defun wallp (c)
  (char= c #\#))

(defstruct (vault (:constructor make-vault%)
                  (:conc-name v-))
  map
  keys
  keys-sorted)

(defun make-vault (data &aux (keys (make-hash-table)))
  (let* ((map (read-map data)))
    (loop
      :for pos :in (hash-table-keys map)
      :for c = (gethash pos map)
      :when (keyp c) :do (hash-table-insert keys c pos))
    (make-vault% :map map
                 :keys keys
                 :keys-sorted (nsorted (hash-table-keys keys)))))

(defun v-start (v)
  (loop
    :for pos :in (hash-table-keys (v-map v))
    :for c = (v-cell v pos)
    :when (eql #\@ c) :collect pos))

(defun v-key-pos (v key)
  (gethash key (v-keys v)))

(defun v-cell (v pos)
  (gethash pos (v-map v)))

(defun v-neighbors (v pos)
  (loop
    :for next-pos :in (adjacents pos)
    :for c = (v-cell v next-pos)
    :unless (wallp c) :collect next-pos))

(defun char- (a b)
  (- (char-code a) (char-code b)))

(defun doors-along-the-way (v path)
  (let ((doors 0))
    (dolist (pos path)
      (let ((c (v-cell v pos)))
        (when (doorp c)
          (setf doors (logior doors (ash 1 (char- c #\A)))))))
    doors))

(defun/memo reach-key (v init-pos target-pos)
  (multiple-value-bind (end-state cost-so-far come-from)
      (a* init-pos
          :goal-state target-pos
          :neighbors (search-unit-cost (partial-1 #'v-neighbors v))
          :heuristic (partial-1 #'manhattan-distance _ target-pos))
    (let* ((path (search-backtrack come-from end-state))
           (doors (doors-along-the-way v path)))
      (list
        (gethash end-state cost-so-far)
        doors))))

(defun doors-unlocked-p (doors keys)
  (= doors (logand doors keys)))

(defun v-reachable-keys (v state)
  (with-slots (pos keys) state
    (loop
      :for key :in (v-keys-sorted v)
      :for key-i :from 0
      :for key-pos = (v-key-pos v key)
      :for (steps doors) = (reach-key v pos key-pos)
      :when (and steps (doors-unlocked-p doors keys)) :collect (cons
                                                                 (make-state :pos key-pos
                                                                             :keys (logior keys (ash 1 key-i)))
                                                                 steps))))

(defun change (orig i value)
  (let ((ret (copy-seq orig)))
    (prog1 ret
      (setf (nth i ret) value))))

; (defun v-reachable-keys-part2 (v states)
;   (loop
;     :with all-keys = (nsorted (flatten (mapcar #'s-keys states)))
;     :for i :from 0 :below (length states)
;     :for state :in states
;     :append (loop
;               :for (next-state . steps) :in (v-reachable-keys v state all-keys)
;               :collect (cons
;                          (change states i next-state)
;                          steps))))

(define-problem (2019 18) (v make-vault)
  (let* ((num-keys (length (v-keys-sorted v)))
         (all-keys (1- (ash 1 num-keys))))
    (values
      (progn
        (reach-key/clear-memo)
        (multiple-value-bind (end-state cost-so-far)
            (a* (make-state :pos (first (v-start v)) :keys 0)
                :goalp (partial-1 #'= all-keys (s-keys _))
                :neighbors (partial-1 #'v-reachable-keys v)
                :test 'equalp)
          (gethash end-state cost-so-far))))))
    ; (progn
    ;   (reach-key/clear-memo)
    ;   (multiple-value-bind (cost-so-far come-from end-state)
    ;       (a* (loop
    ;                 :for pos :in (v-start v)
    ;                 :collect (make-state pos NIL))
    ;               0
    ;               (list (make-state 0 (v-keys-sorted v)))
    ;               (partial-1 #'v-reachable-keys-part2 v)
    ;               (constantly 0)
    ;               :key (lambda (s) (summation s :key (lambda (ss) ss (length (s-keys ss))))))
    ;     (gethash end-state cost-so-far)))))

(1am:test test-2019/18
  (multiple-value-bind (part1) (problem-run)
    (1am:is (= 5068 part1))))
    ; (1am:is (= 1966 part2))))
