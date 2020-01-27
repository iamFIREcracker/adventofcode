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

(defstruct (state (:constructor make-state%)
                  (:conc-name s-))
  pos
  keys)

(defun make-state (pos keys)
  (make-state% :pos pos :keys keys))

(defun emptyp (c)
  (or (char= c #\.)
      (char= c #\@)))

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

(defun doors-along-the-way (v path)
  (remove-if-not #'doorp (mapcar (partial-1 #'v-cell v _) path)))

(defun/memo reach-key (v init-pos target-pos)
  (multiple-value-bind (end-state cost-so-far come-from)
      (a* init-pos
          :goal-state target-pos
          :neighbors (search-unit-cost (partial-1 #'v-neighbors v))
          :heuristic (partial-1 #'manhattan-distance _ target-pos))
    (let* ((path (search-backtrack come-from end-state))
           (required-keys (mapcar #'char-downcase (doors-along-the-way v path))))
      (list
        (gethash end-state cost-so-far)
        (nsorted required-keys)))))

(defun sorted-set-difference (list1 list2)
  (let ((e1 (first list1))
        (e2 (first list2)))
    (cond ((not list2) list1)
          ((not list1) NIL)
          ((char> e1 e2) (sorted-set-difference list1 (rest list2)))
          ((char= e1 e2) (sorted-set-difference (rest list1) (rest list2)))
          ((char< e1 e2) (cons e1 (sorted-set-difference (rest list1) list2))))))

(defun v-key-reachable-p-memo (v init-state target-pos &optional all-keys)
  (destructuring-bind (steps required-keys) (reach-key v (s-pos init-state) target-pos)
    (unless (sorted-set-difference required-keys (or all-keys (s-keys init-state)))
      steps)))

(defun v-reachable-keys (v state &optional all-keys)
  (loop
    :with keys = (s-keys state)
    :for key :in (sorted-set-difference (v-keys-sorted v) (or all-keys keys))
    :for key-pos = (v-key-pos v key)
    :for steps = (v-key-reachable-p-memo v state key-pos all-keys)
    :when steps :collect (cons
                           (make-state key-pos (merge 'list (copy-seq keys) (list key) #'char<))
                           steps)))

(defun v-reachable-keys-part2 (v states)
  (loop
    :with all-keys = (nsorted (flatten (mapcar #'s-keys states)))
    :for i :from 0 :below (length states)
    :for state :in states
    :append (loop
              :for (next-state . steps) :in (v-reachable-keys v state all-keys)
              :for pre = (subseq states 0 i)
              :for post = (subseq states (1+ i))
              :collect (cons
                         (concatenate 'list pre (list next-state) post)
                         steps))))

(define-problem (2019 18) (v make-vault)
  (let ((num-keys (length (v-keys-sorted v))))
    (values
      (progn
        (reach-key/clear-memo)
        (multiple-value-bind (end-state cost-so-far)
            (a* (make-state (first (v-start v)) NIL)
                :goalp (partial-1 #'= num-keys (length (s-keys _)))
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
