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

(defstruct state robots collected)

(defun keyp (c) (lower-case-p c))
(defun doorp (c) (upper-case-p c))
(defun wallp (c) (char= c #\#))

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
  (multiple-value-bind (end-state end-state-cost end-state-path)
      (a* init-pos
          :goal-state target-pos
          :neighbors (search-unit-cost (partial-1 #'v-neighbors v))
          :heuristic (partial-1 #'manhattan-distance _ target-pos))
    (declare (ignore end-state))
    (let* ((doors (doors-along-the-way v end-state-path)))
      (cons end-state-cost doors))))

(defun already-collected (keys i)
  (plusp (logand keys (ash 1 i))))

(defun doors-unlocked-p (doors keys)
  (= doors (logand doors keys)))

(defun change (orig i value)
  (let ((copy (copy-seq orig)))
    (setf (aref copy i) value)
    copy))

(defun v-reachable-keys (v state)
  (with-slots (robots collected) state
    (loop :for i :below (length robots)
          :for pos :across robots
          :append (loop :for key :in (v-keys-sorted v)
                        :for key-i :from 0
                        :for key-pos = (v-key-pos v key)
                        :for (steps . doors) = (reach-key v pos key-pos)
                        :when (and steps (doors-unlocked-p doors collected)
                                   (not (already-collected collected key-i)))
                        :collect (cons
                                   (make-state
                                     :robots (change robots i key-pos)
                                     :collected (logior collected (ash 1 key-i)))
                                   steps)))))

(defun solve (data &aux (v (make-vault data)))
  (let* ((num-keys (length (v-keys-sorted v)))
         (all-keys (1- (ash 1 num-keys)))
         (robots (coerce (v-start v) 'vector)))
    (reach-key/clear-memo)
    (multiple-value-bind (end-state end-state-cost)
        (a* (make-state :robots robots :collected 0)
            :goalp (partial-1 #'= all-keys (state-collected _))
            :neighbors (partial-1 #'v-reachable-keys v)
            :test 'equalp)
      (declare (ignore end-state))
      end-state-cost)))

(defun prepare-part2 (data)
  (let* ((copy (copy-seq data))
         (vault-x (floor (length (first data)) 2))
         (vault-y (floor (length data) 2)))
    (setf (aref (nth (1- vault-y) copy) (1- vault-x)) #\@
          (aref (nth (1- vault-y) copy) vault-x) #\#
          (aref (nth (1- vault-y) copy) (1+ vault-x)) #\@
          (aref (nth vault-y copy) (1- vault-x)) #\#
          (aref (nth vault-y copy) vault-x) #\#
          (aref (nth vault-y copy) (1+ vault-x)) #\#
          (aref (nth (1+ vault-y) copy) (1- vault-x)) #\@
          (aref (nth (1+ vault-y) copy) vault-x) #\#
          (aref (nth (1+ vault-y) copy) (1+ vault-x)) #\@)
    copy))

(define-problem (2019 18) (data)
  (values
    (solve data)
    (solve (prepare-part2 data))))

(1am:test test-2019/18
  (multiple-value-bind (part1 part2) (problem-run)
    (1am:is (= 5068 part1))
    (1am:is (= 1966 part2))))
