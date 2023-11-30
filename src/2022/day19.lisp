(defpackage :aoc/2022/19 #.cl-user::*aoc-use*)
(in-package :aoc/2022/19)


(define-modify-macro maxf (&rest others) max)


(defstruct (blueprint :conc-name)
  id costs)

(defun parse-blueprint (string)
  (destructuring-bind (id ore-robot-cost clay-robot-cost
                          obsidian-robot-ore-cost obsidian-robot-clay-cost
                          geode-robot-ore-cost geode-robot-obsidian-cost)
      (mapcar #'parse-integer (cl-ppcre:all-matches-as-strings "\\d+" string))
    (make-blueprint :id id
                    :costs (list
                             (list
                               (list ore-robot-cost 0 0 0)
                               (list 1 0 0 0))
                             (list
                               (list clay-robot-cost 0 0 0)
                               (list 0 1 0 0))
                             (list
                               (list obsidian-robot-ore-cost obsidian-robot-clay-cost 0 0)
                               (list 0 0 1 0))
                             (list
                               (list geode-robot-ore-cost 0 geode-robot-obsidian-cost 0)
                               (list 0 0 0 1)))
                    )))

(defun blueprints (&optional (file #P"src/2022/day19.txt"))
  (mapcar #'parse-blueprint (uiop:read-file-lines file)))


(defstruct (state :conc-name)
  resources robots)
(defun cracked (s) (fourth (resources s)))
(defun geode-robots (s) (fourth (robots s)))

(defun init-state ()
  (make-state :resources (list 0 0 0 0)
              :robots (list 1 0 0 0)))

(defun collect-resources (resources robots)
  (mapcar #'+ resources robots))

(defun collect (state)
  (make-state :resources (collect-resources (resources state)
                                            (robots state))
              :robots (robots state)))
#+#:excluded (collect (init-state))
#+#:excluded (collect (make-state :resources (list 0 0 0 0)
                                  :robots (list 1 1 1 1)))

(defun can-build-p (resources cost-input)
  (loop for r in resources for c in cost-input always (>= r c)))

(defun build-robot (resources robots cost)
  (list (mapcar #'- resources (car cost))
        (mapcar #'+ robots (cadr cost))))

(defun build-robots (resources robots costs)
  (loop for cost in costs for (cost-input robots-output) = cost
          when (can-build-p resources cost-input)
          collect (build-robot resources robots cost)))

(defun build (blueprint state)
  (loop for (res-next rbts-next) in (build-robots (resources state)
                                                  (robots state)
                                                  (costs blueprint))
        collect (make-state :resources (collect-resources res-next (robots state))
                            :robots rbts-next)))
#+#:excluded (build (car (blueprints #P"scratch.txt")) (init-state))
#+#:excluded (build (car (blueprints #P"scratch.txt")) (make-state :resources (list 4 0 0 0)
                                                                   :robots (list 1 0 0 0)))

(defun next (blueprint state)
  (cons (collect state)
        (build blueprint state)))
#+#:excluded (next (car (blueprints #P"scratch.txt")) (init-state))
#+#:excluded (next (car (blueprints #P"scratch.txt")) (make-state :resources (list 4 0 0 0)
                                                                   :robots (list 1 0 0 0)))

;; HASH-TABLE SIZE heuristic
(defun maximize (limit blueprint)
  (let ((seen (make-hash-table :test 'equalp))
        (best 0))
    (labels ((recur (s time &aux (seen-at (gethash s seen)))
               (cond ((and (= time limit)) (if (> (cracked s) best)
                                             (pr (maxf best (cracked s))
                                                 s)))
                     ((and seen-at (<= seen-at time)) nil)
                     ((= (hash-table-count seen) 200000000) (return-from maximize (pr best
                                                                                      s
                                                                                      time)))
                     (t (setf (gethash s seen) time)
                        (dolist (sn (reverse (next blueprint s)))
                          (recur sn (1+ time)))))))
      (recur (init-state) 0)
      (pr (hash-table-count seen))
      best)))

;; FCALL COUNT heuristic
(defun maximize (limit blueprint)
  (let ((best 0)
        (fcalls 0))
    (labels ((recur (s time)
               (cond ((and (= time limit)) (if (> (cracked s) best)
                                             (pr (maxf best (cracked s))
                                                 s)))
                     ((= fcalls 500000000) (return-from maximize (pr best
                                                                     s
                                                                     time)))
                     (t (incf fcalls)
                        (dolist (sn (reverse (next blueprint s)))
                          (recur sn (1+ time)))))))
      (recur (init-state) 0)
      best)))

;; HOPELESS states
(defun maximize (limit blueprint)
  (let ((best 0))
    (labels ((hopelessp (s time)
               (let* ((remaining (- limit time))
                      (max-robots (1- remaining))
                      (collectable (+ (* (geode-robots s) remaining)
                                      (* (/ max-robots 2) (+ 1 max-robots))
                                      )))
                 (< (+ (cracked s) collectable) best)))
             (recur (s time)
               (cond ((and (= time limit)) (if (> (cracked s) best)
                                             (pr (maxf best (cracked s))
                                                 s)))
                     ((hopelessp s time) nil)
                     (t (dolist (sn (reverse (next blueprint s)))
                          (recur sn (1+ time)))))))
      (recur (init-state) 0)
      best)))

;; HOPELESS states + MEMO
(defun maximize (limit blueprint)
  (let ((seen (make-hash-table :test 'equalp))
        (best 0))
    (labels ((hopelessp (s time)
               (let* ((remaining (- limit time))
                      (max-robots (1- remaining))
                      (collectable (+ (* (geode-robots s) remaining)
                                      (* (/ max-robots 2) (+ 1 max-robots)))))
                 (< (+ (cracked s) collectable) best)))
             (recur (s time &aux (seen-at (gethash s seen)))
               (cond ((and (= time limit)) (if (> (cracked s) best)
                                             (pr (maxf best (cracked s))
                                                 s)))
                     ((and seen-at (<= seen-at time)) nil)
                     ((= (hash-table-count seen) 200000000) (return-from maximize (pr best
                                                                                      s
                                                                                      time)))
                     ((hopelessp s time) nil)
                     (t (setf (gethash s seen) time)
                        (dolist (sn (reverse (next blueprint s)))
                          (recur sn (1+ time)))))))
      (recur (init-state) 0)
      (pr best (hash-table-count seen))
      best)))
#+#:excluded (maximize 24 (car (blueprints #P"scratch.txt")))
#+#:excluded (maximize 24 (cadr (blueprints #P"scratch.txt")))

#+#:excluded (maximize 32 (car (blueprints)))
#+#:excluded (maximize 32 (cadr (blueprints)))
#+#:excluded (maximize 32 (caddr (blueprints)))
#+#:excluded (reduce #'* (blueprints) :key [maximize 32 _] :end 3)

(defun maximize-bfs (limit blueprint)
  (bnd1 (queue (list (init-state)))
    (loop repeat limit
          do (pr (length queue))
          do (setf queue (remove-duplicates
                           (loop for s in queue
                                 nconc (next blueprint s))
                           :test 'equalp)))
    (reduce #'max queue :key [fourth (resources _)])))
#+#:excluded (maximize-bfs 24 (car (blueprints #P"scratch.txt")))
#+#:excluded (maximize 24 (cadr (blueprints #P"scratch.txt")))

(defun quality-level (blueprint cracked) (* (id blueprint) cracked))

#; Scratch

(loop for b in (blueprints #+#:excluded #P"scratch.txt")
      for cracked = (pr (simulate 24 b) b)
      sum (quality-level b cracked))
(loop for b in (subseq (blueprints #+#:excluded #P"scratch.txt") 0 3)
      for cracked = (pr (simulate 32 b) b)
      sum (quality-level b cracked))

(blueprints #P"scratch.txt")

(init-state)

(simulate 24 (car (blueprints)))
(simulate 24 (cadr (blueprints)))
(simulate 24 (caddr (blueprints)))
(simulate 24 (cadddr (blueprints)))
(simulate 24 (caddddr (blueprints)))
(loop for b in (blueprints) do (pr (id b) (simulate 24 b)))

(simulate 18 (car (blueprints #P"scratch.txt")))
(simulate 19 (car (blueprints #P"scratch.txt")))
(simulate 20 (car (blueprints #P"scratch.txt")))
(simulate 21 (car (blueprints #P"scratch.txt")))
(simulate 22 (car (blueprints #P"scratch.txt"))) ; 5
(simulate 23 (car (blueprints #P"scratch.txt")))
(simulate 24 (car (blueprints #P"scratch.txt")))
(simulate 24 (cadr (blueprints #P"scratch.txt")))

(loop for b in (blueprints) do (pr (simulate b 24)))
(sb-ext:gc :full t)
#+#:excluded (&optional (file #P"src/2022/day19.txt"))
#P"scratch.txt"

;;; thought I could build lots of robots in one turn...

(* 28 4 16) ; nope
(* 32 4 16) ; nope
2240 nope
1984 too low
2480 nope
