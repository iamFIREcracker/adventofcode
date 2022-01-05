(defpackage :aoc/2021/23 #.cl-user::*aoc-use*)
(in-package :aoc/2021/23)


(defun parse-input (data &aux
                         (amphipods (make-hash-table :test 'equal))
                         rooms)
  (loop for row below (length data) for string in data do
        (loop for col below (length string) for ch across string unless (char= ch #\#) do
              (when (and (> row 1) (member col '(3 5 7 9)))
                (let* ((type (case col (3 #\A) (5 #\B) (7 #\C) (9 #\D)))
                       (existing (assoc type rooms)))
                  (if existing
                    (push (list row col) (second existing))
                    (push (list type (list (list row col))) rooms))))
              (when (find ch "ABCD") (setf (gethash (list row col) amphipods) ch))))
  (cons amphipods rooms))

(defparameter *hallway* '((1 1) (1 2) (1 4) (1 6) (1 8) (1 10) (1 11)))
(defun siderooms (rooms type) (second (assoc type rooms)))


(defun organize (input &aux (amphipods (car input)) (rooms (cdr input)))
  (search-cost
    (a* amphipods :test 'equalp :goalp (partial-1 #'donep rooms)
        :state-key (partial-1 #'amphipods->list rooms)
        :neighbors (partial-1 #'next rooms)
        :heuristic (constantly 0))))


(defun amphipods->list (rooms amphipods)
  (uiop:while-collecting (add)
    (dolist (h *hallway*) (add (gethash h amphipods)))
    (loop for (type siderooms) in rooms do
          (loop for r in siderooms do (add (gethash r amphipods))))))


(defun donep (rooms amphipods)
  (loop for (type siderooms) in rooms always
        (loop for r in siderooms
              always (eql (gethash r amphipods) type))))


(defun next (rooms amphipods)
  (uiop:while-collecting (next!)
    (loop for type being the hash-values of amphipods using (hash-key pos)
          for siderooms = (second (assoc type rooms)) do
          (cond ((in-place-p siderooms amphipods type pos) nil)
                (t (loop for target in (possible-moves siderooms amphipods type pos)
                         for cost = (* (manhattan-distance pos target) (cost type)) do
                         (next! (cons (move-to amphipods pos target) cost))))))))


(defun in-place-p (siderooms amphipods type pos)
  (loop for r in siderooms
        thereis (equal r pos)
        ;; if not in the room, at least let's make sure the one below is
        ;; occupied by an amphipod of the right type
        always (eql (gethash r amphipods) type)))


(defun possible-moves (siderooms amphipods type pos &aux (row (car pos)))
  (if (/= row 1)
    (remove-if (partial-1 #'blockedp amphipods pos) *hallway*)
    (uiop:while-collecting (target)
      (loop for r in siderooms
            unless (blockedp amphipods pos r) do (target r)
            ;; bottom room first, then the ones above only if the one below is occupied by the right amphipod
            always (eql (gethash r amphipods) type)))))


(defun blockedp (amphipods pos target)
  (destructuring-bind (row1 col1) pos
    (destructuring-bind (row2 col2) target
      (let ((row-step (<=> row2 row1))
            (col-step (<=> col2 col1)))
        (flet ((check-all (row1 col1 row2 col2 row-step col-step)
                 (loop do (incf row1 row-step) (incf col1 col-step)
                       thereis (gethash (list row1 col1) amphipods)
                       until (and (= row1 row2) (= col1 col2)))))
          (if (= row1 1)
            (or (check-all row1 col1 row1 col2 0 col-step)
                (check-all row1 col2 row2 col2 row-step 0))
            (or (check-all row1 col1 row2 col1 row-step 0)
                (check-all row2 col1 row2 col2 0 col-step))))))))


(defun cost (type) (ecase type (#\A 1) (#\B 10) (#\C 100) (#\D 1000)))

(defun move-to (amphipods pos target &aux (rez (copy-hash-table amphipods)))
  (setf (gethash target rez) (gethash pos rez))
  (remhash pos rez)
  rez)


(defun massage (lines)
  (append
    (subseq lines 0 3)
    (list
      "  #D#C#B#A#"
      "  #D#B#A#C#")
    (subseq lines 3)))


(define-solution (2021 23) (lines)
  (values (organize (parse-input lines)) (organize (parse-input (massage lines)))))

(define-test (2021 23) (18282 50132))

;;; https://topaz.github.io/paste/#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
;;; - I was calling CALCULATE-ALL-COSTS on each invocation of HEURISTIC -- and
;;;   this is because PARTIAL-1 does **not** cache any non-free binding!!!!
;;;
;;; - Changing from EQUAL to EQL (when appropriate) does not seem to make any
;;;   difference...great!
;;;
;;; - Changing two nested DESTRUCTURING-BIND with one, whose argument are the
;;;   two original destructured varibales, CONS'd together, does not seem to
;;;   have any impact on the runtime; it would cons more of course, but the
;;;   runtime does not seem to be affected
;;;
;;; - Stopping after 2 moves does not make any difference at all, and I take
;;;   it's because of how the algorithm works, i.e. if an amphipod is in
;;;   the hallway it will try to move it in a room, otherwise it will try to
;;;   move it in the hallway (unless already in place).  This means we don't
;;;   have to keep track of the number of remaining moves anymore.
;;;
;;;   This...brought the run time of the example configuration from 316.632s to
;;;   264.389s.
;;;
;;; - Using a HASH-TABLE as our search state causes A* to waste a lot of time
;;;   while updating `cost-so-far` and `come-from` maps -- that has something
;;;   to do with the fact that it calls EQUALP a lot t.  The solution? add
;;;   :STATE-KEY to A*'s so that the specified function can be used to _reduce_
;;;   the state to a more convenient and easier to process representation
;;;
;;;   On the example configuration, this alone gave a 100x speed up taking the
;;;   run time down to 0.989s!
;;;
;;;   It would run for 7.699s with my actual input to find the answer for part
;;;   1; for part 2 instead, it would run for 10.166s!
;;;
;;;   Possible enhancements:
;;;
;;;   - Enable heuristic
;;;   - Change state representation
; (profile (assert (equal (organize (parse-input (uiop:read-file-lines "src/2021/day23-ex-0008.txt"))) 8)))
; (profile (assert (equal (organize (parse-input (uiop:read-file-lines "src/2021/day23-ex-12481.txt"))) 12481)))
; (profile (assert (equal (organize (parse-input (uiop:read-file-lines "src/2021/day23-ex.txt"))) 12521)))
; (profile (assert (equal (organize (parse-input (uiop:read-file-lines "src/2021/day23.txt"))) 18282)))
; (profile (assert (equal (organize (parse-input (massage (uiop:read-file-lines "src/2021/day23.txt")))) 50132)))
