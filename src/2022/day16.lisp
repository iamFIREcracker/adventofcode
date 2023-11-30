(defpackage :aoc/2022/16 #.cl-user::*aoc-use*)
(in-package :aoc/2022/16)

(defstruct (valve :conc-name)
  name rate connected-to)

(defun parse-valve (s)
  (let ((names (mapcar #'symb (cl-ppcre:all-matches-as-strings "[A-Z]{2}" s)))
        (rate (first (mapcar #'parse-integer (cl-ppcre:all-matches-as-strings "\\d+" s)))))
    (make-valve :name (car names)
                :rate rate
                :connected-to (rest names))))

(defun valves (&optional (file #P"src/2022/day16.txt"))
  (let* ((valves (mapcar #'parse-valve (uiop:read-file-lines file)))
         (map (make-hash-table)))
    (dolist (v valves)
      (setf (gethash (name v) map) v)
      (setf (connected-to v)
            (mapcar [find _ valves :key #'name] ; XXX space required to make vim happy
                    (connected-to v))))
    map))
#+#:excluded (valves)
#+#:excluded (valves #P"scratch.txt")

(defun find-valve (valves name) (gethash name valves))
#+#:excluded (find-valve (valves) 'aa)
#+#:excluded (find-valve (valves) 'at)


(defstruct (state :conc-name)
  current opened released)

(defun init-state (&optional (current 'aa))
  (make-state :current current
              :opened ()
              :released 0))

(defun opened-next (s)
  (sort (remove-duplicates (cons (current s) (copy-seq (opened s)))) #'string<))

(defun release-pressure (valves opened)
  (loop for name in opened for v = (find-valve valves name) sum (rate v)))

(defun released-next (valves s)
  (+ (released s) (release-pressure valves (opened s))))

(defun open-valve (valves s)
  (unless (or (member (current s) (opened s))
              (zerop (rate (find-valve valves (current s)))))
    (make-state :current (current s)
                :opened (opened-next s)
                :released (released-next valves s))))
#+#:excluded (open-valve (valves) (init-state))

(defun change-room (valves s)
  (mapcar (lambda (adj)
            (make-state :current (name adj)
                        :opened (opened s)
                        :released (released-next valves s)))
          (connected-to (find-valve valves (current s)))))
#+#:excluded (change-room (valves) (init-state))

(defun stay-put (valves s)
  (make-state :current (current s)
              :opened (opened s)
              :released (released-next valves s)))
#+#:excluded (stay-put (valves) (init-state))

(defun next (valves s)
  (nconc
    (aand (open-valve valves s) (list it))
    (list (stay-put valves s))
    (change-room valves s)))
#+#:excluded (next (valves) (init-state))

(defun next2 (valves s)
  (loop for n1 in (next valves (make-state :current (car (current s))
                                           :opened (opened s)
                                           :released 0))
        nconc (loop for n2 in (next valves (make-state :current (cadr (current s))
                                                       :opened (opened n1)
                                                       :released 0))
                    collect (make-state :current (sort (list (current n1) (current n2)) #'string<)
                                        :opened (opened n2)
                                        :released (released-next valves s)))))
#+#:excluded (next (valves) (init-state))

(defun maximize (limit valves)
  (let ((seen (make-hash-table :test 'equalp))
        (best 0))
    (labels ((hopelessp (s time)
               (let* ((remaining (- limit time))
                      (closed (sort (remove-if (lambda (v)
                                                 (member (name v) (opened s)))
                                               (hash-table-values valves))
                                    #'>
                                    :key #'rate))
                      (releasable (loop repeat remaining
                                        for opened = (opened s) then (cons (name v) opened)
                                        for v in closed
                                        sum (release-pressure valves opened))))
                 (< (+ (released s) releasable) best)))
             (recur (s time &aux (seen-at (gethash s seen)))
               (cond ((and (= time limit)) (if (> (released s) best)
                                             (pr (maxf best (released s))
                                                 s)))
                     ((and seen-at (<= seen-at time)) nil)
                     ((hopelessp s time) nil)
                     (t (setf (gethash s seen) time)
                        (dolist (sn (next2 valves s))
                          (recur sn (1+ time)))))))
      (recur (init-state 'aa) 0)
      (pr best (hash-table-count seen))
      best)))

(defun maximize2 (limit valves)
  (let ((seen (make-hash-table :test 'equalp))
        (best 0))
    (labels ((hopelessp (s time)
               (let* ((remaining (- limit time))
                      (closed (sort (remove-if (lambda (v)
                                                 (member (name v) (opened s)))
                                               (hash-table-values valves))
                                    #'>
                                    :key #'rate))
                      (releasable (loop repeat remaining
                                        for opened = (opened s) then (cons (name v1) opened)
                                        for v1 = (pop closed)
                                        sum (release-pressure valves opened))))
                 (< (+ (released s) releasable) best)))
             (recur (s time &aux (seen-at (gethash s seen)))
               (cond ((and (= time limit)) (if (> (released s) best)
                                             (pr (maxf best (released s))
                                                 s)))
                     ((and seen-at (<= seen-at time)) nil)
                     ((hopelessp s time) nil)
                     (t (setf (gethash s seen) time)
                        (dolist (sn (next2 valves s))
                          (recur sn (1+ time)))))))
      (recur (init-state (list 'aa 'aa)) 0)
      (pr best (hash-table-count seen))
      best)))

#; Scratch
;; Part 1
(maximize 30 (valves))
(time (maximize2 26 (valves)))
(sb-ext:gc :full t)

;;; was not moving me elephant simoultaneously
;;; one can open a valve while the other can move
;;; already visited was not working in case of star layout where you need to go back to take another path
;;; both human _and_ elephant can end up opening the same valve...
