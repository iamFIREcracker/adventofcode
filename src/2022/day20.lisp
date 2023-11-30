(defpackage :aoc/2022/20 #.cl-user::*aoc-use*)
(in-package :aoc/2022/20)

(defun numbers (&optional (file #P"src/2022/day20.txt"))
  (uiop:read-file-forms file))

(defstruct (state (:conc-name))
  refs ring)

(defun make-cyclic-dlink (content)
  (bnd1 (d (make-dlink :content content))
    (setf (dlink-prev d) d
          (dlink-next d) d)
    d))

(defun init-state (&optional (numbers (numbers)))
  (let* ((head (make-cyclic-dlink (car numbers)))
         (refs (list head)))
    (dolist (each (cdr numbers))
      (push (dlink-insertf (car refs) each) refs))
    (make-state :refs (reverse refs)
                :ring head)))

(defun dlink-move-forward (curr)
  (let* ((prev (dlink-prev curr))
         (next (dlink-next curr))
         (next2 (dlink-next next)))
    (psetf (dlink-next prev)  next
           (dlink-next next)  curr
           (dlink-next curr)  next2
           (dlink-prev next2) curr
           (dlink-prev curr)  next
           (dlink-prev next)  prev
           )))

(defun dlink-move-backward (curr)
  (let* ((prev (dlink-prev curr))
         (next (dlink-next curr))
         (prev2 (dlink-prev prev)))
    (psetf (dlink-next prev2) curr
           (dlink-next curr)  prev
           (dlink-next prev)  next
           (dlink-prev next)  prev
           (dlink-prev prev)  curr
           (dlink-prev curr)  prev2)))

(defun print-state (state)
  (bnd1 (first (car (refs state)))
    (pr (loop for i from 0 for each = first then (dlink-next each)
              collect (dlink-content each)
              if (> i 10000) do (error "FUCK")
              until (eq (dlink-next each) first)))
    (values)))

(defun mix (times state)
  (repeat times
    (dolist (each (refs state))
      ;; mod, abs, then keep on using backward/forward? wtf?
      (repeat (mod (abs (dlink-content each)) 4999)
        (if (< (dlink-content each) 0)
          (dlink-move-backward each)
          (dlink-move-forward each)))))
  state)

(defun mix (times state)
  (bnd1 (max-swap (1- (length (refs state))))
    (repeat times
      (dolist (each (refs state))
        (repeat (mod (dlink-content each) max-swap)
          (dlink-move-forward each)))))
  state)

(defun coordinates (state)
  (bnd1 (zero (find-if [= _ 0] (refs state) :key #'dlink-content))
    (loop for i from 0 for each = zero then (dlink-next each)
          when (member i (list 1000 2000 3000)) sum (pr (dlink-content each))
          until (= i 3001))))

#; Scratch
(numbers)
(length (numbers))

(mapcar #'dlink-content (refs (init-state (numbers #P"scratch.txt"))))
(print-state (init-state (numbers #P"scratch.txt")))

(print-state (mix (init-state (list -1 0 1 2))))
(print-state (mix (init-state (list 4 0 1 2))))
(coordinates (mix 1 (init-state (numbers #P"scratch.txt"))))
(coordinates (mix 1 (init-state (numbers))))

;; Part 1
(coordinates (mix 1 (init-state (numbers))))
;; Part 2
(coordinates (mix 10 (init-state (mapcar [* _ 811589153] (numbers)))))

(refs (init-state))
(&optional (file #P"src/2022/day20.txt"))
#P"scratch.txt"
-10381
571358763712

;;; spent so much time trying to implement a smart solution, i.e. find the target
;;; node and _place_ the current one right _after_ it.
;;; ultimately I decided to swap one element at a time, and that immediately got my star
;;; part 2 was kind of easy -- I only had to figure out I should mod by 4999 instad of 5000
;;; also, I submitted the wrong answer because I had accidentally placed the REPEAT logic
;;; inside the loop that iterates the inital list of elements, meaning it would only mix
;;; once (with bigger numbers, but one time nonetheless)
