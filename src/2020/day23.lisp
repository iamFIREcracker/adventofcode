(defpackage :aoc/2020/23 #.cl-user::*aoc-use*)
(in-package :aoc/2020/23)

(defun parse-cups (data)
  (reverse (digits (parse-integer (first data)))))

(defun play (cups moves &aux (cups-count (length cups))
                  (cups (ncycle cups))
                  (cells (make-hash-table)))
  (labels ((one-less (current)
             (if (> current 1) (1- current) cups-count))
           (destination (cups)
             (loop with (curr cup1 cup2 cup3) = cups
                   for target = (one-less curr) then (one-less target)
                   if (/= target cup1 cup2 cup3)
                   return (gethash target cells))))
    (loop repeat cups-count
          for ref = cups then (cdr ref) do
          (setf (gethash (car ref) cells) ref))
    (loop repeat moves
          for curr = cups for cup1 = (nthcdr 1 cups)
          for cup3 = (nthcdr 3 cups) for after-cup3 = (cdr cup3)
          for dest = (destination cups) for after-dest = (cdr dest) do
          (psetf (cdr curr) after-cup3
                 (cdr cup3) after-dest
                 (cdr dest) cup1
                 cups (cdr cup3))
          ;; propagate above changes into `cells`
          (psetf (gethash (car after-cup3) cells) (cdr curr)
                 (gethash (car after-dest) cells) (cdr cup3)
                 (gethash (car cup1) cells) (cdr dest))
          finally (return (gethash 1 cells)))))

(defun prepare-part2 (cups total-cups)
  (append cups (iota (- total-cups (length cups)) :start 10)))

(define-solution (2020 23) (data)
  (values (loop repeat 8 ; skip cup with label 1
                for cup in (rest (play (parse-cups data) 100))
                collect cup into digits
                finally (return (parse-integer (format nil "~{~A~}" digits))))
          (let ((cups (play (prepare-part2 (parse-cups data) 1000000) 10000000)))
            (* (cadr cups)
               (caddr cups)))))

(define-test (2020 23) (25468379 474747880250))
