(defpackage :aoc/2023/07 #.cl-user::*aoc-use*)
(in-package :aoc/2023/07)


(defun list-of-hands (&optional (strings (uiop:read-file-lines #P"src/2023/day07.txt")))
  (mapcar (lambda(s)
            (destructuring-bind (hand bid)
                (split-sequence:split-sequence #\Space s)
              (cons hand (parse-integer bid))))
          strings))

(defun card-freqs (hand) (sort (alist-values (frequencies hand)) #'>))
(defun five-of-a-kind? (hand) (equal '(5) (card-freqs hand)))
(defun four-of-a-kind? (hand) (equal '(4 1) (card-freqs hand)))
(defun full-house? (hand) (equal '(3 2) (card-freqs hand)))
(defun three-of-a-kind? (hand) (equal '(3 1 1) (card-freqs hand)))
(defun two-pairs? (hand) (equal '(2 2 1) (card-freqs hand)))
(defun one-pair? (hand) (equal '(2 1 1 1) (card-freqs hand)))
(defun high-card? (hand) (equal '(1 1 1 1 1) (card-freqs hand)))


(defparameter *j-as-jokers* nil)

(defun hand-type-strength (hand)
  (if (and *j-as-jokers* (find #\J hand))
    (if (string= hand "JJJJJ")
      ;; Replacing a joker with _any_ card will do...
      ;; it should still result in a five-of-a-kind
      (hand-type-strength (substitute #\A #\J hand))
      ;; Otherwise, converting all the jokers into
      ;; the most frequent card of the hand will
      ;; actually result in the stronger hand
      ;; E.g.
      ;; - AJJJJ -> AAAAA
      ;; - AAAKJ -> AAAAK
      (bnd* ((freqs (sort (frequencies hand) #'> :key #'cdr)))
        (doseq ((label . count) freqs)
          (when-not (char= label #\J)
            (return (hand-type-strength (substitute label #\J hand)))))))
    (cond ((five-of-a-kind? hand) 1)
          ((four-of-a-kind? hand) 2)
          ((full-house? hand) 3)
          ((three-of-a-kind? hand) 4)
          ((two-pairs? hand) 5)
          ((one-pair? hand) 6)
          ((high-card? hand) 7))))


(defun stronger-first-card? (hand1 hand2)
  (bnd1 (labels (if *j-as-jokers* "AKQT98765432J" "AKQJT98765432"))
    (doseqs ((i (map 'vector [position _ labels] hand1))
             (j (map 'vector [position _ labels] hand2)))
      (if (< i j)
        (return t)
        (if (> i j)
          (return nil))))))


(defun better-hand? (hand1 hand2)
  (bnd* ((r1 (hand-type-strength hand1))
         (r2 (hand-type-strength hand2)))
    (or (< r1 r2)
        (and (= r1 r2)
             (stronger-first-card? hand1 hand2)))))


(defun total-winning (&optional (input (list-of-hands)))
  (bnd1 (ranked (reverse (sort (copy-seq input) #'better-hand? :key #'car)))
    (looping
      (doseq ((i (hand . bid)) (enumerate ranked :start 1))
        (sum! (* i bid))))))


(define-solution (2023 07) (input list-of-hands)
  (values (total-winning input)
          (bnd1 (*j-as-jokers* t)
            (total-winning input))))

(define-test (2023 07) (251806792 252113488))
