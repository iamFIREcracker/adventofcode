(defpackage :aoc/2019/22 #.cl-user::*aoc-use*)
(in-package :aoc/2019/22)

(defstruct (deck (:constructor make-deck%)
                 (:conc-name d-))
  size
  offset
  jump)

(defun make-deck (size)
  (make-deck% :size size :offset 0 :jump 1))

(defun deck-position (card d)
  (mod (+ (d-offset d) (* (d-jump d) card)) (d-size d)))

(defun deck-card (position d)
  (mod (* (- position (d-offset d)) (invmod (d-jump d) (d-size d))) (d-size d)))

(defun print-deck (d &aux (r (make-array (d-size d))))
  (dotimes (n (d-size d))
    (setf (aref r (deck-position n d)) n))
  (format t "~a~&" r))

(defun deck-cut (n d)
  (prog1 d
    (setf (d-offset d) (modn (- (d-offset d) n) (d-size d)))))

(defun deck-dins (d)
  (prog1 d
    (setf (d-offset d) (* (d-offset d) -1)
          (d-jump d) (* (d-jump d) -1))
    (deck-cut 1 d)))

(defun deck-dwi (n d)
  (prog1 d
    (setf (d-offset d) (modn (* (d-offset d) n) (d-size d))
          (d-jump d) (modn (* (d-jump d) n) (d-size d)))))

(defun deck-shuffle (instructions d)
  (prog1 d
    (loop
      :for fun :in instructions
      :do (funcall fun d))))

(defun read-instruction (str &aux (parts (split-sequence:split-sequence #\Space str)))
  (cond ((member "increment" parts :test 'equal) (let ((arg (parse-integer (fourth parts))))
                                                   (partial-1 #'deck-dwi arg)))
        ((member "cut" parts :test 'equal) (let ((arg (parse-integer (second parts))))
                                             (partial-1 #'deck-cut arg)))
        ((member "stack" parts :test 'equal) (partial-1 #'deck-dins))))

(defun read-instructions (data)
  (mapcar #'read-instruction data))
;;
;; Calculates the GCD of a and b based on the Extended Euclidean Algorithm. The function also returns
;; the Bézout coefficients s and t, such that gcd(a, b) = as + bt.
;;
;; The algorithm is described on page http://en.wikipedia.org/wiki/Extended_Euclidean_algorithm#Iterative_method_2
;;
(defun egcd (a b)
  (do ((r (cons b a) (cons (- (cdr r) (* (car r) q)) (car r))) ; (r+1 r) i.e. the latest is first.
       (s (cons 0 1) (cons (- (cdr s) (* (car s) q)) (car s))) ; (s+1 s)
       (u (cons 1 0) (cons (- (cdr u) (* (car u) q)) (car u))) ; (t+1 t)
       (q nil))
      ((zerop (car r)) (values (cdr r) (cdr s) (cdr u)))       ; exit when r+1 = 0 and return r s t
    (setq q (floor (/ (cdr r) (car r))))))                     ; inside loop; calculate the q

 
;;
;; Calculates the inverse module for a = 1 (mod m). 
;;
;; Note: The inverse is only defined when a and m are coprimes, i.e. gcd(a, m) = 1.”
;;
(defun invmod (a m)
  (multiple-value-bind (r s k) (egcd a m)
    (declare (ignore k))
    (unless (= 1 r) (error "invmod: Values ~a and ~a are not coprimes." a m))  
    s))

(defun expt-mod (n exponent modulus)
  (if (some (complement #'integerp) (list n exponent modulus))
      (mod (expt n exponent) modulus)
      (loop with result = 1
        for i of-type fixnum from 0 below (integer-length exponent)
        for sqr = n then (mod (* sqr sqr) modulus)
        when (logbitp i exponent)
        do (setf result (mod (* result sqr) modulus))
        finally (return result)))) 

(defun sum-of-powers (base power &key modulus)
  (if modulus
    (mod (* (1- (expt-mod base (1+ power) modulus))
            (invmod (1- base) modulus))
         modulus)
    (/ (1- (expt base (1+ power)))
       (1- base))))

(defun deck-shuffle-ntimes (n instructions d)
  (deck-shuffle instructions d)
  (setf (d-offset d) (mod (* (d-offset d) (sum-of-powers (d-jump d) (1- n) :modulus (d-size d))) (d-size d))
        (d-jump d) (expt-mod (d-jump d) n (d-size d))))

(define-problem (2019 22) (instructions read-instructions)
  (values
    (let ((d (make-deck 10007)))
      (deck-shuffle instructions d)
      (deck-position 2019 d))
    (let ((d (make-deck 119315717514047)))
      (deck-shuffle-ntimes 101741582076661 instructions d)
      (deck-card 2020 d))))

(1am:test test-2019/22
  (multiple-value-bind (part1 part2) (problem-run)
    (1am:is (= 4649 part1))
    (1am:is (= 68849657493596 part2))))
