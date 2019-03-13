(defpackage :aoc/2017/23 #.cl-user::*aoc-use*)
(in-package :aoc/2017/23)

(defun i-sub (x y &aux (pos (aoc/2017/18:reg-name-to-pos x)))
  (decf (aref aoc/2017/18:*registers* pos) (aoc/2017/18:value-or-reg-content y)))

(defun i-jnz (x y)
  (unless (zerop (aoc/2017/18:value-or-reg-content x))
    (incf (aref aoc/2017/18:*registers* aoc/2017/18:ip-pos) (1- (aoc/2017/18:value-or-reg-content y)))))

(defun solve-part-2 ()
  "It turns out the input program tries to find all the not-prime numbers
  in the range [108100, 125100].
  
  How did I figure it out? By converting the input code into working CL code
  first (TAGBODY for the win), and with a lot of PRLs.

    (defun solve-part-2 ()
      (let ((b 0) (c 0) (d 0) (e 0) (f 0) (g 0) (h 0))
        (tagbody
          (setf b 81
                c b)
          part-a
            (prl 'part-a)
            (setf b (* b 100)
                  b (- b -100000)
                  c b
                  c (- c -17000))
          part-b
            (prl 'part-b b c (break))
            (setf f 1
                  d 2)
          part-e
            (prl 'part-e)
            (setf e 2)
          part-d
            (prl 'part-d d e b)
            (setf g (- (* d e) b))
            (if (not (zerop g))
              (go part-c))
            ; (prl 'resetting-f (break))
            (prl 'resetting-f)
            (setf f 0)
          part-c
            (prl 'part-c d e b)
            (setf e (- e -1)
                  g e
                  g (- g b))
            (if (not (zerop g))
              (go part-d))
            (setf d (- d -1)
                  g d
                  g (- g b))
            (if (not (zerop g))
              (go part-e))
            (if (not (zerop f))
              (go part-f))
            ; (prl 'setting-h (break))
            (prl 'setting-h)
            (setf h (- h -1))
          part-f
            (prl 'part-f b c)
            (setf g b
                  g (- g c))
            (if (not (zerop g))
              (go part-g))
            (return-from solve-part-2 (prl b c d e f g h))
          part-g
            (prl 'part-g)
            (setf b (- b -17))
            (go part-b))))

  I guess your eyes are bleeding..."
  (flet ((primesp (n)
           (and (> n 1)
                (or (= n 2) (oddp n))
                (loop
                  :for i :from 3 :to (isqrt n) :by 2
                  :never (dividesp i n)))))
    (loop
      :for n :from 108100 :to 125100 :by 17
      :count (not (primesp n)))))

(define-problem (2017 23) (data)
  (setf aoc/2017/18:*instructions-by-name* `(("set" ,#'aoc/2017/18:i-set)
                                             ("sub" ,#'i-sub)
                                             ("mul" ,#'aoc/2017/18:i-mul)
                                             ("jnz" ,#'i-jnz)))
  (values
    (loop
      :with instructions = (aoc/2017/18:parse-instructions data)
      :with current = (aoc/2017/18:make-program 0 instructions)
      :with aoc/2017/18:*registers* = (aoc/2017/18:program-registers current)
      :for name = (aoc/2017/18:program-next-instruction-name current)
      :count (string= "mul" name)
      :while name
      :do (aoc/2017/18:program-exec-next-instruction current))
    (solve-part-2)))

(1am:test test-2017/23
  (multiple-value-bind (part1 part2) (problem-run)
    (1am:is (= 6241 part1))
    (1am:is (= 909 part2))))
