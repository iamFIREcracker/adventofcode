(defpackage :aoc/2021/18 #.cl-user::*aoc-use*)
(in-package :aoc/2021/18)

(defun parse-snumbers (data) (mapcar #'sread data))

(defun sread (string)
  (flet ((ch->number (ch) (- (char-code ch) (char-code #\0))))
    (loop for ch across string
          unless (eql ch #\,) collect (if (find ch "[]") ch (ch->number ch)))))


(defun s+ (number1 number2)
  (sreduce (append
             (list #\[)
             number1
             number2
             (list #\]))))


(defun sreduce (number)
  (loop
    (uiop:if-let (exploded (sexplode number))
      (setf number exploded)
      (uiop:if-let (split (ssplit number))
        (setf number split)
        (return number)))))


(defun sexplode (number &aux (depth 0) to-the-left)
  (loop for (v . to-the-right) on number do
        (cond ((numberp v) (push v to-the-left))
              ((eql v #\]) (push v to-the-left) (decf depth))
              ((and (eql v #\[) (/= depth 4)) (push v to-the-left) (incf depth))
              (t (let ((left (pop to-the-right))
                       (right (pop to-the-right)))
                   (assert (and (eql v #\[) (= depth 4)))
                   (assert (eql (pop to-the-right) #\]))
                   (flet ((add-first (addend list)
                            (loop for (v . to-the-right) on list
                                  collect (if (numberp v) (+ v addend) v) into vv
                                  when (numberp v) append to-the-right into vv and return vv
                                  finally (return vv))))
                     (return
                       (append
                         (reverse (add-first left to-the-left))
                         (list 0)
                         (add-first right to-the-right)))))))))


(defun ssplit (number)
  (loop for (v . to-the-right) on number
        if (or (not (numberp v)) (< v 10)) collect v into to-the-left
        else return (append
                      to-the-left
                      (list #\[)
                      (list (floor v 2) (ceiling v 2))
                      (list #\])
                      to-the-right)))


(defun smagnitude (number &aux stack)
  (loop for (ch . remaining) on number do
        (case ch
          (#\[ (push ch stack))
          (#\] (let ((right (pop stack))
                     (left (pop stack)))
                 (assert (eql (pop stack) #\[))
                 (push (+ (* left 3) (* right 2)) stack)))
          (otherwise (push ch stack))))
  (pop stack))


(defun part1 (numbers) (smagnitude (reduce #'s+ numbers)))


(defun part2 (numbers)
  (loop for (n1 . remaining) on numbers maximize
        (loop for n2 in remaining
              maximize (smagnitude (s+ n1 n2))
              maximize (smagnitude (s+ n2 n1)))))


(define-solution (2021 18) (numbers parse-snumbers)
  (values (part1 numbers) (part2 numbers)))

(define-test (2021 18) (3987 4500))
