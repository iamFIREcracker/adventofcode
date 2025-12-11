(defpackage :aoc/2025/11 #.cl-user::*aoc-use*)
(in-package :aoc/2025/11)

(defun read-input (&optional (strings (uiop:read-file-lines #P"src/2025/day11.txt")))
  (looping
    (dolist (s strings)
      (destructuring-bind (from . to)
                          (split-sequence:split-sequence #\Space s)
        (collect! (list* (string-trim ":" from) to))))))


(defun count-paths (from to &optional (input (read-input)))
  (let1 memo (make-hash-table :test 'equal)
    (recursively ((state from))
      (memoizing (memo state)
        (cond ((string= state to) 1)
              (t (looping
                   (dolist (next (assoc-value input state))
                     (sum! (recur next))))))))))


(defun count-paths-through-fft-and-dac (from to &optional (input (read-input)))
  (let1 memo (make-hash-table :test 'equal)
    (recursively ((state from)
                  (fft nil)
                  (dac nil))
      (memoizing (memo state fft dac)
        (cond ((and (string= state to) fft dac) 1)
              (t (looping
                   (dolist (next (assoc-value input state))
                     (sum! (recur next
                                  (or fft (string= next "fft"))
                                  (or dac (string= next "dac"))))))))))))


(define-solution (2025 11) (input read-input)
  (values (count-paths  "you" "out" input)
          (count-paths-through-fft-and-dac "svr" "out" input)))

(define-test (2025 11) (688 293263494406608))

