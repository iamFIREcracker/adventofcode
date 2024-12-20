(defpackage :aoc/2024/20 #.cl-user::*aoc-use*)
(in-package :aoc/2024/20)

(defun parse-input (&optional (strings (uiop:read-file-lines #P"src/2024/day20.txt")))
  (let ((map (make-hash-table :test 'equal))
        start end)
    (doeseq (i s strings)
      (doeseq (j ch s)
        (setf (gethash (list i j) map) ch)
        (if (char= ch #\S)
            (setf start (list i j))
            (if (char= ch #\E)
                (setf end (list i j))))))
    (list map start end)))
#+#:excluded (parse-input)


(defun path-to-end (&optional (input (parse-input)))
  (destructuring-bind (map start end) input
    (cons start
          (recursively ((pos start)
                        prev)
            (cond ((equal pos end) nil)
                  (t (destructuring-bind (i j) pos
                       (doseq ((di dj) '((-1 0) (0 1) (1 0) (0 -1)))
                         (let1 next (list (+ i di) (+ j dj))
                           (when (not (equal next prev))
                             (when (aand (gethash next map) (char/= it #\#))
                               (return (cons next (recur next pos))))))))))))))
#+#:excluded (path-to-end)
#+#:excluded (length *)

;; A shortcut is convenient if it takes you further down on the path (i.e,
;; closer to destination); this means that given a position on the path, their
;; candidate shortcuts can only be found in the remainder of the path.
;; Furthermore, a shortcut is legit if the beginning and ending positions of
;; the shortcuts are at most `max-cut-distance` locations away.
(defun count-cheat-cuts-if (predicate &optional (max-cut-distance 2) (input (parse-input)))
  (looping
    (dosublists ((pos . path) (path-to-end input))
      (let1 path-len (length path)
        (doeseq ((n 1) pos1 path)
          (let1 distance (manhattan-distance pos1 pos)
            (let1 path1-len (- path-len n)
              (when (<= distance max-cut-distance)
                (let1 new-path-len (+ distance path1-len)
                  (when (< new-path-len path-len)
                    (count! (funcall predicate (- path-len new-path-len)))))))))))))


(define-solution (2024 20) (input parse-input)
  (values (count-cheat-cuts-if [>= _ 100] 2  input)
          (count-cheat-cuts-if [>= _ 100] 20 input)))

(define-test (2024 20) (1463 985332))
