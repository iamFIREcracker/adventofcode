(defpackage :aoc/2025/04 #.cl-user::*aoc-use*)
(in-package :aoc/2025/04)

(defun read-grid (&optional (strings (uiop:read-file-lines #P"src/2025/day04.txt")))
  (prog1-let grid (make-hash-table :test 'equal)
    (doseq ((i s) (enumerate strings))
      (doseq ((j ch) (enumerate s))
        (setf (gethash (list i j) grid) ch)))))


(defun find-accessible-rolls (&optional (grid (read-grid)))
  (flet ((accessible? (pos)
           (< (looping
                (dolist (pos2 (adjacents pos :include-diagonal t))
                  (count! (char= (gethash pos2 grid #\?) #\@))))
              4)))
    (looping
      (dohashk (pos grid)
        (when (char= (gethash pos grid) #\@)
          (when (accessible? pos)
            (collect! pos)))))))
#+#:excluded (length (find-accessible-rolls))

(defun remove-all-rolls (&optional (grid (read-grid)))
  (looping
    (recursively ()
      (let1 to-remove (find-accessible-rolls grid)
        (dolist (pos to-remove)
          (setf (gethash pos grid) #\.)
          (sum! 1))
        (when to-remove
          (recur))))))
#+#:excluded (remove-all-rolls)

(define-solution (2025 04) (grid read-grid)
  (values (length (find-accessible-rolls grid))
          (remove-all-rolls grid)))

(define-test (2025 04) (1604 9397))
