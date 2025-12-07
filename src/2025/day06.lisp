(defpackage :aoc/2025/06 #.cl-user::*aoc-use*)
(in-package :aoc/2025/06)

(defun read-problems-part1 (&optional (strings (uiop:read-file-lines #P"src/2025/day06.txt")))
  (let1 worksheet (looping
                    (dolist (s strings)
                      (collect! (extract-forms s))))
    (apply #'mapcar 'list (last-elt worksheet) (butlast worksheet))))


(defun read-problems-part2 (&optional (strings (uiop:read-file-lines #P"src/2025/day06.txt"))
                            &aux
                            (last-column (1- (length (first strings))))
                            (rator-row (1- (length strings))))
  (prog1-let problems nil
    (let1 numbers nil
      (dorangei (j last-column 0 -1)
        (let ((n 0))
          (doseq ((i row) (enumerate strings))
            (let1 ch (char row j)
              (cond ((find ch "0123456789") (setf n (+ (* n 10) (parse-integer (spr ch)))))
                    ((char= ch #\Space) (when (= i rator-row)
                                          (push n numbers)
                                          (setf n 0)))
                    (t (push (list* (read-from-string (spr ch)) n numbers) problems)
                       (setf numbers nil)
                       (decf j)))))))
      problems)))


(defun sum-problems-answers (problems)
  (reduce '+ (mapcar 'eval problems)))

(define-solution (2025 06) (strings)
  (values (sum-problems-answers (read-problems-part1 strings))
          (sum-problems-answers (read-problems-part2 strings)) ))

(define-test (2025 06) (6957525317641 13215665360076))
