(defpackage :aoc/2024/08 #.cl-user::*aoc-use*)
(in-package :aoc/2024/08)

(defun parse-input (&optional (strings (uiop:read-file-lines #P"src/2024/day08.txt")))
  (let ((antennas (make-hash-table))
        (map (make-hash-table :test 'equal)))
    (doseq ((i s) (enumerate strings))
      (doseq ((j ch) (enumerate s))
        (let1 coord (list i j)
          (setf (@ map coord) ch)
          (when (char/= ch #\.)
            (push coord (@ antennas ch))))))
    (list antennas map)))
#+#:excluded (parse-input)
#+#:excluded (dbg (print-hash-table (car (parse-input))))
#+#:excluded (dbg (print-hash-table (cadr (parse-input))))


;; For each pair antennas transmitting at the same frequency,
;; emit _all_ their antinodes.
;;
;; An antinode is a LIST of two elements: the first element
;; is the location; the second is the _distance_.
;;
;; For example, given these two antennas:
;;
;;   ..A...A....................
;;
;; The function will return the following list of antinodes (the number
;; represents the _distance_).
;;
;;   ..A...1...2...3...4...5...6
(defun antinodes (&optional (input (parse-input)))
  (destructuring-bind (antennas map) input
    (looping
      (dohash (antenna positions antennas)
        (dolist (p1 positions)
          (dolist (p2 positions)
            (unless (equal p1 p2)
              (let ((di (- (car p2) (car p1)))
                    (dj (- (cadr p2) (cadr p1))))
                (let1 multiplier 1
                  (destructuring-bind (i j) p2
                    (while (@ map (list i j))
                      (collect! (list (list i j) multiplier))
                      (incf multiplier)
                      (incf i di)
                      (incf j dj))))))))))))
(defun location (antinode) (car antinode))
(defun distance (antinode) (cadr antinode))


(define-solution (2024 8) (input parse-input)
  (let1 antinodes (antinodes input)
    (values
      (~> antinodes
          (keep-if [= (distance _) 2] ~)
          (remove-duplicates ~ :test 'equal :key #'location)
          length)
      (~> antinodes
          (remove-duplicates ~ :test 'equal :key #'location)
          length))))

(define-test (2024 9) (371 1229))
