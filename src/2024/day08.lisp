(defpackage :aoc/2024/08 #.cl-user::*aoc-use*)
(in-package :aoc/2024/08)

#;

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


(destructuring-bind (antennas map) (parse-input)
  (looping
    (dohash (antenna positions antennas)
      (dolist (p1 positions)
        (dolist (p2 positions)
          (unless (equal p1 p2)
            (let ((di (- (car p2) (car p1)))
                  (dj (- (cadr p2) (cadr p1))))
              (let1 p3 (list (- (car p1) di) (- (cadr p1) dj))
                (when (@ map p3)
                  (collect! p3))))))))))
(remove-duplicates * :test 'equal)
(length *)
301
334
371!
#+#:excluded (destructuring-bind (antennas map) (parse-input)
               (looping
                 (dohash (antenna positions antennas)
                   (dolist (p1 positions)
                     (dolist (p2 positions)
                       (unless (equal p1 p2)
                         (let ((di (- (car p2) (car p1)))
                               (dj (- (cadr p2) (cadr p1))))
                           (let1 p3 (list (- (car p1) di) (- (cadr p1) dj))
                             (while (@ map p3)
                               (collect! p3)
                               (setf p3 (list (- (car p3) di) (- (cadr p3) dj))))))))))))

(destructuring-bind (antennas map) (parse-input)
  (looping
    (dohash (antenna positions antennas)
      (dolist (p1 positions)
        (dolist (p2 positions)
          (unless (equal p1 p2)
            (let ((di (- (car p2) (car p1)))
                  (dj (- (cadr p2) (cadr p1))))
              (dotimes (n 500) ; LOL
                (let1 p3 (list (- (car p1) (* n di)) (- (cadr p1) (* n dj)))
                  (when (@ map p3)
                    (collect! p3)))))))))))
(remove-duplicates * :test 'equal)
(length *)
1097 too low
1229!
