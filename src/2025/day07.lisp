(defpackage :aoc/2025/07 #.cl-user::*aoc-use*)
(in-package :aoc/2025/07)

(defun read-input (&optional (strings (uiop:read-file-lines #P"src/2025/day07.txt")))
  (list (list 0 (position #\S (first strings)))
        (prog1-let grid (make-hash-table :test 'equal)
          (doseq ((i s) (enumerate strings))
            (doseq ((j ch) (enumerate s))
              (setf (gethash (list i j) grid) ch))))))


(defun forward (pos)
  (destructuring-bind (i j) pos
    (list (1+ i) j)))

(defun split (pos)
  (destructuring-bind (i j) pos
    (list (list i (1- j))
          (list i (1+ j)))))

(defun count-beam-splits (&optional (input (read-input)))
  (destructuring-bind (beam grid) input
    (let1 seen (make-hash-table :test 'equal)
      (length
        (looping
          (recursively ((beam beam))
            (when-not-seen (seen beam)
              (let1 tile (gethash beam grid)
                (cond ((not tile) nil)
                      ((find tile ".S") (recur (forward beam)))
                      ((char= tile #\^) (adjoin! beam :test 'equal)
                                        (destructuring-bind (beam1 beam2) (split beam)
                                          (recur beam1)
                                          (recur beam2))))))))))))


(defun count-tachyon-timelines (&optional (input (read-input)))
  (destructuring-bind (beam grid) input
    (let1 memo (make-hash-table :test 'equal)
      (recursively ((beam beam))
        (memoizing (memo beam)
          (let1 tile (gethash beam grid)
            (cond ((not tile) 1)
                  ((find tile ".S") (recur (forward beam)))
                  ((char= tile #\^) (destructuring-bind (beam1 beam2) (split beam)
                                      (+ (recur beam1)
                                         (recur beam2)))))))))))

(define-solution (2025 07) (input read-input)
  (values (count-beam-splits input)
          (count-tachyon-timelines input)))

(define-test (2025 06) (1698 95408386769474))
