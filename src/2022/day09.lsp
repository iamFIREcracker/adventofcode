(defpackage :aoc/2022/09 #.cl-user::*aoc-use*)
(in-package :aoc/2022/09)


(defun parse-move (string)
  (with-input-from-string (s string)
    (let ((dir (read s))
          (steps (read s)))
      (list steps
            (ecase dir
              (u (list 0 -1))
              (r (list 1 0))
              (d (list 0 1))
              (l (list -1 0)))))))

(defun parse-moves (&optional (strings (uiop:read-file-lines #P"src/2022/day09.txt")))
  (mapcar #'parse-move strings))


(defun simulate (&optional (rope-size 2) (moves (parse-moves)))
  (let ((rope (loop repeat rope-size collect (list 0 0)))
        (visited (make-hash-table :test 'equal)))
    (flet ((track-tail ()
             (setf (gethash (copy-list (car (last rope))) visited) t)))
      (loop with (head . tail) = rope for (steps dir) in moves do
            (loop repeat steps do
                  (progn
                    (movef head dir)
                    (loop for prev = head then knot for knot in tail do
                          (when (too-far-p prev knot)
                            (adjustf prev knot)))
                    (track-tail)))))
    (hash-table-count visited)))


(defun movef (knot dir)
  (destructuring-bind (dx dy) dir
    (incf (car knot) dx)
    (incf (cadr knot) dy)))

(defun adjustf (kn1 kn2)
  (destructuring-bind (x1 y1) kn1
    (destructuring-bind (x2 y2) kn2
      (let ((dx (<=> x1 x2))
            (dy (<=> y1 y2)))
        (movef kn2 (list dx dy))))))

(defun too-far-p (kn1 kn2)
  (destructuring-bind (x1 y1) kn1
    (destructuring-bind (x2 y2) kn2
      (if (or (= x1 x2) (= y1 y2))
        (> (manhattan-distance kn1 kn2) 1)
        (> (manhattan-distance kn1 kn2) 2)))))


(define-solution (2022 09) (moves parse-moves)
  (values (simulate 2 moves) (simulate 10 moves)))

(define-test (2022 09) (6498 2531))

;;; Version 1:
;;; - Renamed functions / variables
;;; - Implemented coordinates as _proper_ lists (i.e. `(list x y)` vs `(cons x y)`)
;;; - [link](https://topaz.github.io/paste/#XQAAAQBMBgAAAAAAAAAUGQimgx+p6PZhRh7uIO+bSkP/DQZHn8eJQavVd678Xc7E2qRAdXKlDkS6qKhBN2IfofR/aQw180pzEh0YBUJ3vanc75YzeLPQubBbjFGkUAdj3X+COAi2/zF0l6KYlT2uOQgupw4a4SstqKkk/zhM0kKvM5NzAp5cX6+xKsNwBVGd5oUXUmaQuuHN25A7mis2DS6saSmeDQdH3jhm28e/jNXKzyQ7YqOyj3mPiBMaZGsTVkj+tuvvHwWPjS+JjGfxTky6ZmpMWmfoy05THjHxQRhrPXRzDDXb4f6SUiBCWuPiR31d4tKZST2ooVNIjPxpiXhGeJ5da/U9NznklGeudkgMDCSF70B3snL7Dqx+cZ/AJaW5u73oxJXYEEtiexIRwmSXpLZL3r1KAzQyA4Zua1QRtKvf92zN7Qpo81FDYxfB4We60yU/LJOuQwSDU5AQGwreE5ZacS6SKVyI+xBs/c0eoHrQjmEDNaJyGGS+CjMh1CFSzMIPgTDU6l8s0hXu9ubM2vzruSIsr6636maosf1ZuHAgeZ5kX5arPuxHwWP25Z/qA5IkWAK7GZ3mQP/uGTLqKpKMpusCCa34f6NZqqhO0L9WdflNgWlasBEqJsjqVAhjVwX82vChD5aM0sPlZ8kY+7l0IrWmwfkg/w6aU4Rh9CGFDCgYHdCFDAuQ93rEYwMAILaBxMlrTaeVlEqFWsfNcL5sDbp+QMJOCLqxwcs7O0Jge4nCNpHVnGGhngB1BVMSOY/Mugz58t+cRFgtg3zDEQYA3kSSWSBEGdsrl/gADa8uWLZLUHRVWiFuOPLmvHkj3cR20Ujl37M3RiMjvAUWFNMmZf6dbf3yY44=)
;;; Version 0:
;;; - Spaceship operator to understand how `tails` follows `head`
;;; - [link](https://topaz.github.io/paste/#XQAAAQCeCgAAAAAAAAAUGQimgx+p6PZhRh7uIO+bSkP/DQZHn8eJQavVd678Xc7E2qRAdXKlDkS6qKhBN2IfofR/aQw180pzEh0YBUJ3vanc75YzeLPQubBbjFGkUAdj3X+COAi2/zF0l6KYlT2uOQgupw4a4SstqKkk/zhM0kKvM5NzAp5cX6+xKsNwBVGd5oUXUmaQuuHN25A7mis2DS6saSmeDQdH3jhm28e/jNXKzyQ7YqOyj3mPiBMaZGsTVkj+tuvvHwWPjS+JjGfxTky6ZmpMWmfoy05THjHxP2EX/+N1wWulsIfgg8JBXN4uG+LSKdSxeg/Q4+lg5SvJWI41IpuhJw3JCc4oKhi9jI1/JbQcJ4ASQx1/gU8Myvxyme4I+pWItn93K4bmGdd0q87JHQw21wEzt0C+6tgxx32z25II4A+x3UYZFkvxHqj2AuivkrfRIsMDxdQ765d7USHi7sjBQrQs4EOSc9Eb9ymz2VFE0oXU2YMdzSuOX9AOse/7SLe0qPaBGZ+/kLbgEobjybHn6pUjlUb7+KuWnBUV+pV1MMcgukPlTFZUWS9uBkpQBFgLGGclqGWiMLJO1+bDE8hw/n0ylAi9+/QIIzHoJob7Nt8adt8OYY17JwLRRLG4Kwzz5lMZeEwV027vPJJ+OMv3EdBvVZoXWmyLk680eWBJNUF7Amif2CJGjHyp+j+B2uWGsfAJDyjPVna18CUqzMEI5NywyHURMJLSAHBEi1J21LF9gR0ZnSYUxIJqcvBF764hxmGqmZFsxMWPb6R5rD1HdlQj3p2nvnjjaCTkV1TA7Vy8PEzGHAft1ZSJ28a6zrXU7OqgOsNxwx+sB10wGvbv/edEhzTXh4+T7REvA44c0dwZU/cvK1UEJNOMAonnME4k5PHVeOJNO1fLSe6A4tBVJf7zD+BwgHG9zjbrwOxAlFQBKgGSatjDvU71RwHCC0/zWkjoC3RRM4fIGm1tPjMuVgVRuAnC227ZSxWLU9xPt7XmaS+ja7QUJbIhWHbGBxn0jBvPmHeDvNGpOgrycjMh2Le1qgBMTLQdogfHe8J9B5mjeV44+9fnwtxYT4MBKvTIZjLQin668f/X+6bm)
