(defpackage :aoc/2022/08 #.cl-user::*aoc-use*)
(in-package :aoc/2022/08)


(defun parse-map (&optional (strings (uiop:read-file-lines #P"src/2022/day08.txt")))
  (flet ((char-to-int (c) (- (char-code c) (char-code #\0))))
    (let* ((rows (mapcar [map 'list #'char-to-int _] strings))
           (m (length rows))
           (n (length (car rows))))
      (make-array (list m n) :initial-contents rows))))


(defun part1 (map)
  (destructuring-bind (m n) (array-dimensions map)
    (loop for i from 0 below m sum (loop for j from 0 below n count (count-visible map i j)))))

(defun count-visible (map i j)
  (destructuring-bind (m n) (array-dimensions map)
    (flet ((smallerp (ii jj) (> (aref map i j) (aref map ii jj))))
      (or
       (loop for ii from (1- i) downto 0 always (smallerp ii j))
       (loop for ii from (1+ i) below m always (smallerp ii j))
       (loop for jj from (1- j) downto 0 always (smallerp i jj))
       (loop for jj from (1+ j) below n always (smallerp i jj))))))


(defun part2 (map)
  (destructuring-bind (n m) (array-dimensions map)
    (loop for i from 0 below n maximize (loop for j from 0 below m maximize (scenic-score map i j)))))

(defun scenic-score (map i j)
  (destructuring-bind (m n) (array-dimensions map)
    (flet ((smallerp (ii jj) (> (aref map i j) (aref map ii jj))))
      (*
        (loop for ii from (1- i) downto 0 sum 1 while (smallerp ii j))
        (loop for ii from (1+ i) below m sum 1 while (smallerp ii j))
        (loop for jj from (1- j) downto 0 sum 1 while (smallerp i jj))
        (loop for jj from (1+ j) below n sum 1 while (smallerp i jj))))))


(defun solution-run (&optional (map (parse-map)))
  (values (part1 map) (part2 map)))

(define-test (2022 08) (1719 590824))

;;; Version 1:
;;; - It's m-by-n matrices, not n-by-m lol
;;; - Use :sum instead of :count (the latter is usually paired with
;;;  a condition, an event whose occurrences you want to count, while here we
;;;  are always summing 1)
;;; - Factor out CHAR-TO-INT and SMALLERP
;;; - [link](https://topaz.github.io/paste/#XQAAAQADBgAAAAAAAAAUGQimgx+p6PZhRh7uIO+WahZ/TufE7S99VrN5o7dyceR0EkHT0+1UdT1z5YLK+57VWn7TfDqNY4sMgGEOPOOAICHOmvmnF83j0HYhHjituh1Wu6Mwl+0XcQB59Z0ZSq+A6Ptns+aE16ik4ktMIJRS6SE9aSvSpD3OyYXzXTT3w2zjvEuvnKcgsD8fOOiQQrrrfNIJ6b7E6n8hXJl/CEnFLkHsLItraEaxQeE6nOXz7kSVhyGsf1G5X/Q16niirlpMliXocd0kLPb7HmNaeUJuoinM+RIhCaIhLHS/K326bYk9td/dILXj9tt6jdkvD/tteUa9rJ0jilAk9nAtxrk6tGxwB/KQ20ywh/Iy/kSQ7Xcz80lEOiNN6E4gMozlAJRICbSihDN2/8JsyRIKQbtchDsA4k7BF65ILoKW5DALrTE9V9Q1TzSkxzGn/ZdfKISTW8YRwrNghiTX8IweDASJkNYXYRrxHcngBAW7G9OSUAv5+Qu73Jl5+Q8F0r9bPPVJYaEccbLuR//vTubLIOJEHVrRLT4iQC9rbx/O52oWqGmiScG7ZXY0nY5BNeOpaPNZD714qAHHrOBNs8gcGbPYNWx0LljgPnP+Jx8b7wRbkKMok0wQvgIH/7RYSUw=)
;;; Version 0:
;;; - [link](https://topaz.github.io/paste/#XQAAAQAABgAAAAAAAAAUGQimgx+p6PZhRh7uIO+WahZ/TufE7S99VrN5o7dyceR0EkHT0+1UdT1z5YLK+57VWn7TfDqNY4sMgGEOPOOAICHOmvmnF83j0HYhHjituh1Wu6Mwt+Roti8PNO9TrxtMYfNKl4v/KYhz3i+47/dZZnc4iqAwyG9tSC3L6p7vdMXHTWZR4We5+Gc0P7hhF7MC92x4modIv1uLjFBUXgF5z7N4FAjfRcWla9+f2RRSS7UUdHHMJ5BMT+AuMchHF02FJuejCyBEORCVnWEOq5JP9spnrqFwM82roC4RPXuE7H3PZSlDtitQDuvN1gAoKgDksIVi3oS3P5C/UTM9z8OZ26gYUzDeKHtSunRgsvREgb1ZEVjfr7t7D9xHDVNV5jse8HzdKul3czlQI86mOqvhZnIn6tNqijlC3pVT3zZfG4NbWNnScd71Jm+qbQVwtO3awn2UBQbi2KfCNYMbDiyWscZZVaakUQKWMrgrXp65LNOvZrLFUmYuZsroPcRBmfX/tGcIgZxiEe/NRC1TEoBHu9FPi4bHqTLa6+9yhL9Pdshe0bjVv1Vf7CLgrtx2PBn/p3ueQg==)
