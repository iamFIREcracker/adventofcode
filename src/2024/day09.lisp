(defpackage :aoc/2024/09 #.cl-user::*aoc-use*)
(in-package :aoc/2024/09)

(defun parse-input (&optional (strings (uiop:read-file-lines #P"src/2024/day09.txt")))
  (let1 id -1
    (looping
      (doseqs ((ch (first strings))
               (file? (ncycle (list t nil))))
        (if file?
            (collect! (list (incf id) (- (char-code ch) (char-code #\0))))
            (collect! (list -1 (- (char-code ch) (char-code #\0)))))))))
#+#:excluded (take 10 (parse-input))

(defun id (i) (car i))
(defun size (i) (cadr i))
(defun file? (i) (>= (car i) 0))


(defun defrag (layout &key whole-files?)
  (let1 remaining (looping
                    (dolist (item layout)
                      (when (file? item)
                        (push! item))))
    (dolist (file remaining)
      (zapf layout [rearrange-file _ file :whole-files? whole-files?]))
    layout))

(defun rearrange-file (layout file &key whole-files?)
  (recursively ((layout layout)
                (file file)
                (moved 0))
    (cond
      ((null layout) (assert nil () "Should never get here?!"))
      ;; No empty blocks to the left of `file` -- update the current
      ;; file and create an empty block after the file, based on
      ;; the number of blocks we were able to move
      ((= (id file) (id (car layout))) (cons file (if (plusp moved)
                                                      (cons (list -1 moved)
                                                            (cdr layout))
                                                      (cdr layout))))
      ;; Leave files in place
      ((file? (car layout)) (cons (car layout) (recur (cdr layout) file moved)))
      ;; Nothing is left to move -- carry on so we can _blank_ the old spot
      ((zerop (size file)) (cons (car layout) (recur (cdr layout) file moved)))
      ;; The the free item is not big enough and we only want to move
      ;; whole files (i.e. part 2), then skip it
      ((and whole-files? (< (size (car layout)) (size file)))
       (cons (car layout) (recur (cdr layout) file moved)))
      ;; Do the magic
      (t (let* ((can-move (min (size (car layout)) (size file)))
                (free-size1 (- (size (car layout)) can-move))
                (file-size1 (- (size file) can-move)))
           (cons (list (id file) can-move)
                 (recur (if (plusp free-size1)
                            (cons (list -1 free-size1)
                                  (cdr layout))
                            (cdr layout))
                        (list (id file) file-size1)
                        (+ can-move moved))))))))


(defun checksum (layout)
  (looping
    (let1 position -1
      (doseq (region layout)
        (repeat (size region)
          (if (file? region)
              (sum! (* (incf position) (id region)))
              (incf position)))))))


(define-solution (2024 9) (layout parse-input)
  (values (~> layout defrag checksum)
          (~> layout (defrag ~ :whole-files? t) checksum)))

(define-test (2024 9) (6398608069280 6427437134372))
