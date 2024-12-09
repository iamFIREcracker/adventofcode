(defpackage :aoc/2024/09 #.cl-user::*aoc-use*)
(in-package :aoc/2024/09)

#;

(defun parse-input (&optional (s (first (uiop:read-file-lines #P"src/2024/day09.txt"))))
  (let ((id 0) (file? t))
    (looping
      (doseq (ch s)
        (if file?
          (progn
            (collect! (list id (- (char-code ch) (char-code #\0))))
            (incf id))
          (collect! (list -1 (- (char-code ch) (char-code #\0)))))
        (setf file? (not file?))))))
#+#:excluded (take 10 (parse-input))
(defun id (e) (car e))
(defun size (e) (cadr e))
(defun file? (e) (>= (car e) 0))
#+#:excluded (mapcar #'file? (take 10 (parse-input)))

(defun rearrange (&optional (layout (parse-input)))
  (let1 rev-files (reverse (remove-if-not #'file? layout))
    (looping
      (dolist (e layout)
        (if (file? e)
          (when (find-if [= (id _) (id e)] rev-files)
            (collect! e)
            (setf rev-files (remove-if [= (id _) (id e)] rev-files)))
          (if (not rev-files)
            (collect! e)
            (while (and (> (size e) 0) rev-files)
              (let1 file (pop rev-files)
                (let1 placeable (min (size e) (size file))
                  (collect! (list (id file) placeable))
                  (decf (cadr e) placeable)
                  (decf (cadr file) placeable)
                  (if (> (size file) 0)
                    (push file rev-files)))))))))))
#+#:excluded (rearrange)

(defun checksum (layout)
  (looping
    (let1 position -1
      (doseq ((id size) layout)
        (repeat size
          (if (>= id 0)
              (sum! (* (incf position) id))
              (incf position)))))))
(checksum (rearrange))
6398859261607 ; too high

(defun rearrange-file (file layout)
  (recursively ((layout layout))
    (let1 first (car layout)
      (cond ((not first) nil)
            ((= (id file) (id first)) layout) ;;
            ((file? first) (cons first (recur (cdr layout))))
            ((< (size first) (size file)) (cons first (recur (cdr layout))))
            (t (cons file
                     (let1 left (- (size first) (size file))
                       (let1 rest (looping
                                    (dolist (f (cdr layout))
                                      (if (= (id f) (id file))
                                          (collect! (list -1 (size file)))
                                          (collect! f))))
                         (if (plusp left)
                             (cons (list -1 left) rest)
                             rest)))))))))

(defun rearrange2 (&optional (layout (parse-input)))
  (let1 rev-files (reverse (remove-if-not #'file? layout))
    (dolist (file rev-files layout)
      (setf layout (rearrange-file file layout)))))

(time (checksum (rearrange2)))
37270336648016 ; too high
