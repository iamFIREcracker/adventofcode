(defpackage :aoc/2020/09 #.cl-user::*aoc-use*)
(in-package :aoc/2020/09)

(defun make-set (numbers)
  (let ((set (make-hash-table)))
    (dolist (x numbers set)
      (setf (gethash x set) t))))

(defun sadd (validator number) (setf (gethash number validator) t))
(defun srem (validator number) (remhash number validator))

(defun validp (validator number)
  (loop for n being the hash-keys of validator
        thereis (gethash (- number n) validator)))

(defun find-first-invalid (numbers)
  (loop with s = (make-set (subseq numbers 0 25))
        for oldest in numbers for newest in (nthcdr 25 numbers)
        if (not (validp s newest)) return newest
        else do (srem s oldest) (sadd s newest)))

(defun subseq-that-adds-up-to (numbers target &optional frontier)
  (loop with n = (first numbers)
        for (psum . seq) in (cons (cons 0 nil) frontier)
        for psum-next = (+ n psum) for seq-next = (cons n seq)
        if (= psum-next target) return seq-next
        else collect (cons psum-next seq-next) into frontier-next
        finally (return (subseq-that-adds-up-to (rest numbers)
                                                target
                                                frontier-next))))

(define-solution (2020 9) (numbers parse-integers)
  (let* ((invalid (find-first-invalid numbers))
         (subseq (subseq-that-adds-up-to numbers invalid)))
    (values
      invalid
      (+ (apply 'min subseq) (apply 'max subseq)))))

(define-test (2020 9) (177777905 23463012))
