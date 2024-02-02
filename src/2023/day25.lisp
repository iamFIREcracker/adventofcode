(defpackage :aoc/2023/25 #.cl-user::*aoc-use*)
(in-package :aoc/2023/25)


(defun parse-connections (&optional (strings (aoc::read-problem-input 2023 25)))
  (bnd1 (adj (make-hash-table))
    (dolist (s strings)
      (destructuring-bind (a . rest) (mapcar #'as-keyword (cl-ppcre:all-matches-as-strings "\\w+" s))
        (dolist (b rest)
          (push b (gethash a adj))
          (push a (gethash b adj)))))
    adj))


(defun random-edge (nodes adj)
  ;; TODO: is the below truly uniform?!
  (bnd* ((a (random-elt nodes))
         (b (random-elt (gethash a adj))))
    (cons a b)))


(defun karger-min-cut (&optional (adj (parse-connections)))
  (setf adj (copy-hash-table adj))
  (bnd* ((nodes (hash-table-keys adj))
         contracted-into)
    (labels ((contract (a b)
               (dolist (c (gethash b adj))
                 (removef (gethash c adj) b)
                 (unless (eq c a)
                   (push c (gethash a adj))
                   (push a (gethash c adj))))
               (remhash b adj)
               (removef nodes b)
               (appendf (getf contracted-into a) (list b) (getf contracted-into b))))
      (while (> (hash-table-count adj) 2)
        (destructuring-bind (a . b) (random-edge nodes adj)
          (contract a b))))
    (destructuring-bind (a b) nodes
      (values (length (gethash a adj))
              (cons a (getf contracted-into a))
              (cons b (getf contracted-into b))))))


(define-solution (2023 25) (adj parse-connections)
  (recursively ()
    (multiple-value-bind (cut l r) (karger-min-cut adj)
      (if (= cut 3)
        (* (length l) (length r))
        (recur)))))

(define-test (2023 25) (544523))
