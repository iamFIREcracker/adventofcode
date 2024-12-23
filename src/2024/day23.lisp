(defpackage :aoc/2024/23 #.cl-user::*aoc-use*)
(in-package :aoc/2024/23)

(defun parse-input (&optional (strings (uiop:read-file-lines #P"src/2024/day23.txt")))
  (prog1-let (edges (make-hash-table :test 'equal))
    (doseq (s strings)
      (destructuring-bind (from to) (split-sequence:split-sequence #\- s)
        (pushnew to (gethash from edges))
        (pushnew from (gethash to edges))))))
#+#:excluded (parse-input)


(defun canonicalize (clique)
  (sort (copy-seq clique) #'string<))

(defun find-cliques (size &optional (edges (parse-input)))
  (let1 seen (make-hash-table :test 'equal)
    (looping
      (dohashk (comp edges)
        (recursively ((clique (list comp))
                      (computers (gethash comp edges)))
          (zapf clique #'canonicalize)
          (unless-already-seen (seen clique)
            (cond ((= (length clique) size) (adjoin! (canonicalize clique)))
                  (t (dolist (comp computers)
                       (recur (cons comp clique)
                              (intersection computers (gethash comp edges)
                                            :test 'string=)))))))))))


(defun find-max-clique (&optional (edges (parse-input)))
  (let1 seen (make-hash-table :test 'equal)
    (looping
      (dohashk (comp edges)
        (recursively ((clique (list comp))
                      (computers (gethash comp edges)))
          (zapf clique #'canonicalize)
          (maximize! clique :key #'length)
          (unless-already-seen (seen clique)
            (dolist (comp computers)
              (recur (cons comp clique)
                     (intersection computers (gethash comp edges)
                                   :test 'string=)))))))))


(define-solution (2024 23) (edges parse-input)
  (values (looping
            (dolist (clique (find-cliques 3 edges))
              (count! (some [string-starts-with-p "t" _] clique))))
          (~> (find-max-clique edges) (format nil "~{~A~^,~}" ~))))

(define-test (2024 23) (1000 "cf,ct,cv,cz,fi,lq,my,pa,sl,tt,vw,wz,yd"))
