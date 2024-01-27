(defpackage :aoc/2023/12 #.cl-user::*aoc-use*)
(in-package :aoc/2023/12)

(defun parse-condition-record (s)
  (destructuring-bind (springs groups) (split-sequence:split-sequence #\Space s)
    (list springs (extract-positive-integers groups))))


(defun count-valid-arrangements (s)
  (destructuring-bind (springs groups) (parse-condition-record s)
    (bnd1 (memo (make-hash-table :test 'equal))
      (labels ((recur (springs current remaining)
                 ;; It's important to keep (car springs) as part of the key;
                 ;; when stomping on a ?, we are recursively call ourselves
                 ;; after having prepended either a # or a . character to
                 ;; the list of springs, which means there could be multiple
                 ;; values for the same length
                 (memoizing (memo (car springs) (length springs) current (length remaining))
                   (bnd1 (ch (car springs))
                     (cond
                       ((and (null springs) (plusp current) (not remaining)) 0)
                       ((and (null springs) (plusp current))
                        (if (= (car remaining) current)
                          (recur springs 0 (cdr remaining))
                          0))
                       ((and (null springs) (zerop current) (not remaining) 1))
                       ((and (null springs) (zerop current) remaining 0))
                       ((and (plusp current) (not remaining)) 0)
                       ((and (char= ch #\.) (plusp current))
                        (if (= (car remaining) current)
                          (recur (cdr springs) 0 (cdr remaining))
                          0))
                       ((and (char= ch #\.) (zerop current))
                        (recur (cdr springs) 0 remaining))
                       ((char= ch #\# ) (recur (cdr springs) (1+ current) remaining))
                       ((and (char= ch #\?))
                        (+ (recur (cons #\. (cdr springs)) current remaining)
                           (recur (cons #\# (cdr springs)) current remaining)))
                       (t (error "WTF?!")))))))
        (recur (coerce springs 'list) 0 groups)))))


(defun repeat-seq (seq times)
  (loop :repeat times :collect seq))

(defun join (list sep) (apply #'mkstrs sep list))

(defun massage-input (s)
  (destructuring-bind (springs groups) (split-sequence:split-sequence #\Space s)
    (mkstr (join (repeat-seq springs 5) #\?)
           " "
           (join (repeat-seq groups 5) #\,))))


(define-solution (2023 12) (strings)
  (values (reduce #'+ strings :key #'count-valid-arrangements)
          (reduce #'+ strings :key [count-valid-arrangements (massage-input _)])))

(define-test (2023 12) (7460 6720660274964))
