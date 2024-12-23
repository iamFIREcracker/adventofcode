(defpackage :aoc/2024/23 #.cl-user::*aoc-use*)
(in-package :aoc/2024/23)

#;



(defun parse-input (&optional (strings (uiop:read-file-lines #P"src/2024/day23.txt")))
  (prog1-let (conns (make-hash-table :test 'equal))
    (doseq (s strings)
      (destructuring-bind (from to) (split-sequence:split-sequence #\- s)
        (pushnew to (gethash from conns))
        (pushnew from (gethash to conns))))))
#+#:excluded (parse-input)

(defun find-connected (conns &optional (size 3))
  (looping
    (dohashk (comp conns)
      (recursively ((network (list comp))
                    (computers (gethash comp conns)))
        (cond ((= (length network) size) (adjoin! (sort (copy-seq network) #'string<) :test 'equal))
              (t (dolist (comp computers)
                   (recur (cons comp network)
                          (intersection computers (gethash comp conns) :test 'string=)))))))))
(find-connected (parse-input))
(find-connected (parse-input))
(count-if [some [string-starts-with-p "t" _] _] (find-connected (parse-input)))
1000!

(defun find-connected (conns &optional (size 3))
  (let1 seen (make-hash-table :test 'equal)
    (looping
      (dohashk (comp conns)
        (recursively ((network (list comp))
                      (computers (gethash comp conns)))
          (zapf network [sort (copy-seq _) #'string<])
          (unless-already-seen (seen network)
            (cond ((= (length network) size) (format t "~{~A~^,~}" network) (count! 1))
                  (t (dolist (comp computers)
                       (recur (cons comp network)
                              (intersection computers (gethash comp conns) :test 'string=)))))))))))
(find-connected (parse-input))
(find-connected (parse-input) 4)
(find-connected (parse-input) 5)
(find-connected (parse-input) 6)
(find-connected (parse-input) 10)
(find-connected (parse-input) 13)
cf,ct,cv,cz,fi,lq,my,pa,sl,tt,vw,wz,yd
