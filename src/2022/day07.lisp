(defpackage :aoc/2022/07 #.cl-user::*aoc-use*)
(in-package :aoc/2022/07)


(defun cd? (s)
  (cl-ppcre:register-groups-bind (dir)
      ("\\$ cd (.+)" s)
    dir))
(defun cd-in? (s)
  (aif (cd? s)
    (unless (string= it "..")
      it)))
(defun cd-out? (s)
  (aif (cd? s)
    (not (cd-in? s))))
(defun ls? (s) (search "$ ls" s))
(defun dir? (s)
  (cl-ppcre:register-groups-bind (dir)
      ("dir (.+)" s)
    dir))
(defun file? (s)
  (cl-ppcre:register-groups-bind ((#'parse-integer size) file)
      ("(\\d+) (.+)" s)
    (cons file size)))


(defun make-fs (&optional (session (uiop:read-file-lines #P"src/2022/day07.txt")))
  (let ((fs (make-hash-table :test 'equal)))
    (loop with cwd = nil for cmd in session do
          (cond ((cd-in? cmd) (push (cd? cmd) cwd))
                ((cd-out? cmd) (pop cwd))
                ((ls? cmd) nil)
                ((or (file? cmd) (dir? cmd)) (push cmd (gethash cwd fs nil)))
                (t (error "Unexpected"))))
    fs))


(defun all-directories (&optional (fs (make-fs (uiop:read-file-lines #P"src/2022/day07.txt"))))
  (let ((ret (make-hash-table :test 'equal)))
    (labels ((recur (cwd)
               (setf (gethash cwd ret)
                     (loop for cmd in (gethash cwd fs) sum
                           (cond ((file? cmd) (cdr (file? cmd)))
                                 ((dir? cmd) (recur (cons (dir? cmd) cwd)))
                                 (t (error "WTF?")))))))
      (recur (list "/"))
      ret)))


(define-solution (2022 07) (fs make-fs)
  (let* ((all-dirs (all-directories fs))
         (used (gethash (list "/") all-dirs)))
    (values (loop for size being the hash-values of all-dirs
                  when (<= size 100000) sum size)
            (loop for size being the hash-values of all-dirs
                  when (>= size (- 30000000 (- 70000000 used))) minimize size))))

(define-test (2022 07) (1243729 4443914))

;;; Version 1:
;;; - Add ALL-DIRECTORIES to convert the filesystem into a HASH-TABLE mapping
;;;   from the directory name to its size - With the above done, solving both
;;;   part1 and part2 becomes pretty trivial
;;; - [link](https://topaz.github.io/paste/#XQAAAQALBwAAAAAAAAAUGQimgx+p6NuJqP4NbT429lNlQmwQ+NA7Z6Sb67RJftrxz699FNGmyRMxsY+y+KowOaZntGBMDmV7jXYuq9CFQATlECid81i0LB+D2oLfJEY/GEOlw5TfbAzh0VeJlDtHRvQD8qbCLHdzAqgpv237wtQkjwVwntRlieQZb3oGQC83AwgJ91KfXbMTjfvuIKOHrb9Cf8Q/jXaLq8s9r8oFhArM5egaLsM8+CRniqdJJfyrQUlKbc9eAmcVB477aa2Z5Qs+QTctqvpZrYYx4UEs46QnWgK0TFAxBW5iTNlWjflG52NLlMF9e79BhGVJToLjpp7GvAdsRno7TMBxHQpS+SvU9WXmFMuzm43/SefEoBWBXc1butqEIdI+2yj1U66aJ3HxJzXibMRpJpp10mnPffXxt+6IIfVoZ9gJX7KvUQHnbHm0I4PFKEGufibUI3JdAfA4JrqQBeV+rOIocM4kq5ohiyh4XtFDgaThg+fDUzXyjOFb7duNuOQRwRFAnh2ddDg2F1rmMMO5WfGAfggeKp5HTbgZtVKoIOyuN0NRWKmO7hQSv5eYus/iqs2F2TYEsuyQU/g7HbbBVVzg9NMBD7nAs+1EPAww+qEIjr8K2WgtzFLpl/E3fHMZNlofdIgAtE01G0ZDxN2Du0dX4oU++OTj/Lcf8SVdIWySIShE6AHO4vvy0aVXEhjmkGiKEafVSyijGCw+qWWKemb9X8lHft/OlGgzLQAxvCGJkBeJHMcmVDbgsc42jOKjz8uIcJFJsgOKtB9r9CGRdimrA6YY7P2B+RzZfV/C+GtRIhsvJK7O9mJSdIkCI9FiOuS2obDEiQi7jsTh2VX2tWV8YnKgWr5x4zBQUuy/iLcTMKvvz2hmadr/26HXMA==)
;;; Version 0:
;;; - Extensive use of CL-PPCRE:REGISTER-GROUPS-BIND to parse the commands
;;; - Parse the input into a HASH-TABLE mapping from paths to their content
;;; - [link](https://topaz.github.io/paste/#XQAAAQCqCgAAAAAAAAAUGQimgx+p6NuJqP4NbT429lNlQmwQ+NA7Z6Sb67RJftrxz699FNGmyRMxsY+y+KowOaZntGBMDmV7jXYuq9CFQATlECid81i0LB+D2oLfJEY/GEOlw5TfbAzh0VeJlDtHRvQD8qbCLHdzAqgpv237wtQkjwVwntRlieQZb3oGQC83AwgJ91KfXbMTjfvuIKOHrb9Cf8Q/jXaLq8s9r8oFhArM5egaLsM8+CRniqdJJfyrQUlKbc9eAmcVB477aa2Z5Qs+QTctqvpZrYYx4UEs46Qnn1SeycgOyDXVwi8Ej6IrZZbHM3tuvtYCMvQcxn4HmxHdXOlhVhbQEH5FpCZwZKFb734iddV2ZGG7b2N3dkbVhPl0W/a2gjbc2By+lhltWBWzuYNVSlwKDmkYMazRzsP9/HNorqDwZA3cIUWRoojVTqybCXRIAX0BO+EZd7OznMFO0fIDuJdzSsKEwVFBBRjRwTaSxTXaQeTYbAGNF56LlOp8ogZHrUu1ikLoSqgI/iBTMnygGiW9lpqJUK9ymDbpWKiXSgMwPN4iUmbFm4W61TAvaXscFevXDu78ls9Ci/YqbXW58PaBPWbJiGoJHxoQuqDGE456xIw6S3ADh4J3CXfJwD2xOzJcXLAY8c2T8GZoqJd8RVbXqQEqe8iI0y5aQZ9+nxZn7h+WF4beJ2XkVdfLdNKC3O6o3rVY3VXfgTsdUbQzRpBCUcVwEzIOBS586ZTGv4F8qovY41k2rkSNn39NP2q+i6yGsYDQY0fcmhYH7GohOQONC9pjnCx2yK5GTbx/Qa0r7sUMja1gvhtxKuuJxjgQeeywg3nRPeI01cUOH/Gn+Zt/KCVx2+Hal47K0a16EZ+Rajh3AUKr5UxCoI09MptYHYa24GmEx1BZCndigJ1wuSHbrxDIWyrSqJV0Kv5PSf9heLg7Ca4gMGJxs77UU3clqQEkWpR3ifpQ30vDyv8IkXopaoHglL4fHnihlDnIMvZ55iUwClkBLKuBBYBwr2+VBh0c3miIcIwsIbm6TTyoIBuMeo7inHZxzvSsXTY//rBmOA==)
