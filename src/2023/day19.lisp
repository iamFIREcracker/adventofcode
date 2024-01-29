(defpackage :aoc/2023/19 #.cl-user::*aoc-use*)
(in-package :aoc/2023/19)


;;; Input parsing ------------------------------------------------------------

(defun rule (s)
  (or (cl-ppcre:register-groups-bind ((#'as-keyword q)
                                      (#'symb p)
                                      (#'parse-integer o)
                                      (#'as-keyword n))
          ("(\\w+)(<|>)(\\d+):(\\w+)" s)
        (list q p o n))
      (as-keyword s)))

(defun rules (s)
  (looping
    (dolist (r (split-sequence:split-sequence #\, s))
      (collect! (rule r)))))

(defun workflow (s)
  (cl-ppcre:register-groups-bind ((#'as-keyword name) (#'rules rules))
      ("(\\w+)\\{(.*)\\}" s)
    (list name rules)))

(defun category (s)
  (cl-ppcre:register-groups-bind ((#'as-keyword q) (#'parse-integer o))
      ("(\\w+)=(\\d+)" s)
    (list q o)))

(defun part (s)
  (setf s (subseq- s 1 -1))
  (looping
    (dolist (p (split-sequence:split-sequence #\, s))
      (append! (category p)))))

(defun parse-input (&optional (strings (aoc::read-problem-input 2023 19)))
  (destructuring-bind (workflows parts)
      (split-sequence:split-sequence "" strings :test #'string=)
    (list (mapcar #'workflow workflows)
          (mapcar #'part parts))))


;;; Actual solution ----------------------------------------------------------

(defun accepted-parts (&optional (input (parse-input)))
  (destructuring-bind (workflows parts) input
    (looping
      (dolist (part parts)
        (when (accepted? :in part workflows)
          (collect! (reduce #'+ (keep-if #'numberp part))))))))

(defun accepted? (state part workflows)
  (cond ((eq state :A) t)
        ((eq state :R) nil)
        (t (destructuring-bind (name rules) (assoc state workflows)
             (declare (ignore name))
             (dolist (r rules)
               (if (atom r)
                 (return (accepted? r part workflows))
                 (destructuring-bind (c op num next) r
                   (if (funcall op (getf part c) num)
                     (return (accepted? next part workflows))))))))))


(defun acceptance-criteria (workflows)
  (looping
    (labels ((append-condition (criteria c op num)
               (setf criteria (copy-seq criteria))
               (push (list op num) (getf criteria c))
               criteria))
      (recursively ((state :in) (criteria nil))
        (cond ((eq state :A) (collect! criteria))
              ((eq state :R) nil)
              (t (destructuring-bind (name rules) (assoc state workflows)
                   (declare (ignore name))
                   (dolist (r rules)
                     (if (atom r)
                       (recur r criteria)
                       (destructuring-bind (c op num next) r
                         (recur next (append-condition criteria c op num))
                         (setf criteria
                               (append-condition criteria
                                                 c
                                                 (if (eq op '<) '> '<)
                                                 (if (eq op '<) (1- num) (1+ num))
                                                 ))))))))))))

(defun count-distinct-values (criteria)
  (looping
    (doseq (c '(:x :m :a :s))
      (multiply!
        (looping
          (dorangei (n 1 4000)
            (count! (loop for (op val) in (getf criteria c)
                          always (funcall op n val)))))))))


(define-solution (2023 19) (input parse-input)
  (destructuring-bind (workflows _) input
    (declare (ignore _))
    (values (reduce #'+ (accepted-parts input))
            (reduce #'+ (acceptance-criteria workflows)
                    :key #'count-distinct-values))))

(define-test (2023 19) (398527 133973513090020))
