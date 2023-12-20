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

(defun parse-input (&optional (strings (uiop:read-file-lines #P"src/2023/day19.txt")))
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
          (collect! part))))))

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


(defun score (part) (reduce #'+ (keep-if #'numberp part)))


(define-solution (2023 19) (input parse-input)
  (values (reduce #'+ (accepted-parts input) :key #'score)))
(solution-run)

(define-test (2023 19) (398527))

(defun valid-ratings-conditions (workflows)
  (looping
    (labels ((append-condition (parts part op num)
               (setf ratings (copy-seq ratings))
               (push (list op num) (getf ratings part))
               ratings)
             (recur (state ratings)
               (cond ((eq state :A) (collect! ratings))
                     ((eq state :R) nil)
                     (t (destructuring-bind (name rules) (assoc state workflows)
                          (declare (ignore name))
                          (dolist (r rules)
                            (if (atom r)
                              (recur r ratings)
                              (destructuring-bind (part op num next) r
                                (recur next (append-condition ratings part op num))
                                (setf ratings
                                      (append-condition ratings
                                                        part
                                                        (if (eq op '<) '> '<)
                                                        (if (eq op '<) (1- num) (1+ num))
                                                        ))))))))))
      (recur :in nil))))

(defun score2 (ratings)
  (reduce #'* (mapcar [count-distinct-values ratings _] (list :x :m :a :s))))

#+#:excluded (destructuring-bind (workflows ratings) (parse-input)
               (declare (ignore ratings))
               (reduce #'+ (mapcar #'score2 (valid-ratings-conditions workflows))))
; 10536956617799618 nope
; 133973513090020
