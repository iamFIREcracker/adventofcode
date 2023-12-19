(defpackage :aoc/2023/19 #.cl-user::*aoc-use*)
(in-package :aoc/2023/19)


(defun rule (s)
  (or (cl-ppcre:register-groups-bind ((#'as-keyword q)
                                      (#'symb p)
                                      (#'parse-integer o)
                                      (#'as-keyword n))
          ("(\\w+)(<|>)(\\d+):(\\w+)" s)
        (list q p o n))
      (as-keyword s)))
#+#:excluded (rule "a<2006:qkq")
#+#:excluded (rule "rfg")

(defun rules (s)
  (looping
    (dolist (r (split-sequence:split-sequence #\, s))
      (collect! (rule r)))))
#+#:excluded (rules "a<2006:qkq,m>2090:A,rfg")

(defun workflow (s)
  (cl-ppcre:register-groups-bind ((#'as-keyword name) (#'rules rules))
      ("(\\w+)\\{(.*)\\}" s)
    (list name rules)))
#+#:excluded (workflow "px{a<2006:qkq,m>2090:A,rfg}")

(defun rating (s)
  (cl-ppcre:register-groups-bind ((#'as-keyword q) (#'parse-integer o))
      ("(\\w+)=(\\d+)" s)
    (list q o)))
#+#:excluded (rating "x=787")

(defun ratings (s)
  (cl-ppcre:register-groups-bind (ratings)
      ("\\{(.*)\\}" s)
    (looping
      (dolist (p (split-sequence:split-sequence #\, ratings))
        (append! (rating p))))))
#+#:excluded (ratings "{x=787,m=2655,a=1222,s=2876}")

(defun parse-input (&optional (strings (uiop:read-file-lines #P"src/2023/day19.txt")))
  (destructuring-bind (workflows ratings)
      (split-sequence:split-sequence "" strings :test #'string=)
    (list (mapcar #'workflow workflows)
          (mapcar #'ratings ratings))))


(defun accepted? (state ratings workflows)
  (cond ((eq state :A) t)
        ((eq state :R) nil)
        (t (destructuring-bind (name rules) (assoc state workflows)
             (declare (ignore name))
             (dolist (r rules)
               (if (atom r)
                 (return (accepted? r ratings workflows))
                 (destructuring-bind (part op num next) r
                   (if (funcall op (getf ratings part) num)
                     (return (accepted? next ratings workflows))))))))))

(defun score (ratings) (reduce #'+ (keep-if #'numberp ratings)))

#+#:excluded (destructuring-bind (workflows ratings) (parse-input)
               (looping
                 (dolist (state ratings)
                   (when (accepted? :in state workflows)
                     (sum! (score state))))))
; 398527

(defun valid-ratings-conditions (workflows)
  (looping
    (labels ((append-condition (ratings part op num)
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
