(defpackage :aoc/2023/08 #.cl-user::*aoc-use*)
(in-package :aoc/2023/08)

(defun parse-input (&optional (strings (uiop:read-file-lines #P"src/2023/day08.txt")))
  (destructuring-bind (instructions ignore . nodes) strings
    (flet ((node (s)
             (mapcar #'as-keyword (cl-ppcre:all-matches-as-strings "\\w+" s))))
      (cons (coerce instructions 'list)
            (mapcar #'node nodes)))))

; (loop with (inst . nodes) = (parse-input)
;       for step from 0
;       for node = (assoc :aaa nodes) then (assoc next nodes)
;       for in in (ncycle inst) for next = (if (eq in #\L) (second node) (third node))
;       when (eq (car node) :zzz) return step)
; => 21409

(defun node-name-ends-with-char? (ch node)
  (char= (char (reverse (mkstr (first node))) 0) ch))
(node-name-ends-with-char? #\A '(:aaz))

(defun find-part2 (start &optional (strings (uiop:read-file-lines #P"src/2023/day08.txt")))
  (loop with (inst . nodes) = (parse-input)
        for step from 0
        for node = (assoc start nodes) then (assoc next nodes)
        for in in (ncycle inst) for next = (if (eq in #\L) (second node) (third node))
        when (node-name-ends-with-char? #\Z node) return step
        never (not node)))

; (apply
;   #'lcm
;   (looping
;     (dolist (start (remove-if-not [node-name-ends-with-char? #\A _] (cdr (parse-input))))
;       (collect! (find-part2 (car start))))))
; => 21165830176709
